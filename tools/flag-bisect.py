#!/usr/bin/python3
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

import argparse
import asyncio
import copy
import json
import math
import os
import platform
import re
import subprocess
import sys
import textwrap
from enum import Enum

def add_parser(subparsers):
    flag_bisect_command = subparsers.add_parser('flag-bisect',
        help=help(),
        description=help(),
        epilog=epilog(),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        )

    add_argument_parsers(flag_bisect_command)
    flag_bisect_command.set_defaults(func=flag_bisect_main)
    return flag_bisect_command

def help():
    return 'Search for a set of flags triggering the faulty behavior in unit tests'

def get_terminal_width():
    try:
        return os.get_terminal_size().columns
    except:
        # Return a reasonable default when a terminal is not available
        return 80
def wrap_text(text, width):
    leading_whitespace_re = re.compile('( *)')

    def get_paragraphs_and_indent(string):
        lines = string.split('\n')
        result = ''
        line_count = 0
        initial_indent = ''
        subsequent_indent = ''
        for line in lines:
            if len(line.strip()) == 0:
                if line_count > 0:
                    yield result, initial_indent, subsequent_indent
                    result = ''
                    line_count = 0
            else:
                line_count += 1
                if line_count == 1:
                    initial_indent = leading_whitespace_re.match(line).group(1)
                    subsequent_indent = initial_indent
                elif line_count == 2:
                    subsequent_indent = leading_whitespace_re.match(line).group(1)
                result += line.strip() + '\n'

    result = ''
    for paragraph, initial_indent, subsequent_indent in get_paragraphs_and_indent(text):
        result += textwrap.fill(paragraph, width=width, initial_indent=initial_indent, subsequent_indent=subsequent_indent, break_on_hyphens=False) + '\n\n'
    return result

def wrap_text_for_terminal(text):
    right_margin = 2 # This margin matches what argparse uses when formatting argument documentation
    min_width = 20
    width = max(min_width, get_terminal_width() - right_margin)
    return wrap_text(text, width)

def epilog():
    return wrap_text_for_terminal('''
    This tool uses the delta debugging algorithm to minimize the set of flags to the ones that are faulty in your unit tests,
    and the usage is trivial. Just provide a path to the unit test and you're done, the tool will do the rest.

    There are many use cases with flag-bisect. Included but not limited to:

      1: If your test is failing when you omit `--fflags=true` but it works when passing `--fflags=true`, then you can
         use this tool to find that set of flag requirements to see which flags are missing that will help to fix it. Ditto
         for the opposite too, this tool is generalized for that case.

      2: If you happen to run into a problem on production, and you're not sure which flags is the problem and you can easily
         create a unit test, you can run flag-bisect on that unit test to rapidly find the set of flags.

      3: If you have a flag that causes a performance regression, there's also the `--timeout=N` where `N` is in seconds.

      4: If you have tests that are demonstrating flakiness behavior, you can also use `--tries=N` where `N` is the number of
         attempts to run the same set of flags before moving on to the new set. This will eventually drill down to the flaky flag(s).
         Generally 8 tries should be more than enough, but it depends on the rarity. The more rare it is, the higher the attempts count
         needs to be. Note that this comes with a performance cost the higher you go, but certainly still faster than manual search.
         This argument will disable parallel mode by default. If this is not desired, explicitly write `--parallel=on`.

      5: By default flag-bisect runs in parallel mode which uses a slightly modified version of delta debugging algorithm to support
         trying multiple sets of flags concurrently. This means that the number of sets the algorithm will try at once is equal to the
         number of concurrent jobs. There is currently no upper bound to that, so heed this warning that your machine may slow down
         significantly. In this mode, we display the number of jobs it is running in parallel. Use `--parallel=off` to disable parallel
         mode.

         Be aware that this introduces some level of *non-determinism*, and it is fundamental due to the interaction with flag dependencies
         and the fact one job may finish faster than another job that got ran in the same cycle. However, it generally shouldn't matter
         if your test is deterministic and has no implicit flag dependencies in the codebase.

    The tool will try to automatically figure out which of `--pass` or `--fail` to use if you omit them or use `--auto` by applying
    heuristics. For example, if the tests works using `--fflags=true` and crashes if omitting `--fflags=true`, then it knows
    to use `--pass` to give you set of flags that will cause that crash. As usual, vice versa is also true. Since this is a
    heuristic, if it gets that guess wrong, you can override with `--pass` or `--fail`.

    You can speed this process up by scoping it to as few tests as possible, for example if you're using doctest then you'd
    pass `--tc=my_test` as an argument after `--`, so `flag-bisect ./path/to/binary -- --tc=my_test`.
    ''')

class InterestnessMode(Enum):
    AUTO = 0,
    FAIL = 1,
    PASS = 2,

def add_argument_parsers(parser):
    parser.add_argument('binary_path', help='Path to the unit test binary that will be bisected for a set of flags')

    parser.add_argument('--tries', dest='attempts', type=int, default=1, metavar='N',
        help='If the tests are flaky, flag-bisect will try again with the same set by N amount of times before moving on')

    parser.add_argument('--parallel', dest='parallel', choices=['on', 'off'], default='default',
        help='Test multiple sets of flags in parallel, useful when the test takes a while to run.')

    parser.add_argument('--explicit', dest='explicit', action='store_true', default=False, help='Explicitly set flags to false')

    parser.add_argument('--filter', dest='filter', default=None, help='Regular expression to filter for a subset of flags to test')

    parser.add_argument('--verbose', dest='verbose', action='store_true', default=False, help='Show stdout and stderr of the program being run')

    interestness_parser = parser.add_mutually_exclusive_group()
    interestness_parser.add_argument('--auto', dest='mode', action='store_const', const=InterestnessMode.AUTO,
        default=InterestnessMode.AUTO, help='Automatically figure out which one of --pass or --fail should be used')
    interestness_parser.add_argument('--fail', dest='mode', action='store_const', const=InterestnessMode.FAIL,
        help='You want this if passing --fflags=true causes tests to fail')
    interestness_parser.add_argument('--pass', dest='mode', action='store_const', const=InterestnessMode.PASS,
        help='You want this if passing --fflags=true causes tests to pass')
    interestness_parser.add_argument('--timeout', dest='timeout', type=int, default=0, metavar='SECONDS',
        help='Find the flag(s) causing performance regression if time to run exceeds the timeout in seconds')

class Options:
    def __init__(self, args, other_args, sense):
        self.path = args.binary_path
        self.explicit = args.explicit
        self.sense = sense
        self.timeout = args.timeout
        self.interested_in_timeouts = args.timeout != 0
        self.attempts = args.attempts
        self.parallel = (args.parallel == 'on' or args.parallel == 'default') if args.attempts == 1 else args.parallel == 'on'
        self.filter = re.compile(".*" + args.filter + ".*") if args.filter else None
        self.verbose = args.verbose
        self.other_args = [arg for arg in other_args if arg != '--'] # Useless to have -- here, discard.

    def copy_with_sense(self, sense):
        new_copy = copy.copy(self)
        new_copy.sense = sense
        return new_copy

class InterestnessResult(Enum):
    FAIL = 0,
    PASS = 1,
    TIMED_OUT = 2,

class Progress:
    def __init__(self, count, n_of_jobs=None):
        self.count = count
        self.steps = 0
        self.n_of_jobs = n_of_jobs
        self.buffer = None

    def show(self):
        # remaining is actually the height of the current search tree.
        remain = int(math.log2(self.count))
        flag_plural = 'flag' if self.count == 1 else 'flags'
        node_plural = 'node' if remain == 1 else 'nodes'
        jobs_info = f', running {self.n_of_jobs} jobs' if self.n_of_jobs is not None else ''
        return f'flag bisection: testing {self.count} {flag_plural} (step {self.steps}, {remain} {node_plural} remain{jobs_info})'

    def hide(self):
        if self.buffer:
            sys.stdout.write('\b \b' * len(self.buffer))

    def update(self, len, n_of_jobs=None):
        self.hide()
        self.count = len
        self.steps += 1
        self.n_of_jobs = n_of_jobs
        self.buffer = self.show()
        sys.stdout.write(self.buffer)
        sys.stdout.flush()

def list_fflags(options):
    try:
        out = subprocess.check_output([options.path, '--list-fflags'], encoding='UTF-8')
        flag_names = []

        # It's unlikely that a program we're going to test has no flags.
        # So if the output doesn't start with FFlag, assume it doesn't support --list-fflags and therefore cannot be bisected.
        if not out.startswith('FFlag') and not out.startswith('DFFlag') and not out.startswith('SFFlag'):
            return None

        flag_names = out.split('\n')[:-1]

        subset = [flag for flag in flag_names if options.filter.match(flag) is not None] if options.filter else flag_names
        return subset if subset else None
    except:
        return None

def mk_flags_argument(options, flags, initial_flags):
    lst = [flag + '=true' for flag in flags]

    # When --explicit is provided, we'd like to find the set of flags from initial_flags that's not in active flags.
    # This is so that we can provide a =false value instead of leaving them out to be the default value.
    if options.explicit:
        for flag in initial_flags:
            if flag not in flags:
                lst.append(flag + '=false')

    return '--fflags=' + ','.join(lst)

def mk_command_line(options, flags_argument):
    arguments = [options.path, *options.other_args]
    if flags_argument is not None:
        arguments.append(flags_argument)
    return arguments

async def get_interestness(options, flags_argument):
    try:
        timeout = options.timeout if options.interested_in_timeouts else None
        cmd = mk_command_line(options, flags_argument)
        stdout = subprocess.PIPE if not options.verbose else None
        stderr = subprocess.PIPE if not options.verbose else None
        process = subprocess.run(cmd, stdout=stdout, stderr=stderr, timeout=timeout)
        return InterestnessResult.PASS if process.returncode == 0 else InterestnessResult.FAIL
    except subprocess.TimeoutExpired:
        return InterestnessResult.TIMED_OUT

async def is_hot(options, flags_argument, pred=any):
    results = await asyncio.gather(*[get_interestness(options, flags_argument) for _ in range(options.attempts)])

    if options.interested_in_timeouts:
        return pred([InterestnessResult.TIMED_OUT == x for x in results])
    else:
        return pred([(InterestnessResult.PASS if options.sense else InterestnessResult.FAIL) == x for x in results])

def pairwise_disjoints(flags, granularity):
    offset = 0
    per_slice_len = len(flags) // granularity
    while offset < len(flags):
        yield flags[offset:offset + per_slice_len]
        offset += per_slice_len

def subsets_and_complements(flags, granularity):
    for disjoint_set in pairwise_disjoints(flags, granularity):
        yield disjoint_set, [flag for flag in flags if flag not in disjoint_set]

# https://www.cs.purdue.edu/homes/xyzhang/fall07/Papers/delta-debugging.pdf
async def ddmin(options, initial_flags):
    current = initial_flags
    granularity = 2

    progress = Progress(len(current))
    progress.update(len(current))

    while len(current) >= 2:
        changed = False

        for (subset, complement) in subsets_and_complements(current, granularity):
            progress.update(len(current))
            if await is_hot(options, mk_flags_argument(options, complement, initial_flags)):
                current = complement
                granularity = max(granularity - 1, 2)
                changed = True
                break
            elif await is_hot(options, mk_flags_argument(options, subset, initial_flags)):
                current = subset
                granularity = 2
                changed = True
                break

        if not changed:
            if granularity == len(current):
                break
            granularity = min(granularity * 2, len(current))

    progress.hide()
    return current

async def ddmin_parallel(options, initial_flags):
    current = initial_flags
    granularity = 2

    progress = Progress(len(current))
    progress.update(len(current), granularity)

    while len(current) >= 2:
        changed = False

        subset_jobs = []
        complement_jobs = []

        def advance(task):
            nonlocal current
            nonlocal granularity
            nonlocal changed
            # task.cancel() calls the callback passed to add_done_callback...
            if task.cancelled():
                return
            hot, new_delta, new_granularity = task.result()
            if hot and not changed:
                current = new_delta
                granularity = new_granularity
                changed = True
                for job in subset_jobs:
                    job.cancel()
                for job in complement_jobs:
                    job.cancel()

        for (subset, complement) in subsets_and_complements(current, granularity):
            async def work(flags, new_granularity):
                hot = await is_hot(options, mk_flags_argument(options, flags, initial_flags))
                return (hot, flags, new_granularity)

            # We want to run subset jobs in parallel first.
            subset_job = asyncio.create_task(work(subset, 2))
            subset_job.add_done_callback(advance)
            subset_jobs.append(subset_job)

            # Then the complements afterwards, but only if we didn't find a new subset.
            complement_job = asyncio.create_task(work(complement, max(granularity - 1, 2)))
            complement_job.add_done_callback(advance)
            complement_jobs.append(complement_job)

        # When we cancel jobs, the asyncio.gather will be waiting pointlessly.
        # In that case, we'd like to return the control to this routine.
        await asyncio.gather(*subset_jobs, return_exceptions=True)
        if not changed:
            await asyncio.gather(*complement_jobs, return_exceptions=True)
        progress.update(len(current), granularity)

        if not changed:
            if granularity == len(current):
                break
            granularity = min(granularity * 2, len(current))

    progress.hide()
    return current

def search(options, initial_flags):
    if options.parallel:
        return ddmin_parallel(options, initial_flags)
    else:
        return ddmin(options, initial_flags)

async def do_work(args, other_args):
    sense = None

    # If --timeout isn't used, try to apply a heuristic to figure out which of --pass or --fail we want.
    if args.timeout == 0 and args.mode == InterestnessMode.AUTO:
        inner_options = Options(args, other_args, sense)

        # We aren't interested in timeout for this heuristic. It just makes no sense to assume timeouts.
        # This actually cannot happen by this point, but if we make timeout a non-exclusive switch to --auto, this will go wrong.
        inner_options.timeout = 0
        inner_options.interested_in_timeouts = False

        all_tasks = asyncio.gather(
            is_hot(inner_options.copy_with_sense(True), '--fflags=true', all),
            is_hot(inner_options.copy_with_sense(False), '--fflags=false' if inner_options.explicit else None, all),
        )

        # If it times out, we can print a message saying that this is still working. We intentionally want to continue doing work.
        done, pending = await asyncio.wait([all_tasks], timeout=1.5)
        if all_tasks not in done:
            print('Hang on! I\'m running your program to try and figure out which of --pass or --fail to use!')
            print('Need to find out faster? Cancel the work and explicitly write --pass or --fail')

        is_pass_hot, is_fail_hot = await all_tasks

        # This is a bit counter-intuitive, but the following table tells us which of the sense we want.
        # Because when you omit --fflags=true argument and it fails, then is_fail_hot is True.
        # Consequently, you need to use --pass to find out what that set of flags is. And vice versa.
        #
        # Also, when is_pass_hot is True and is_fail_hot is False, then that program is working as expected.
        # There should be no reason to run flag bisection.
        # However, this can be ambiguous in the opposite of the aforementioned outcome!
        #
        # is_pass_hot | is_fail_hot | is ambiguous?
        #-------------|-------------|---------------
        # True        | True        | No! Pick --pass.
        # False       | False       | No! Pick --fail.
        # True        | False       | No! But this is the exact situation where you shouldn't need to flag-bisect. Raise an error.
        # False       | True        | Yes! But we'll pragmatically pick --fail here in the hope it gives the correct set of flags.

        if is_pass_hot and not is_fail_hot:
            print('The tests seems to be working fine for me. If you really need to flag-bisect, please try again with an explicit --pass or --fail', file=sys.stderr)
            return 1

        if not is_pass_hot and is_fail_hot:
            print('I couldn\'t quite figure out which of --pass or --fail to use, but I\'ll carry on anyway')

        sense = is_pass_hot
        argument = '--pass' if sense else '--fail'
        print(f'I\'m bisecting flags as if {argument} was used')
    else:
        sense = True if args.mode == InterestnessMode.PASS else False

    options = Options(args, other_args, sense)

    initial_flags = list_fflags(options)
    if initial_flags is None:
        print('I cannot bisect flags with ' + options.path, file=sys.stderr)
        print('These are required for me to be able to cooperate:', file=sys.stderr)
        print('\t--list-fflags must print a list of flags separated by newlines, including FFlag prefix', file=sys.stderr)
        print('\t--fflags=... to accept a comma-separated pair of flag names and their value in the form FFlagFoo=true', file=sys.stderr)
        return 1

    # On Windows, there is an upper bound on the numbers of characters for a command line incantation.
    # If we don't handle this ourselves, the runtime error is going to look nothing like the actual problem.
    # It'd say "file name way too long" or something to that effect. We can teed up a better error message and
    # tell the user how to work around it by using --filter.
    if platform.system() == 'Windows':
        cmd_line = ' '.join(mk_command_line(options, mk_flags_argument(options, initial_flags, [])))
        if len(cmd_line) >= 8191:
            print(f'Never mind! The command line is too long because we have {len(initial_flags)} flags to test', file=sys.stderr)
            print('Consider using `--filter=<regex>` to narrow it down upfront, or use any version of WSL instead', file=sys.stderr)
            return 1

    hot_flags = await search(options, initial_flags)
    if hot_flags:
        print('I narrowed down to these flags:')
        print(textwrap.indent('\n'.join(hot_flags), prefix='\t'))

        # If we showed the command line in explicit mode, all flags would be listed here.
        # This would pollute the terminal with 3000 flags. We don't want that. Don't show it.
        # Ditto for when the number flags we bisected are equal.
        if not options.explicit and len(hot_flags) != len(initial_flags):
            print('$ ' + ' '.join(mk_command_line(options, mk_flags_argument(options, hot_flags, initial_flags))))

        return 0

    print('I found nothing, sorry', file=sys.stderr)
    return 1

def flag_bisect_main(args, other_args):
    return asyncio.run(do_work(args, other_args))

def main():
    parser = argparse.ArgumentParser(description=help(), epilog=epilog(), formatter_class=argparse.RawTextHelpFormatter)
    add_argument_parsers(parser)
    args, other_args = parser.parse_known_args()
    return flag_bisect_main(args, other_args)

if __name__ == '__main__':
    sys.exit(main())
