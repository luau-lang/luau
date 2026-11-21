# `golden.py`: End-to-End Golden Tests for Luau

Luau includes a custom testing harness called `golden.py` to support an end-to-end test suite for
both `luau` and `luau-analyze` that matches the output of these programs in various configurations
against recorded output files (called "golden files" or sometimes "snapshot tests"). This test suite
is located under `tests/golden`, and every `.luau` file under it is automatically discovered as a
test. Philosophically, our golden testing framework aims to be pluralistic and unopinionated about
how you author tests because we aim to make it as easy as possible to author the tests you have in
mind.

## Quick Start

To get started, create a Luau file under `tests/golden`. Its name will be used as the name of the
test. In fact, every `.luau` file within the directory is a test; we can add a directive
or a snapshot file to record its expected outcome:

```luau
--!golden ok

local answer: number = 42
assert(answer == 42)
```


This single directive specifies that we expect the test to correctly execute under both `luau` and
`luau-analyze`. We can confirm this holds by running the following from the Luau source root:

```sh
python3 -m tools.golden

# or alternatively, if you're a nerd...
uv run tools/golden.py
```

When invoked, the runner will select resolved copies of `luau` and `luau-analyze`, print their paths
out (to help catch any issues where the automation might have picked an unexpected copy of the
executable), and then execute each test using these executables with both all flags off and all
flags on. If you'd like to be more explicit, you can provide direct paths for either or both
executables and circumvent the automated selection entirely:

```sh
python3 -m tools.golden --luau path/to/luau --luau-analyze path/to/luau-analyze

# or alternatively...
uv run tools/golden.py --luau path/to/luau --luau-analyze path/to/luau-analyze
```

You can also forcibly enable fast flags across the whole suite using the `--fflags` option. In this
case, the harness will forward any flag configurations to both executables:

```sh
python3 -m tools.golden \
  --fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

# or alternatively...
uv run tools/golden.py
  --fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
```

`--fflags` can be repeated arbitrarily, and each value is forwarded to all the executables in
the order supplied. Any fast flag definitions per-test will be set _after_ these global ones.

When you're adding a new test, if you'd like to define exact snapshots of its outputs as a baseline,
rather than use the directives, you can run the harness an update mode:

```sh
python3 -m tools.golden --update all --config all example
# or alternatively...
uv run tools/golden.py --update all --config all example
```

These commands will set up the expected outputs for analysis in strict mode, analysis in non-strict
mode, and normal runtime execution in both the flags on and flags off configuration. If you'd like
to limit expectations to e.g. only flags on, you can instead use `--config flags-on`. Alternatively,
if you'd like your test to only specify behavior for analysis, you can say
`--update strict,nonstrict`. In general, both commands expect a comma-separated listed of options
whose variants can be found in the output of `--help`, e.g.

```sh
python3 -m tools.golden --help
# or
uv run tools/golden.py --help
```

## Test layouts and test selectors

As mentioned in the Quick Start section, each `.luau` file is automatically discovered as its own 
single-file test:

```text
tests/golden/
└── types/
    └── generic.luau       # referenced as: types/generic
```


The expection to this is that folders with `init.luau` files instead define a single multifile test
with `init.luau` as its entrypoint for both executables. Everything nested anywhere beneath it ---
including any files in subfolders --- are part of that single test. They can be required as normal
from the entrypoint, but will never be invoked on their own by the runner (even if they contain
golden directives).

```text
tests/golden/
└── packages/
    └── cycle/             # referenced as: packages/cycle
        ├── init.luau      # entry point with any directives or expected output
        ├── first.luau     # separate file as part of packages/cycle
        └── nested/
            └── second.luau  # separate file as part of packages/cycle
```

Since it would turn the entire test suite into one test, `init.luau` is disallowed at the root
directory `tests/golden`. Multifile tests must always be under at least one subdirectory to group
them under a single usable test identifier. In the case of any nesting, the outermost `init.luau` is
what defines the test. Any further nested `init.luau` files are submodules of the enclosing test,
rather than the root of a new test. 

The test harness supports executing both subsections of the test suite and individual tests by
reference. Selectors can either be absolute paths into the test suite, relative paths from the
current working directory, or relative paths from the root of the test suite, `tests/golden`.
Selectors can include or omit the `.luau` file extension as desired. If you select a file nested
under a multifile test, the whole test it is a part of will be run, and any directories will run all
tests discoverable within them:

```sh
python3 -m tools.golden types/generic packages/cycle
python3 -m tools.golden tests/golden/types/generic.luau
python3 -m tools.golden analysis/tables
python3 -m tools.golden analysis
```

Selection does not support globs, though your shell environment may support globs for you. Any
overlapping or repeated selections will not cause the test to run more than once. Erroneous
selectors that match neither a test nor a directory containing tests, or any paths outside
`tests/golden` will result in an error.

## Golden Directives in Tests

Tests can optionally include directives using `--!golden` as the first nonblank lines in the test's
entry point (an entry point is a single-file test or an `init.luau`). These directives can specify
expectations for the test in question. The simplest directive we've already seen in the Quick Start
section, and specifies the expected status for all configurations:

```luau
--!golden ok
```

This directive defines the expected status for all executables to be success. If we instead wish to
expect either executable to fail, we can instead set the status to `fail`:

```luau
--!golden fail
```

We can also split the status of the test across configurations, e.g.

```luau
--!golden flags-on.status: ok
--!golden flags-off.status: fail
```

This combination of directives specifies that the test will execute successfully with all flags on,
and fail with all flags off. We can even combine these into a single directive, since the syntax for
directives takes a comma-separated list of `key: value` assignments:

```luau
--!golden flags-on.status: ok, flags-off.status: fail
```

Like the two directives before it, this defines that the test will succeed with all flags on, and
fail with all flags off. We can also author output directives that specify a regular expression to
be matched against the output of the executable in that configuration/command:

```luau
--!golden status: fail
--!golden strict.output: /type.*mismatch/
--!golden runtime.output: /https:\/\/example\.test\/result/
```

This example would look for "type" followed eventually by "mismatch" in the strict mode analysis
output, and "https://example.test/result" in the runtime output. Since some tests might depend on
specific flags being enabled, we also support a directive for enabling flags scoped to a particular
test:

```luau
--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
```

In this case, the flags `DebugLuauUserDefinedClasses` and `DebugLuauUserDefinedClassesRuntime` will
both be enabled for the test's execution. Since the syntax of fflag definitions includes its own
comma-separation, scoped fast flags must be defined with their own directive, and cannot be combined
like we did earlier. Only one `fflags=` directive is allowed per test.

## The Syntax of Directives

The full grammar for directives takes a comma-separated list of `key: value` assignments, except
in the case of declaring scoped fast flags:

```text
--!golden status: ok, flags-off.status: fail
--!golden strict.output: /common output/
--!golden flags-on.nonstrict.output: /new diagnostic/
--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
```

Supported keys consist of:

- `status` and `CONFIG.status`
- `[CONFIG.]COMMAND.output`

The options for configurations are `flags-on` and `flags-off` which represent running the
executables with all flags on and all flags off respectively. Commands correspond to the executables
(and configurations) that can be run: `strict`, `nonstrict`, and `runtime`. These represent running
`luau-analyze` in strict and non-strict mode respectively (both with the New Type Solver) and
running `luau` itself.

The values for status can be either `ok` or `fail`. A configuration-specific status takes precedence
over a global status directive. Each configuration must either define its expected status via a
directive, or have at least one exact output in the test's snapshot file. The test runner will
error if neither are provided for a test.

Status `ok` indicates that all three commands should exit successfully with exit code 0. Status
`fail` requires at least one command to exit 1; the others may exit 0 or 1. Only exit 1 satisfies
`fail`; a signal, timeout, or any other abnormal exit is a harness failure, and always fails the
test regardless of its expected status. When status is omitted for a configuration, command exit
codes are not checked for that configuration.

Values for output directives are Python regular expressions delimited by `/`. Commas inside
the delimiters are part of the regex, and do not split assignments. `\/` escapes a literal slash.
Python inline flags are supported, and every applicable repeated pattern must match using
`re.search`:

```luau
--!golden status: fail
--!golden strict.output: /type.*mismatch/
--!golden nonstrict.output: /(?im)^.*expected number.*$/
--!golden runtime.output: /https:\/\/example\.test\/result/
```

Malformed directives, invalid regexes, unknown keys/configurations, duplicate statuses at the same
scope, and configurations with neither status nor exact output fail discovery before any test
process starts. A line beginning with `--!golden` must use that exact marker followed by whitespace;
misspellings such as `--!golden:` or `--!golden-status` are rejected, rather than ignored.

## Test Configurations

Each selected test is run independently under this fixed matrix:

| Configuration | `strict`                                                       | `nonstrict`                                                       | `runtime`                   |
| ------------- | -------------------------------------------------------------- | ----------------------------------------------------------------- | --------------------------- |
| `flags-on`    | `luau-analyze --mode=strict --solver=new --fflags=true ENTRY`  | `luau-analyze --mode=nonstrict --solver=new --fflags=true ENTRY`  | `luau --fflags=true ENTRY`  |
| `flags-off`   | `luau-analyze --mode=strict --solver=new --fflags=false ENTRY` | `luau-analyze --mode=nonstrict --solver=new --fflags=false ENTRY` | `luau --fflags=false ENTRY` |

`--fflags=true` and `--fflags=false` affect both `luau` and `luau-analyze` executables.
CLI-provided `--fflags` values are applied after the base value from the given configuration. Then,
any flags defined by a test's `--!golden fflags=...` directive will be applied last. To limit us to
a reasonable number of configurations, the harness is currently configured to only use the New Type
Solver. To add a new command or flag configuration, add a `Command` or `Configuration` entry in
`tools/golden/models.py`. These will then automatically be supported as valid syntax in both golden
directives and CLI arguments. This documentation will require a manual update however.

## Exact output in Golden Files

Each test's exact expected output lives in a single TOML snapshot file adjacent to its entry point,
named after the entry point with a `.golden.toml` extension. For a multifile test, the prefix is
`init`, so expectations sit beside `init.luau` as `init.golden.toml`:

```text
example.luau
example.golden.toml
packages/cycle/init.luau
packages/cycle/init.golden.toml
```

The snapshot has one table per configuration, and one string key per command within it. For
example, `tests/golden/meta/exact_output.golden.toml` expects empty analysis output and two
printed lines at runtime in both configurations:

```toml
[flags-on]
strict = ""
nonstrict = ""
runtime = '''
exact output
42
'''

[flags-off]
strict = ""
nonstrict = ""
runtime = '''
exact output
42
'''
```

A present key must match the output exactly byte-for-byte, while a missing key (or a missing
snapshot file) means that command's output is ignored entirely. An empty string requires that the
output be empty. The harness writes multiline literal strings (`'''`) wherever it can, so the
snapshot reads as the raw captured output; the opening newline is not part of the value. Output that
can't be expressed that way (containing `'''` or control characters) is written as an escaped basic
string instead. All snapshot files must be UTF-8, and reading them requires Python 3.11 or newer
(for `tomllib`).

An exact output also makes the use of status directives optional for that test in that
configuration. This is because the exact comparison of the output already implies the full behavior
of the test.

The `.golden.toml` file extension is reserved for snapshots throughout the golden test suite. Every
such file must refer to a real test entry point, and may only use the configurations and commands
of the test matrix as tables and keys. Any misspelled configurations, commands, non-string values,
invalid TOML, and orphan files cause the test execution to fail, allowing us to catch situations
where the programmer might believe a test to be working when it isn't. Stray `.output` files are
rejected as well, since they would otherwise be silently ignored.

Currently, the harness will signal an error if a test attempts to mix both an exact output and
regular expression directives for the same combination of configuration and command. This is because
we think they represent distinct approaches to test output specification, and combining them would
likely be redundant.

Both CRLF and CR line endings are normalized to LF in both the captured and exact output before
comparison. Exact mismatches are reported as unified git-style diffs labeled with the snapshot path
and key (e.g. `example.golden.toml [flags-on.runtime]`), while regex mismatches include the captured
output. You can run the test runner with `--dump` to write the _actual_ (rather than expected)
outputs to an adjacent `.tmp` snapshot as well, if you wish to have further basis for manual
inspection and comparison.

## Comparing and updating test outputs

By default, the golden test runner operates in validation mode, comparing tests against their
captured specifications for every configuration:

```sh
python3 -m tools.golden
python3 -m tools.golden types/generic packages/cycle
```

We can instead run in an _update_ mode to write out new exact outputs for both specific tests and for
the entire suite, by using the `--update` and `--update-all` commands. With `--update`, we can
perform a targeted update for (at least one) test provided as an argument to the call. In doing so,
we can specify both the command (between `runtime`, `strict`, and `nonstrict`) and the configuration
(between `flags-on` and `flags-off`). This can be used either to define initial expectations for a
new test, or to update the exact outputs for a particular configuration (typically flags on)
after implementing a bug fix. Here are some examples:

```sh
python3 -m tools.golden --update=strict --config=flags-on types/generic
python3 -m tools.golden --update=nonstrict,strict \
  --config=flags-on,flags-off types/generic
python3 -m tools.golden --update=nonstrict --config=all packages/cycle
python3 -m tools.golden --update=all --config=all runtime/assertions
```

With `--update-all`, we can automatically update exact outputs of the specified configurations and
commands for the entire test suite at once. This will never add new keys to any snapshot (or create
any new snapshot file), it will only update existing ones. This pairs especially well with a
`git diff` afterwards to see how all the files have changed in a compact interface. Example
invocations with `--update-all` include:

```sh
python3 -m tools.golden --update-all=strict,runtime --config=flags-off
python3 -m tools.golden --update-all=strict --update-all=runtime \
  --config=flags-on --config=flags-off
python3 -m tools.golden --update-all=all --config=all
```

Each of `--update`, `--update-all`, and `--config` accept one matrix value, a
comma-separated list, repeated options, or `all`. `all` represents all possible values for command
and configuration based on where it's used, and cannot be combined with named values. 
`--update` and `--update-all` are mutually exclusive, so you cannot use both within one invocation
of the test runner.

Updates rewrite the whole snapshot file in a canonical order (configurations, then commands, in
matrix order), preserving every existing key that wasn't selected for the update. A configuration
that hits a harness failure keeps its previous expectations.

## Inspecting captured output more carefully

If just writing the files and using `git diff` directly is not sufficient, you can use `--dump` to
write the _actual_ output of each test next to its snapshot in a `.tmp` file. These files will
automatically be ignored by git, and can be used with arbitrary tooling to compare any mismatches
between the actual and expected outputs:

```sh
python3 -m tools.golden --dump types/generic
diff tests/golden/types/generic.golden.toml tests/golden/types/generic.golden.toml.tmp
```

In particular, `--dump` writes `<entry>.golden.toml.tmp` beside the entry point, containing every
command in every selected configuration in the same format as the snapshot. It uses the expected
`.golden.toml` name plus a `.tmp` suffix, so the pair sorts together:

```text
example.golden.toml       # expected (committed)
example.golden.toml.tmp   # last captured run (gitignored)
```

`--dump` is independent of comparison and update modes, as it only records output and never affects
the actual test status. Commands that hit a harness failure (timeout, spawn error, and so on)
captured no usable output and are omitted. The `.tmp` files are intended to be thrown away: they are
blindly overwritten each run with `--dump`, ignored by discovery, and defined to be ignored by git.

If you wish to clean up these temporary files, `--clean` will do just that. It deletes every
`*.golden.toml.tmp` file under the test root and exits without running any tests whatsoever, so it
needs neither the Luau executables nor a valid corpus:

```sh
python3 -m tools.golden --clean
```

`--clean` removes only the throwaway `.tmp` files, and committed `.golden.toml` snapshots are left
untouched. `--clean` takes no test selectors and cannot be combined with `--dump`, `--update`, or
`--update-all`.

## Automatic executable discovery

While all the build system-integrated targets for running the golden test framework specify exact
executable paths, when the runner is invokved manually, it will attempt to automatically discover
the locations of the built artifacts for both `luau` and `luau-execute`. If you'd like to skip this,
you can either specify environment variables for `LUAU` and `LUAU_ANALYZE`, or you can pass the
paths to each executable to the `--luau` and `--luau-analyze` options:

```sh
python3 -m tools.golden --luau ./build/debug/luau \
  --luau-analyze ./build/debug/luau-analyze

LUAU=./build/debug/luau \
LUAU_ANALYZE=./build/debug/luau-analyze \
python3 -m tools.golden
```

Automated executable discovery is resolved according to the following priority:

1. beside an explicitly resolved counterpart
2. a colocated pair of executables in the current working directory
3. a colocated pair of executables in the Luau source root
4. a colocated pair of executables in exactly one of the common Make/CMake build directories
5. the executables available on the system `PATH`

Both plain executable names and `.exe` extensions are recognized as long as the files in question
are marked as executable. The selection will always prefer colocated pairs to separate ones, and
will reject situations in which the desired executables are ambiguous. The process will also never
invoke the build system on its own, and those who desire the build to be tied into the process
should invoke the corresponding target in the build system of their choice, e.g. `Luau.GoldenTest`
in CMake and Buck2, and `make test` in the Makefile.

## Process and data rules

Invoked commands use argument arrays without a shell, execute with the testing root as their working
directory, and receive forward-slash relative entry paths. The default timeout is 10 seconds and can
be changed with the option `--timeout SECONDS`. The root directory for the test suite can be
explicitly specified using `--test-root PATH`, but defaults to `tests/golden`. Each configuration is
executed concurrently. `--jobs N` sets the amount of concurrent jobs allowed manually, where `0`
(the default) auto-sizes to the local machine and `1` forces serial execution.

Exit 0 and 1 are treated as success and failure outcomes for each test. Spawn failures, timeouts,
signals, all other exit codes, and invalid UTF-8 output are harness failures that are raised
separately and do not match against a `fail` status. Output from all commands is decoded strictly as
UTF-8. Each command's stderr is redirected to stdout before launch, preserving their combined pipe
order, but reducing the number of required outputs for each test.
