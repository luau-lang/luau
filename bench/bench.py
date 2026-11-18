#!/usr/bin/env python3
# PYTHON_ARGCOMPLETE_OK
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
import argparse
import os
import subprocess
import math
import sys
import re
import json

import influxbench

# Taken from rotest
from color import colored, Color
from tabulate import TablePrinter, Alignment

try:
    import matplotlib
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    matplotlib = None

try:
    import scipy
    from scipy import stats
except ModuleNotFoundError:
    print("Warning: scipy package is not installed, confidence values will not be available")
    stats = None

scriptdir = os.path.dirname(os.path.realpath(__file__))
defaultVm = 'luau.exe' if os.name == "nt" else './luau'

METRIC_LABELS = {
    'time': ('Time', 'ms'),
    'cycles': ('Cycles', 'Mcyc'),
    'instructions': ('Instructions', 'Minstr'),
}

argumentParser = argparse.ArgumentParser(description='Benchmark Lua script execution with an option to compare different VMs')

argumentParser.add_argument('--vm', dest='vm',default=defaultVm,help='Lua executable to test (' + defaultVm + ' by default)')
argumentParser.add_argument('--folder', dest='folder',default=os.path.join(scriptdir, 'tests'),help='Folder with tests (tests by default)')
argumentParser.add_argument('--compare', dest='vmNext',type=str,nargs='*',help='List of Lua executables to compare against')
argumentParser.add_argument('--results', dest='results',type=str,nargs='*',help='List of json result files to compare and graph')
argumentParser.add_argument('--run-test', action='store', default=None, help='Regex test filter')
argumentParser.add_argument('--extra-loops', action='store',type=int,default=0, help='Amount of times to loop over one test (one test already performs multiple runs)')
argumentParser.add_argument('--filename', action='store',type=str,default='bench', help='File name for graph and results file')
argumentParser.add_argument('--callgrind', dest='callgrind',action='store_const',const=1,default=0,help='Use callgrind to run benchmarks')
argumentParser.add_argument('--show-commands', dest='show_commands',action='store_const',const=1,default=0,help='Show the command line used to launch the VM and tests')
argumentParser.add_argument('--metric', dest='metric', choices=list(METRIC_LABELS), default=None, help="Metric to benchmark, 'cycles' and 'instructions' are only supported on macOS")

if matplotlib != None:
    argumentParser.add_argument('--absolute', dest='absolute',action='store_const',const=1,default=0,help='Display absolute values instead of relative (enabled by default when benchmarking a single VM)')
    argumentParser.add_argument('--speedup', dest='speedup',action='store_const',const=1,default=0,help='Draw a speedup graph')
    argumentParser.add_argument('--sort', dest='sort',action='store_const',const=1,default=0,help='Sort values from worst to best improvements, ignoring conf. int. (disabled by default)')
    argumentParser.add_argument('--window', dest='window',action='store_const',const=1,default=0,help='Display window with resulting plot (disabled by default)')
    argumentParser.add_argument('--graph-vertical', action='store_true',dest='graph_vertical', help="Draw graph with vertical bars instead of horizontal")

argumentParser.add_argument('--report-metrics', dest='report_metrics', help="Send metrics about this session to InfluxDB URL upon completion.")

argumentParser.add_argument('--print-influx-debugging', action='store_true', dest='print_influx_debugging', help="Print output to aid in debugging of influx metrics reporting.")
argumentParser.add_argument('--no-print-influx-debugging', action='store_false', dest='print_influx_debugging', help="Don't print output to aid in debugging of influx metrics reporting.")

argumentParser.add_argument('--no-print-final-summary', action='store_false', dest='print_final_summary', help="Don't print a table summarizing the results after all tests are run")

# Assume 2.5 IPC on a 4 GHz CPU; this is obviously incorrect but it allows us to display simulated instruction counts using regular time units
CALLGRIND_INSN_PER_SEC = 2.5 * 4e9

# Counter names as printed by macOS `/usr/bin/time -l`
COUNTER_METRICS = {
    'cycles': 'cycles elapsed',
    'instructions': 'instructions retired',
}

# Increase runs for short benchmarks to reduce the relative impact of startup/setup noise, capped to limit the run count
COUNTER_TARGET_MS = 500
COUNTER_MIN_RUNS = 20
COUNTER_MAX_RUNS = 100000

valueMetric = 'time'

def setMetric(metric):
    global valueMetric
    if metric not in METRIC_LABELS:
        sys.exit('Unknown benchmark metric: ' + str(metric))
    valueMetric = metric

def formatValue(value):
    return '{:8.3f}'.format(value) + METRIC_LABELS[valueMetric][1]

def arrayRange(count):
    result = []

    for i in range(count):
        result.append(i)

    return result

def arrayRangeOffset(count, offset):
    result = []

    for i in range(count):
        result.append(i + offset)

    return result

def getCallgrindOutput(stdout, lines):
    result = []
    name = None

    for l in lines:
        if l.startswith("desc: Trigger: Client Request: "):
            name = l[31:].strip()
        elif l.startswith("summary: ") and name != None:
            insn = int(l[9:])
            # Note: we only run each bench once under callgrind so we only report a single time per run; callgrind instruction count variance is ~0.01% so it might as well be zero
            result += "|><|" + name + "|><|" + str(insn / CALLGRIND_INSN_PER_SEC * 1000.0) + "||_||"
            name = None

    # If no results were found above, this may indicate the native executable running
    # the benchmark doesn't have support for callgrind builtin.  In that case just
    # report the "totals" from the output file.
    if len(result) == 0:
        elements = stdout.decode('utf8').split("|><|")
        if len(elements) >= 2:
            name = elements[1]

            for l in lines:
                if l.startswith("totals: "):
                    insn = int(l[8:])
                    # Note: we only run each bench once under callgrind so we only report a single time per run; callgrind instruction count variance is ~0.01% so it might as well be zero
                    result += "|><|" + name + "|><|" + str(insn / CALLGRIND_INSN_PER_SEC * 1000.0) + "||_||"

    return "".join(result)

def conditionallyShowCommand(cmd):
    if arguments.show_commands:
        print(f'{colored(Color.BLUE, "EXECUTING")}: {cmd}')

def checkValgrindExecutable():
    """Return true if valgrind can be successfully spawned"""
    try:
        subprocess.check_call("valgrind --version", shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except:
        print(f"{colored(Color.YELLOW, 'WARNING')}: Unable to spawn 'valgrind'.  Please ensure valgrind is installed when using '--callgrind'.")
        return False
    
    return True

def getVmOutput(cmd):
    if os.name == "nt":
        try:
            fullCmd = "start /realtime /affinity 1 /b /wait cmd /C \"" + cmd + "\""
            conditionallyShowCommand(fullCmd)
            return subprocess.check_output(fullCmd, shell=True, cwd=scriptdir).decode()
        except KeyboardInterrupt:
            exit(1)
        except:
            return ""
    elif arguments.callgrind:
        if not checkValgrindExecutable():
            return ""
        output_path = os.path.join(scriptdir, "callgrind.out")
        try:
            os.unlink(output_path)  # Remove stale output
        except:
            pass
        fullCmd = "valgrind --tool=callgrind --callgrind-out-file=callgrind.out --combine-dumps=yes --dump-line=no " + cmd
        conditionallyShowCommand(fullCmd)
        try:
            output = subprocess.check_output(fullCmd, shell=True, stderr=subprocess.DEVNULL, cwd=scriptdir)
        except subprocess.CalledProcessError as e:
            print(f"{colored(Color.YELLOW, 'WARNING')}: Valgrind returned error code {e.returncode}")
            output = e.output
        with open(output_path, "r") as file:
            lines = file.readlines()
        os.unlink(output_path)
        return getCallgrindOutput(output, lines)
    else:
        conditionallyShowCommand(cmd)
        with subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, cwd=scriptdir) as p:
            # Try to lock to a single processor
            if sys.platform != "darwin":
                os.sched_setaffinity(p.pid, { 0 })

            # Try to set high priority (requires sudo)
            try:
                os.nice(-10)
            except:
                pass

            return p.communicate()[0]

def getLuauCliCounterArgs(vm, filepath, programArgs):
    """Default counterArgsCallback for the Luau CLI.

    Program args are only visible to top-level scripts, so bench_support.lua runs first and forwards them to the
    module instance that the test file requires."""
    return "{} {} -a {}".format(os.path.join(scriptdir, "bench_support.lua"), filepath, programArgs)

def launchWithCounter(cmd):
    """Run cmd under `/usr/bin/time -l`.

    Returns (stdout, counter value) or None if the launch failed or the counter was not reported."""
    fullCmd = "/usr/bin/time -l " + cmd
    conditionallyShowCommand(fullCmd)

    p = subprocess.run(fullCmd, shell=True, capture_output=True, text=True, cwd=scriptdir)

    match = re.search(r"^\s*(\d+)\s+" + COUNTER_METRICS[valueMetric] + r"\s*$", p.stderr, re.MULTILINE)
    if p.returncode != 0 or match is None:
        return None

    return p.stdout, int(match.group(1))

# Keyed by (test file, benchmark index) so every VM uses the same run count for a benchmark
counterRunsCache = {}

def getCounterRuns(filepath, index, lowOutput):
    """Returns the run count for a benchmark, scaled from the time of the single run in its low launch."""
    key = (filepath, index)
    if key not in counterRunsCache:
        runs = COUNTER_MIN_RUNS
        try:
            runMs = float(lowOutput.split("||_||")[index].split("|><|")[2])
            if runMs > 0:
                runs = max(runs, min(COUNTER_MAX_RUNS, math.ceil(COUNTER_TARGET_MS / runMs)))
        except (IndexError, ValueError):
            pass
        counterRunsCache[key] = runs

    return counterRunsCache[key]

def getCounterOutput(vm, cmd, filepath):
    """Measures with process-wide CPU counters instead of the timings reported by bench_support.lua.

    Counters cover the whole process, including startup and test setup. That fixed cost is removed by launching
    each benchmark twice, with 1 and 1 + N runs, and dividing the difference by N (see getCounterRuns). Files with
    several benchmarks launch once per benchmark, selected by index (see bench_support.lua).

    Returns output in the same format as bench_support.lua, with one value per benchmark in millions of counter
    events per run; runTest merges additional samples from --extra-loops."""
    def launch(only, runs):
        programArgs = "LUAU_BENCH_ONLY={} LUAU_BENCH_RUNS={}".format(only, runs)
        return launchWithCounter(cmd + " " + counterArgsCallback(vm, filepath, programArgs))

    # Selecting benchmark 0 skips every benchmark, which lists their names and warms the disk cache; a cold launch of a
    # large VM can cost hundreds of millions of extra cycles and would skew the first sample.
    warmup = launch(0, 0)
    if warmup is None:
        return ""

    entries = [el.split("|><|") for el in warmup[0].split("||_||")[:-1]]
    if any(len(el) > 2 for el in entries):
        sys.exit("--metric {} requires a VM that passes LUAU_BENCH_ONLY and LUAU_BENCH_RUNS program args to bench_support.lua: {}".format(valueMetric, cmd))

    names = [el[1] for el in entries]

    output = ""
    for index, name in enumerate(names):
        low = launch(index + 1, 1)
        if low is None:
            return ""

        runs = getCounterRuns(filepath, index, low[0])
        high = launch(index + 1, 1 + runs)
        if high is None:
            return ""

        value = (high[1] - low[1]) / runs / 1e6
        if value <= 0:
            print("{}: '{}' measured no work above per-launch noise with {} runs".format(colored(Color.YELLOW, 'WARNING'), name, runs))
            return ""

        output += "|><|" + name + "|><|" + str(value) + "||_||"
    return output

def getShortVmName(name):
    # Hope that the path to executable doesn't contain spaces
    argumentPos = name.find(" ")

    if argumentPos != -1:
        executableName = name[0:argumentPos]
        arguments = name[argumentPos+1:]

        pathPos = executableName.rfind("\\")

        if pathPos == -1:
            pathPos = executableName.rfind("/")

        if pathPos != -1:
            executableName = executableName[pathPos+1:]

        return executableName + " " + arguments

    pathPos = name.rfind("\\")

    if pathPos == -1:
        pathPos = name.rfind("/")

    if pathPos != -1:
        return name[pathPos+1:]

    return name

class TestResult:
    filename = ""
    vm = ""
    shortVm = ""
    name = ""

    values = []
    count = 0
    min = None
    avg = 0
    max = None

    sampleStdDev = 0
    unbiasedEst = 0
    sampleConfidenceInterval = 0

def extractResult(filename, vm, output):
    elements = output.split("|><|")

    # Remove test output
    elements.remove(elements[0])

    result = TestResult()

    result.filename = filename
    result.vm = vm
    result.shortVm = getShortVmName(vm)

    result.name = elements[0]
    elements.remove(elements[0])

    timeTable = []

    for el in elements:
        timeTable.append(float(el))

    result.values = timeTable
    result.count = len(timeTable)

    return result

def mergeResult(lhs, rhs):
    for value in rhs.values:
        lhs.values.append(value)

    lhs.count = len(lhs.values)

def mergeResults(lhs, rhs):
    for a, b in zip(lhs, rhs):
        mergeResult(a, b)

def finalizeResult(result):
    total = 0.0

    # Compute basic parameters
    for v in result.values:
        if result.min == None or v < result.min:
            result.min = v

        if result.max == None or v > result.max:
            result.max = v

        total = total + v

    if result.count > 0:
        result.avg = total / result.count
    else:
        result.avg = 0

    # Compute standard deviation
    sumOfSquares = 0

    for v in result.values:
        sumOfSquares = sumOfSquares + (v - result.avg) ** 2

    if result.count > 1:
        result.sampleStdDev = math.sqrt(sumOfSquares / (result.count - 1))
        result.unbiasedEst = result.sampleStdDev * result.sampleStdDev

        if stats:
            # Two-tailed distribution with 95% conf.
            tValue = stats.t.ppf(1 - 0.05 / 2, result.count - 1)

            # Compute confidence interval
            result.sampleConfidenceInterval = tValue * result.sampleStdDev / math.sqrt(result.count)
        else:
            result.sampleConfidenceInterval = result.sampleStdDev
    else:
        result.sampleStdDev = 0
        result.unbiasedEst = 0
        result.sampleConfidenceInterval = 0

    return result

# Full result set
allResults = []


# Data for the graph
plotLegend = []

plotLabels = []

plotValueLists = []
plotConfIntLists = []

# Totals
vmTotalMin = []
vmTotalAverage = []
vmTotalImprovement = []
vmTotalResults = []

# Data for Telegraf report
mainTotalMin = 0
mainTotalAverage = 0
mainTotalMax = 0

def getExtraArguments(filepath):
    try:
        with open(filepath) as f:
            for i in f.readlines():
                pos = i.find("--bench-args:")
                if pos != -1:
                    return i[pos + 13:].strip()
    except:
        pass

    return ""

def substituteArguments(cmd, extra):
    if argumentSubstituionCallback != None:
        cmd = argumentSubstituionCallback(cmd)

    if cmd.find("@EXTRA") != -1:
        cmd = cmd.replace("@EXTRA", extra)
    else:
        cmd = cmd + " " + extra

    return cmd

def extractResults(filename, vm, output, allowFailure):
    results = []

    splitOutput = output.split("||_||")

    if len(splitOutput) <= 1:
        if allowFailure:
            result = TestResult()

            result.filename = filename
            result.vm = vm
            result.shortVm = getShortVmName(vm)

            results.append(result)

        return results

    splitOutput.remove(splitOutput[len(splitOutput) - 1])

    for el in splitOutput:
        results.append(extractResult(filename, vm, el))

    return results

def analyzeResult(subdir, main, comparisons):
    # Aggregate statistics
    global mainTotalMin, mainTotalAverage, mainTotalMax

    mainTotalMin = mainTotalMin + main.min
    mainTotalAverage = mainTotalAverage + main.avg
    mainTotalMax = mainTotalMax + main.max

    if arguments.vmNext != None:
        resultPrinter.add_row({
            'Test': main.name,
            'Min': formatValue(main.min),
            'Average': formatValue(main.avg),
            'StdDev%': '{:8.3f}%'.format(main.sampleConfidenceInterval / main.avg * 100),
            'Driver': main.shortVm,
            'Speedup': "",
            'Significance': "",
            'P(T<=t)': ""
        })
    else:
        resultPrinter.add_row({
            'Test': main.name,
            'Min': formatValue(main.min),
            'Average': formatValue(main.avg),
            'StdDev%': '{:8.3f}%'.format(main.sampleConfidenceInterval / main.avg * 100),
            'Driver': main.shortVm
        })

    if reporter != None:
        reporter.report_result(subdir, main.name, main.filename, "SUCCESS", main.min, main.avg, main.max, main.sampleConfidenceInterval, main.shortVm, main.vm)

    print(colored(Color.GREEN, 'SUCCESS') + ': {:<40}'.format(main.name) + ": " + formatValue(main.avg) + " +/- " +
        '{:6.3f}'.format(main.sampleConfidenceInterval / main.avg * 100) + "% on " + main.shortVm)

    plotLabels.append(main.name)

    index = 0

    if len(plotValueLists) < index + 1:
        plotValueLists.append([])
        plotConfIntLists.append([])

        vmTotalMin.append(0.0)
        vmTotalAverage.append(0.0)
        vmTotalImprovement.append(0.0)
        vmTotalResults.append(0)

    if arguments.absolute or arguments.speedup:
        scale = 1
    else:
        scale = 100 / main.avg

    plotValueLists[index].append(main.avg * scale)
    plotConfIntLists[index].append(main.sampleConfidenceInterval * scale)

    vmTotalMin[index] += main.min
    vmTotalAverage[index] += main.avg

    for compare in comparisons:
        index = index + 1

        if len(plotValueLists) < index + 1 and not arguments.speedup:
            plotValueLists.append([])
            plotConfIntLists.append([])

            vmTotalMin.append(0.0)
            vmTotalAverage.append(0.0)
            vmTotalImprovement.append(0.0)
            vmTotalResults.append(0)

        if compare.min == None:
            print(colored(Color.RED, 'FAILED') + ":  '" + main.name + "' on '" + compare.vm +  "'")

            resultPrinter.add_row({ 'Test': main.name, 'Min': "", 'Average': "FAILED", 'StdDev%': "", 'Driver': compare.shortVm, 'Speedup': "", 'Significance': "", 'P(T<=t)': "" })

            if reporter != None:
                reporter.report_result(subdir, main.filename, main.filename, "FAILED", 0.0, 0.0, 0.0, 0.0, compare.shortVm, compare.vm)

            if arguments.speedup:
                plotValueLists[0].pop()
                plotValueLists[0].append(0)

                plotConfIntLists[0].pop()
                plotConfIntLists[0].append(0)
            else:
                plotValueLists[index].append(0)
                plotConfIntLists[index].append(0)

            continue

        if main.count > 1 and stats:
            pooledStdDev = math.sqrt((main.unbiasedEst + compare.unbiasedEst) / 2)

            tStat = abs(main.avg - compare.avg) / (pooledStdDev * math.sqrt(2 / main.count))
            degreesOfFreedom = 2 * main.count - 2

            # Two-tailed distribution with 95% conf.
            tCritical = stats.t.ppf(1 - 0.05 / 2, degreesOfFreedom)

            noSignificantDifference = tStat < tCritical
            pValue = 2 * (1 - stats.t.cdf(tStat, df = degreesOfFreedom))
        else:
            noSignificantDifference = None
            pValue = -1

        if noSignificantDifference is None:
            verdict = ""
        elif noSignificantDifference:
            verdict = "likely same"
        elif main.avg < compare.avg:
            verdict = "likely worse"
        else:
            verdict = "likely better"

        speedup = (plotValueLists[0][-1] / (compare.avg * scale) - 1)
        speedupColor = Color.YELLOW if speedup < 0 and noSignificantDifference else Color.RED if speedup < 0 else Color.GREEN if speedup > 0 else Color.YELLOW

        resultPrinter.add_row({
            'Test': main.name,
            'Min': formatValue(compare.min),
            'Average': formatValue(compare.avg),
            'StdDev%': '{:8.3f}%'.format(compare.sampleConfidenceInterval / compare.avg * 100),
            'Driver': compare.shortVm,
            'Speedup': colored(speedupColor, '{:8.3f}%'.format(speedup * 100)),
            'Significance': verdict,
            'P(T<=t)': '---' if pValue < 0 else '{:.0f}%'.format(pValue * 100)
        })

        print(colored(Color.GREEN, 'SUCCESS') + ': {:<40}'.format(main.name) + ": " + formatValue(compare.avg) + " +/- " +
            '{:6.3f}'.format(compare.sampleConfidenceInterval / compare.avg * 100) + "% on " + compare.shortVm +
            ' ({:+7.3f}%, '.format(speedup * 100) + verdict + ")")

        if reporter != None:
            reporter.report_result(subdir, main.name, main.filename, "SUCCESS", compare.min, compare.avg, compare.max, compare.sampleConfidenceInterval, compare.shortVm, compare.vm)

        if arguments.speedup:
            oldValue = plotValueLists[0].pop()
            newValue = compare.avg

            plotValueLists[0].append((oldValue / newValue - 1) * 100)

            plotConfIntLists[0].pop()
            plotConfIntLists[0].append(0)
        else:
            plotValueLists[index].append(compare.avg * scale)
            plotConfIntLists[index].append(compare.sampleConfidenceInterval * scale)

        vmTotalMin[index] += compare.min
        vmTotalAverage[index] += compare.avg
        vmTotalImprovement[index] += math.log(main.avg / compare.avg)
        vmTotalResults[index] += 1

def getTestOutput(vm, filepath):
    cmd = substituteArguments(vm, getExtraArguments(filepath))

    if valueMetric != 'time':
        return getCounterOutput(vm, cmd, filepath)

    return getVmOutput(cmd + " " + filepath)

def runTest(subdir, filename, filepath):
    filepath = os.path.abspath(filepath)

    mainVm = os.path.abspath(arguments.vm)

    # Process output will contain the test name and execution times
    mainOutput = getTestOutput(mainVm, filepath)
    mainResultSet = extractResults(filename, mainVm, mainOutput, False)

    if len(mainResultSet) == 0:
        print(colored(Color.RED, 'FAILED') + ":  '" + filepath + "' on '" + mainVm +  "'")

        if arguments.vmNext != None:
            resultPrinter.add_row({ 'Test': filepath, 'Min': "", 'Average': "FAILED", 'StdDev%': "", 'Driver': getShortVmName(mainVm), 'Speedup': "", 'Significance': "", 'P(T<=t)': "" })
        else:
            resultPrinter.add_row({ 'Test': filepath, 'Min': "", 'Average': "FAILED", 'StdDev%': "", 'Driver': getShortVmName(mainVm) })

        if reporter != None:
            reporter.report_result(subdir, filename, filename, "FAILED", 0.0, 0.0, 0.0, 0.0, getShortVmName(mainVm), mainVm)
        return

    compareResultSets = []

    if arguments.vmNext != None:
        for compareVm in arguments.vmNext:
            compareVm = os.path.abspath(compareVm)

            compareOutput = getTestOutput(compareVm, filepath)
            compareResultSet = extractResults(filename, compareVm, compareOutput, True)

            compareResultSets.append(compareResultSet)

    if arguments.extra_loops > 0:
        # get more results
        for i in range(arguments.extra_loops):
            extraMainOutput = getTestOutput(mainVm, filepath)
            extraMainResultSet = extractResults(filename, mainVm, extraMainOutput, False)

            mergeResults(mainResultSet, extraMainResultSet)

            if arguments.vmNext != None:
                i = 0
                for compareVm in arguments.vmNext:
                    compareVm = os.path.abspath(compareVm)

                    extraCompareOutput = getTestOutput(compareVm, filepath)
                    extraCompareResultSet = extractResults(filename, compareVm, extraCompareOutput, True)

                    mergeResults(compareResultSets[i], extraCompareResultSet)
                    i += 1

    # finalize results
    for result in mainResultSet:
        finalizeResult(result)

    for compareResultSet in compareResultSets:
        for result in compareResultSet:
            finalizeResult(result)

    # analyze results
    for i in range(len(mainResultSet)):
        mainResult = mainResultSet[i]
        compareResults = []

        for el in compareResultSets:
            if i < len(el):
                compareResults.append(el[i])
            else:
                noResult = TestResult()

                noResult.filename = el[0].filename
                noResult.vm = el[0].vm
                noResult.shortVm = el[0].shortVm

                compareResults.append(noResult)

        analyzeResult(subdir, mainResult, compareResults)

        mergedResults = []
        mergedResults.append(mainResult)

        for el in compareResults:
            mergedResults.append(el)

        allResults.append(mergedResults)

def rearrangeSortKeyForComparison(e):
    if plotValueLists[1][e] == 0:
        return 1

    return plotValueLists[0][e] / plotValueLists[1][e]

def rearrangeSortKeyForSpeedup(e):
    return plotValueLists[0][e]

def rearrangeSortKeyDescending(e):
    return -plotValueLists[0][e]

# Re-arrange results from worst to best
def rearrange(key):
    global plotLabels

    index = arrayRange(len(plotLabels))
    index = sorted(index, key=key)

    # Recreate value lists in sorted order
    plotLabelsPrev = plotLabels
    plotLabels = []

    for i in index:
        plotLabels.append(plotLabelsPrev[i])

    for group in range(len(plotValueLists)):
        plotValueListPrev = plotValueLists[group]
        plotValueLists[group] = []

        plotConfIntListPrev = plotConfIntLists[group]
        plotConfIntLists[group] = []

        for i in index:
            plotValueLists[group].append(plotValueListPrev[i])
            plotConfIntLists[group].append(plotConfIntListPrev[i])

# Graph
def graph():
    if len(plotValueLists) == 0:
        print("No results")
        return

    ind = arrayRange(len(plotLabels))
    width = 0.8 / len(plotValueLists)
    valueLabel, valueUnit = METRIC_LABELS[valueMetric]
    if arguments.absolute:
        axisLabel = valueLabel + ' (' + valueUnit + ')'
    elif arguments.speedup:
        axisLabel = 'Speedup (%)'
    else:
        axisLabel = 'Relative ' + valueLabel.lower() + ' (%)'

    if arguments.graph_vertical:
        # Extend graph width when we have a lot of tests to draw
        barcount = len(plotValueLists[0])
        plt.figure(figsize=(max(8, barcount * 0.3), 8))
    else:
        # Extend graph height when we have a lot of tests to draw
        barcount = len(plotValueLists[0])
        plt.figure(figsize=(8, max(8, barcount * 0.3)))

    plotBars = []

    matplotlib.rc('xtick', labelsize=10)
    matplotlib.rc('ytick', labelsize=10)

    if arguments.graph_vertical:
        # Draw Y grid behind the bars
        plt.rc('axes', axisbelow=True)
        plt.grid(True, 'major', 'y')

        for i in range(len(plotValueLists)):
            bar = plt.bar(arrayRangeOffset(len(plotLabels), i * width), plotValueLists[i], width, yerr=plotConfIntLists[i])
            plotBars.append(bar[0])

        plt.ylabel(axisLabel)

        plt.title('Benchmark')
        plt.xticks(ind, plotLabels, rotation='vertical')
    else:
        # Draw X grid behind the bars
        plt.rc('axes', axisbelow=True)
        plt.grid(True, 'major', 'x')

        for i in range(len(plotValueLists)):
            bar = plt.barh(arrayRangeOffset(len(plotLabels), i * width), plotValueLists[i], width, xerr=plotConfIntLists[i])
            plotBars.append(bar[0])

        plt.xlabel(axisLabel)

        plt.title('Benchmark')
        plt.yticks(ind, plotLabels)

        plt.gca().invert_yaxis()

    plt.legend(plotBars, plotLegend)

    plt.tight_layout()

    plt.savefig(arguments.filename + ".png", dpi=200)

    if arguments.window:
        plt.show()

def computeGeomean(vmIndex):
    """Compute per-iteration geometric means across all benchmarks for a given VM index.

    Returns (TestResult, None) on success, (None, errorString) on failure."""
    if len(allResults) == 0:
        return None, None

    # Gather the values arrays for the given VM index across all benchmarks
    valuesPerBenchmark = []
    for resultSet in allResults:
        if vmIndex >= len(resultSet):
            continue
        result = resultSet[vmIndex]
        if result.min is None or len(result.values) == 0:
            continue
        valuesPerBenchmark.append(result.values)

    if len(valuesPerBenchmark) == 0:
        return None, None

    # All benchmarks must have the same iteration count
    n = len(valuesPerBenchmark[0])
    for values in valuesPerBenchmark:
        if len(values) != n:
            counts = sorted(set(len(v) for v in valuesPerBenchmark))
            return None, "inconsistent iteration counts ({})".format(", ".join(str(c) for c in counts))

    # Compute per-iteration geometric means
    geomeanValues = []
    numBenchmarks = len(valuesPerBenchmark)
    for k in range(n):
        product = 1.0
        for values in valuesPerBenchmark:
            product *= values[k]
        geomeanValues.append(product ** (1.0 / numBenchmarks))

    geomeanResult = TestResult()
    geomeanResult.filename = "<geomean>"
    geomeanResult.vm = allResults[0][vmIndex].vm if vmIndex < len(allResults[0]) else ""
    geomeanResult.shortVm = allResults[0][vmIndex].shortVm if vmIndex < len(allResults[0]) else ""
    geomeanResult.name = "Geomean"
    geomeanResult.values = geomeanValues
    geomeanResult.count = len(geomeanValues)

    finalizeResult(geomeanResult)
    return geomeanResult, None

def addTotalsToTable():
    if len(vmTotalMin) == 0:
        return

    if arguments.vmNext != None:
        index = 0

        resultPrinter.add_row({
            'Test': 'Total',
            'Min': formatValue(vmTotalMin[index]),
            'Average': formatValue(vmTotalAverage[index]),
            'StdDev%': "---",
            'Driver': getShortVmName(os.path.abspath(arguments.vm)),
            'Speedup': "",
            'Significance': "",
            'P(T<=t)': ""
        })

        for compareVm in arguments.vmNext:
            index = index + 1

            speedup = vmTotalAverage[0] / vmTotalAverage[index] * 100 - 100

            resultPrinter.add_row({
                'Test': 'Total',
                'Min': formatValue(vmTotalMin[index]),
                'Average': formatValue(vmTotalAverage[index]),
                'StdDev%': "---",
                'Driver': getShortVmName(os.path.abspath(compareVm)),
                'Speedup': colored(Color.RED if speedup < 0 else Color.GREEN if speedup > 0 else Color.YELLOW, '{:8.3f}%'.format(speedup)),
                'Significance': "",
                'P(T<=t)': ""
            })
    else:
        resultPrinter.add_row({
            'Test': 'Total',
            'Min': formatValue(vmTotalMin[0]),
            'Average': formatValue(vmTotalAverage[0]),
            'StdDev%': "---",
            'Driver': getShortVmName(os.path.abspath(arguments.vm))
        })

def addGeomeanToTable():
    mainGeomean, mainError = computeGeomean(0)
    if mainGeomean is None and mainError is None:
        return

    if mainError is not None:
        if arguments.vmNext != None:
            resultPrinter.add_row({
                'Test': 'Geomean',
                'Min': "",
                'Average': colored(Color.RED, mainError),
                'StdDev%': "",
                'Driver': getShortVmName(os.path.abspath(arguments.vm)),
                'Speedup': "",
                'Significance': "",
                'P(T<=t)': ""
            })
        else:
            resultPrinter.add_row({
                'Test': 'Geomean',
                'Min': "",
                'Average': colored(Color.RED, mainError),
                'StdDev%': "",
                'Driver': getShortVmName(os.path.abspath(arguments.vm))
            })
        return

    if arguments.vmNext != None:
        resultPrinter.add_row({
            'Test': 'Geomean',
            'Min': formatValue(mainGeomean.min),
            'Average': formatValue(mainGeomean.avg),
            'StdDev%': '{:8.3f}%'.format(mainGeomean.sampleConfidenceInterval / mainGeomean.avg * 100),
            'Driver': mainGeomean.shortVm,
            'Speedup': "",
            'Significance': "",
            'P(T<=t)': ""
        })

        index = 0
        for compareVm in arguments.vmNext:
            index = index + 1
            compareGeomean, compareError = computeGeomean(index)
            if compareGeomean is None:
                continue

            speedup = mainGeomean.avg / compareGeomean.avg * 100 - 100

            resultPrinter.add_row({
                'Test': 'Geomean',
                'Min': formatValue(compareGeomean.min),
                'Average': formatValue(compareGeomean.avg),
                'StdDev%': '{:8.3f}%'.format(compareGeomean.sampleConfidenceInterval / compareGeomean.avg * 100),
                'Driver': compareGeomean.shortVm,
                'Speedup': colored(Color.RED if speedup < 0 else Color.GREEN if speedup > 0 else Color.YELLOW, '{:8.3f}%'.format(speedup)),
                'Significance': "",
                'P(T<=t)': ""
            })
    else:
        resultPrinter.add_row({
            'Test': 'Geomean',
            'Min': formatValue(mainGeomean.min),
            'Average': formatValue(mainGeomean.avg),
            'StdDev%': '{:8.3f}%'.format(mainGeomean.sampleConfidenceInterval / mainGeomean.avg * 100),
            'Driver': mainGeomean.shortVm
        })

def writeResultsToFile():
    class TestResultEncoder(json.JSONEncoder):
        def default(self, obj):
            if isinstance(obj, TestResult):
                return [obj.filename, obj.vm, obj.shortVm, obj.name, obj.values, obj.count]
            return json.JSONEncoder.default(self, obj)

    try:
        with open(arguments.filename + ".json", "w", encoding="utf-8") as allResultsFile:
            allResultsFile.write(json.dumps({'metric': valueMetric, 'results': allResults}, cls=TestResultEncoder))
    except:
        print("Failed to write results to a file")

def run(args, argsubcb, reporter_factory=None, counterargscb=getLuauCliCounterArgs):
    """counterargscb(vm, filepath, programArgs) returns the command line tail that runs filepath and passes programArgs
    (LUAU_BENCH_ONLY=<n> LUAU_BENCH_RUNS=<n>) to bench_support.lua when measuring with CPU counters."""
    global arguments, resultPrinter, reporter, argumentSubstituionCallback, counterArgsCallback, allResults
    arguments = args
    argumentSubstituionCallback = argsubcb
    counterArgsCallback = counterargscb
    expectedMetric = arguments.metric
    setMetric(expectedMetric or 'time')

    if os.name == "nt" and arguments.callgrind:
        print(f"{colored(Color.RED, 'ERROR')}: --callgrind is not supported on Windows.  Please consider using this option on another OS, or Linux using WSL.")
        sys.exit(1)

    if arguments.results is None and valueMetric != 'time':
        if sys.platform != "darwin":
            sys.exit("--metric {} is only supported on macOS".format(valueMetric))
        if arguments.callgrind:
            sys.exit("--metric {} can't be combined with --callgrind".format(valueMetric))

    if matplotlib == None:
        arguments.absolute = 0
        arguments.speedup = 0
        arguments.sort = 0
        arguments.window = 0

    # Load results from files
    if arguments.results != None:
        vmList = []

        for result in arguments.results:
            with open(result, encoding="utf-8") as resultsFile:
                resultData = json.load(resultsFile)

            # Legacy result arrays contain timings in milliseconds.
            resultMetric = resultData['metric'] if isinstance(resultData, dict) else 'time'
            resultArray = resultData['results'] if isinstance(resultData, dict) else resultData
            if expectedMetric is not None and resultMetric != expectedMetric:
                sys.exit("{}: metric '{}' does not match '{}'".format(result, resultMetric, expectedMetric))
            setMetric(resultMetric)
            expectedMetric = resultMetric

            for test in resultArray:
                for i in range(len(test)):
                    arr = test[i]

                    tr = TestResult()

                    tr.filename = arr[0]
                    tr.vm = arr[1]
                    tr.shortVm = arr[2]
                    tr.name = arr[3]
                    tr.values = arr[4]
                    tr.count = arr[5]

                    test[i] = tr

            for test in resultArray[0]:
                if vmList.count(test.vm) > 0:
                    pointPos = result.rfind(".")

                    if pointPos != -1:
                        vmList.append(test.vm + " [" + result[0:pointPos] + "]")
                    else:
                        vmList.append(test.vm + " [" + result + "]")
                else:
                    vmList.append(test.vm)

            if len(allResults) == 0:
                allResults = resultArray
            else:
                for prevEl in allResults:
                    found = False

                    for nextEl in resultArray:
                        if nextEl[0].filename == prevEl[0].filename and nextEl[0].name == prevEl[0].name:
                            for run in nextEl:
                                prevEl.append(run)
                            found = True

                    if not found:
                        el = resultArray[0]

                        for run in el:
                            result = TestResult()

                            result.filename = run.filename
                            result.vm = run.vm
                            result.shortVm = run.shortVm
                            result.name = run.name

                            prevEl.append(result)

        arguments.vmNext = []

        for i in range(len(vmList)):
            if i == 0:
                arguments.vm = vmList[i]
            else:
                arguments.vmNext.append(vmList[i])

    if valueMetric != 'time' and (reporter_factory or arguments.report_metrics or arguments.print_influx_debugging):
        sys.exit('Counter results cannot be sent to time-based metric reporters')
    if reporter_factory:
        reporter = reporter_factory(arguments)
    elif arguments.report_metrics or arguments.print_influx_debugging:
        reporter = influxbench.InfluxReporter(arguments)
    else:
        reporter = None

    plotLegend.append(getShortVmName(arguments.vm))

    if arguments.vmNext != None:
        for compareVm in arguments.vmNext:
            plotLegend.append(getShortVmName(compareVm))
    else:
        arguments.absolute = 1 # When looking at one VM, I feel that relative graph doesn't make a lot of sense

    # Results table formatting
    if arguments.vmNext != None:
        resultPrinter = TablePrinter([
            {'label': 'Test', 'align': Alignment.LEFT},
            {'label': 'Min', 'align': Alignment.RIGHT},
            {'label': 'Average', 'align': Alignment.RIGHT},
            {'label': 'StdDev%', 'align': Alignment.RIGHT},
            {'label': 'Driver', 'align': Alignment.LEFT},
            {'label': 'Speedup', 'align': Alignment.RIGHT},
            {'label': 'Significance', 'align': Alignment.LEFT},
            {'label': 'P(T<=t)', 'align': Alignment.RIGHT}
        ])
    else:
        resultPrinter = TablePrinter([
            {'label': 'Test', 'align': Alignment.LEFT},
            {'label': 'Min', 'align': Alignment.RIGHT},
            {'label': 'Average', 'align': Alignment.RIGHT},
            {'label': 'StdDev%', 'align': Alignment.RIGHT},
            {'label': 'Driver', 'align': Alignment.LEFT}
        ])

    if arguments.results != None:
        for resultSet in allResults:
            # finalize results
            for result in resultSet:
                finalizeResult(result)

            # analyze results
            mainResult = resultSet[0]
            compareResults = []

            for i in range(len(resultSet)):
                if i != 0:
                    compareResults.append(resultSet[i])

            analyzeResult('', mainResult, compareResults)
    else:
        all_files = [subdir + os.sep + filename for subdir, dirs, files in os.walk(arguments.folder) for filename in files]
        for filepath in sorted(all_files):
            subdir, filename = os.path.split(filepath)
            if filename.endswith(".lua") or filename.endswith(".luau"):
                if os.path.isfile(os.path.join(subdir, "bench_resource_directory")):
                    continue
                if arguments.run_test == None or re.match(arguments.run_test, filename[:-4]):
                    runTest(subdir, filename, filepath)

    if arguments.sort and len(plotValueLists) > 1:
        rearrange(rearrangeSortKeyForComparison)
    elif arguments.sort and len(plotValueLists) == 1:
        rearrange(rearrangeSortKeyDescending)
    elif arguments.speedup:
        rearrange(rearrangeSortKeyForSpeedup)

        plotLegend[0] = arguments.vm + " vs " + arguments.vmNext[0]

    if arguments.print_final_summary:
        addTotalsToTable()
        addGeomeanToTable()

        print()
        print(colored(Color.YELLOW, '==================================================RESULTS=================================================='))
        resultPrinter.print(summary=False)
        print(colored(Color.YELLOW, '---'))

    if len(vmTotalMin) != 0 and arguments.vmNext != None:
        index = 0

        for compareVm in arguments.vmNext:
            index = index + 1

            name = getShortVmName(os.path.abspath(compareVm))
            deltaGeoMean = math.exp(vmTotalImprovement[index] / vmTotalResults[index]) * 100 - 100

            if deltaGeoMean > 0:
                print("'{}' change is {:.3f}% positive on average".format(name, deltaGeoMean))
            else:
                print("'{}' change is {:.3f}% negative on average".format(name, deltaGeoMean))

    if matplotlib != None:
        graph()

    writeResultsToFile()

    if reporter != None:
        reporter.report_result(arguments.folder, "Total", "all", "SUCCESS", mainTotalMin, mainTotalAverage, mainTotalMax, 0.0, getShortVmName(arguments.vm), os.path.abspath(arguments.vm))
        reporter.flush(0)


if __name__ == "__main__":
    try:
        import argcomplete
        argcomplete.autocomplete(argumentParser)
    except ImportError:
        pass

    arguments = argumentParser.parse_args()
    run(arguments, None)
