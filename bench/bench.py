#!/usr/bin/python
# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
import argparse
import os
import subprocess
import math
import sys
import re
import json

# Taken from rotest
from color import colored, Color
from tabulate import TablePrinter, Alignment

# Based on rotest, specialized for benchmark results
import influxbench

try:
    import matplotlib
    import matplotlib.pyplot as plt
except ModuleNotFoundError:
    matplotlib = None

try:
    import scipy
    from scipy import stats
except ModuleNotFoundError:
    print("scipy package is required")
    exit(1)

scriptdir = os.path.dirname(os.path.realpath(__file__))
defaultVm = 'luau.exe' if os.name == "nt" else './luau'

argumentParser = argparse.ArgumentParser(description='Benchmark Lua script execution with an option to compare different VMs')

argumentParser.add_argument('--vm', dest='vm',default=defaultVm,help='Lua executable to test (' + defaultVm + ' by default)')
argumentParser.add_argument('--folder', dest='folder',default=os.path.join(scriptdir, 'tests'),help='Folder with tests (tests by default)')
argumentParser.add_argument('--compare', dest='vmNext',type=str,nargs='*',help='List of Lua executables to compare against')
argumentParser.add_argument('--results', dest='results',type=str,nargs='*',help='List of json result files to compare and graph')
argumentParser.add_argument('--run-test', action='store', default=None, help='Regex test filter')
argumentParser.add_argument('--extra-loops', action='store',type=int,default=0, help='Amount of times to loop over one test (one test already performs multiple runs)')
argumentParser.add_argument('--filename', action='store',type=str,default='bench', help='File name for graph and results file')

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

def getVmOutput(cmd):
    if os.name == "nt":
        try:
            return subprocess.check_output("start /realtime /affinity 1 /b /wait cmd /C \"" + cmd + "\"", shell=True, cwd=scriptdir).decode()
        except KeyboardInterrupt:
            exit(1)
        except:
            return ""
    else:
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

        # Two-tailed distribution with 95% conf.
        tValue = stats.t.ppf(1 - 0.05 / 2, result.count - 1)

        # Compute confidence interval
        result.sampleConfidenceInterval = tValue * result.sampleStdDev / math.sqrt(result.count)
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
            'Min': '{:8.3f}ms'.format(main.min),
            'Average': '{:8.3f}ms'.format(main.avg),
            'StdDev%': '{:8.3f}%'.format(main.sampleConfidenceInterval / main.avg * 100),
            'Driver': main.shortVm,
            'Speedup': "",
            'Significance': "",
            'P(T<=t)': ""
        })
    else:
        resultPrinter.add_row({
            'Test': main.name,
            'Min': '{:8.3f}ms'.format(main.min),
            'Average': '{:8.3f}ms'.format(main.avg),
            'StdDev%': '{:8.3f}%'.format(main.sampleConfidenceInterval / main.avg * 100),
            'Driver': main.shortVm
        })

    if influxReporter != None:
        influxReporter.report_result(subdir, main.name, main.filename, "SUCCESS", main.min, main.avg, main.max, main.sampleConfidenceInterval, main.shortVm, main.vm)

    print(colored(Color.YELLOW, 'SUCCESS') + ': {:<40}'.format(main.name) + ": " + '{:8.3f}'.format(main.avg) + "ms +/- " +
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

            if influxReporter != None:
                influxReporter.report_result(subdir, main.filename, main.filename, "FAILED", 0.0, 0.0, 0.0, 0.0, compare.shortVm, compare.vm)

            if arguments.speedup:
                plotValueLists[0].pop()
                plotValueLists[0].append(0)

                plotConfIntLists[0].pop()
                plotConfIntLists[0].append(0)
            else:
                plotValueLists[index].append(0)
                plotConfIntLists[index].append(0)

            continue

        pooledStdDev = math.sqrt((main.unbiasedEst + compare.unbiasedEst) / 2)

        tStat = abs(main.avg - compare.avg) / (pooledStdDev * math.sqrt(2 / main.count))
        degreesOfFreedom = 2 * main.count - 2

        # Two-tailed distribution with 95% conf.
        tCritical = stats.t.ppf(1 - 0.05 / 2, degreesOfFreedom)

        noSignificantDifference = tStat < tCritical

        pValue = 2 * (1 - stats.t.cdf(tStat, df = degreesOfFreedom))

        if noSignificantDifference:
            verdict = "likely same"
        elif main.avg < compare.avg:
            verdict = "likely worse"
        else:
            verdict = "likely better"

        speedup = (plotValueLists[0][-1] / (compare.avg * scale) - 1)
        speedupColor = Color.YELLOW if speedup < 0 and noSignificantDifference else Color.RED if speedup < 0 else Color.GREEN if speedup > 0 else Color.YELLOW

        resultPrinter.add_row({
            'Test': main.name,
            'Min': '{:8.3f}ms'.format(compare.min),
            'Average': '{:8.3f}ms'.format(compare.avg),
            'StdDev%': '{:8.3f}%'.format(compare.sampleConfidenceInterval / compare.avg * 100),
            'Driver': compare.shortVm,
            'Speedup': colored(speedupColor, '{:8.3f}%'.format(speedup * 100)),
            'Significance': verdict,
            'P(T<=t)': '---' if pValue < 0 else '{:.0f}%'.format(pValue * 100)
        })

        print(colored(Color.YELLOW, 'SUCCESS') + ': {:<40}'.format(main.name) + ": " + '{:8.3f}'.format(compare.avg) + "ms +/- " +
            '{:6.3f}'.format(compare.sampleConfidenceInterval / compare.avg * 100) + "% on " + compare.shortVm +
            ' ({:+7.3f}%, '.format(speedup * 100) + verdict + ")")

        if influxReporter != None:
            influxReporter.report_result(subdir, main.name, main.filename, "SUCCESS", compare.min, compare.avg, compare.max, compare.sampleConfidenceInterval, compare.shortVm, compare.vm)

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

def runTest(subdir, filename, filepath):
    filepath = os.path.abspath(filepath)

    mainVm = os.path.abspath(arguments.vm)

    # Process output will contain the test name and execution times
    mainOutput = getVmOutput(substituteArguments(mainVm, getExtraArguments(filepath)) + " " + filepath)
    mainResultSet = extractResults(filename, mainVm, mainOutput, False)

    if len(mainResultSet) == 0:
        print(colored(Color.RED, 'FAILED') + ":  '" + filepath + "' on '" + mainVm +  "'")

        if arguments.vmNext != None:
            resultPrinter.add_row({ 'Test': filepath, 'Min': "", 'Average': "FAILED", 'StdDev%': "", 'Driver': getShortVmName(mainVm), 'Speedup': "", 'Significance': "", 'P(T<=t)': "" })
        else:
            resultPrinter.add_row({ 'Test': filepath, 'Min': "", 'Average': "FAILED", 'StdDev%': "", 'Driver': getShortVmName(mainVm) })

        if influxReporter != None:
            influxReporter.report_result(subdir, filename, filename, "FAILED", 0.0, 0.0, 0.0, 0.0, getShortVmName(mainVm), mainVm)
        return

    compareResultSets = []

    if arguments.vmNext != None:
        for compareVm in arguments.vmNext:
            compareVm = os.path.abspath(compareVm)

            compareOutput = getVmOutput(substituteArguments(compareVm, getExtraArguments(filepath)) + " " + filepath)
            compareResultSet = extractResults(filename, compareVm, compareOutput, True)

            compareResultSets.append(compareResultSet)

    if arguments.extra_loops > 0:
        # get more results
        for i in range(arguments.extra_loops):
            extraMainOutput = getVmOutput(substituteArguments(mainVm, getExtraArguments(filepath)) + " " + filepath)
            extraMainResultSet = extractResults(filename, mainVm, extraMainOutput, False)

            mergeResults(mainResultSet, extraMainResultSet)

            if arguments.vmNext != None:
                i = 0
                for compareVm in arguments.vmNext:
                    compareVm = os.path.abspath(compareVm)

                    extraCompareOutput = getVmOutput(substituteArguments(compareVm, getExtraArguments(filepath)) + " " + filepath)
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

        if arguments.absolute:
            plt.ylabel('Time (ms)')
        elif arguments.speedup:
            plt.ylabel('Speedup (%)')
        else:
            plt.ylabel('Relative time (%)')

        plt.title('Benchmark')
        plt.xticks(ind, plotLabels, rotation='vertical')
    else:
        # Draw X grid behind the bars
        plt.rc('axes', axisbelow=True)
        plt.grid(True, 'major', 'x')

        for i in range(len(plotValueLists)):
            bar = plt.barh(arrayRangeOffset(len(plotLabels), i * width), plotValueLists[i], width, xerr=plotConfIntLists[i])
            plotBars.append(bar[0])

        if arguments.absolute:
            plt.xlabel('Time (ms)')
        elif arguments.speedup:
            plt.xlabel('Speedup (%)')
        else:
            plt.xlabel('Relative time (%)')

        plt.title('Benchmark')
        plt.yticks(ind, plotLabels)

        plt.gca().invert_yaxis()

    plt.legend(plotBars, plotLegend)

    plt.tight_layout()

    plt.savefig(arguments.filename + ".png", dpi=200)

    if arguments.window:
        plt.show()

def addTotalsToTable():
    if len(vmTotalMin) == 0:
        return

    if arguments.vmNext != None:
        index = 0

        resultPrinter.add_row({
            'Test': 'Total',
            'Min': '{:8.3f}ms'.format(vmTotalMin[index]),
            'Average': '{:8.3f}ms'.format(vmTotalAverage[index]),
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
                'Min': '{:8.3f}ms'.format(vmTotalMin[index]),
                'Average': '{:8.3f}ms'.format(vmTotalAverage[index]),
                'StdDev%': "---",
                'Driver': getShortVmName(os.path.abspath(compareVm)),
                'Speedup': colored(Color.RED if speedup < 0 else Color.GREEN if speedup > 0 else Color.YELLOW, '{:8.3f}%'.format(speedup)),
                'Significance': "",
                'P(T<=t)': ""
            })
    else:
        resultPrinter.add_row({
            'Test': 'Total',
            'Min': '{:8.3f}ms'.format(vmTotalMin[0]),
            'Average': '{:8.3f}ms'.format(vmTotalAverage[0]),
            'StdDev%': "---",
            'Driver': getShortVmName(os.path.abspath(arguments.vm))
        })

def writeResultsToFile():
    class TestResultEncoder(json.JSONEncoder):
        def default(self, obj):
            if isinstance(obj, TestResult):
                return [obj.filename, obj.vm, obj.shortVm, obj.name, obj.values, obj.count]
            return json.JSONEncoder.default(self, obj)

    try:
        with open(arguments.filename + ".json", "w") as allResultsFile:
            allResultsFile.write(json.dumps(allResults, cls=TestResultEncoder))
    except:
        print("Failed to write results to a file")

def run(args, argsubcb):
    global arguments, resultPrinter, influxReporter, argumentSubstituionCallback, allResults
    arguments = args
    argumentSubstituionCallback = argsubcb

    if arguments.report_metrics or arguments.print_influx_debugging:
        influxReporter = influxbench.InfluxReporter(arguments)
    else:
        influxReporter = None

    if matplotlib == None:
        arguments.absolute = 0
        arguments.speedup = 0
        arguments.sort = 0
        arguments.window = 0

    # Load results from files
    if arguments.results != None:
        vmList = []

        for result in arguments.results:
            with open(result) as resultsFile:
                resultArray = json.load(resultsFile)

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
        for subdir, dirs, files in os.walk(arguments.folder):
            for filename in files:
                filepath = subdir + os.sep + filename

                if filename.endswith(".lua"):
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

    if influxReporter != None:
        influxReporter.report_result(arguments.folder, "Total", "all", "SUCCESS", mainTotalMin, mainTotalAverage, mainTotalMax, 0.0, getShortVmName(arguments.vm), os.path.abspath(arguments.vm))
        influxReporter.flush(0)


if __name__ == "__main__":
    arguments = argumentParser.parse_args()
    run(arguments, None)
