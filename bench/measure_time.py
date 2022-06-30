# This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
import os, sys, time, numpy 

try:
  import scipy
  from scipy import mean, stats
except ModuleNotFoundError:
  print("Warning: scipy package is not installed, confidence values will not be available")
  stats = None

duration_list = []

DEFAULT_CYCLES_TO_RUN = 100
cycles_to_run = DEFAULT_CYCLES_TO_RUN

try:
  cycles_to_run = sys.argv[3] if sys.argv[3] else DEFAULT_CYCLES_TO_RUN
  cycles_to_run = int(cycles_to_run)
except IndexError:
  pass
except (ValueError, TypeError):
  cycles_to_run = DEFAULT_CYCLES_TO_RUN
  print("Error: Cycles to run argument must be an integer. Using default value of {}".format(DEFAULT_CYCLES_TO_RUN))

# Numpy complains if we provide a cycle count of less than 3 ~ default to 3 whenever a lower value is provided
cycles_to_run = cycles_to_run if cycles_to_run > 2 else 3

for i in range(1,cycles_to_run):
  start = time.perf_counter()
  
  # Run the code you want to measure here
  os.system(sys.argv[1])

  end = time.perf_counter()

  duration_ms = (end - start) * 1000
  duration_list.append(duration_ms)

# Stats
mean = numpy.mean(duration_list)
std_err = stats.sem(duration_list)

print("SUCCESS: {} : {:.2f}ms +/- {:.2f}% on luau ".format('duration', mean,std_err))
