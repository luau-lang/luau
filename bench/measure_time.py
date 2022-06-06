import os, sys, time
try:
    import numpy as np
    from scipy import mean, stats
except ModuleNotFoundError:
    print("Warning: scipy package is not installed, confidence values will not be available")
    stats = None

duration_list = []

for i in range(1,10):
  start = time.perf_counter()


  os.system(sys.argv[1])

  end = time.perf_counter()

  duration_ms = (end - start) * 1000
  print("SUCCESS: {} : {:.2f}ms +/- {:.2f}% on luau ".format('duration', duration_ms,0))
  duration_list.append(duration_ms)


# Stats
mean = np.mean(duration_list)
std_err = stats.sem(duration_list)

print("--------------------------------------------------------------------------------")
print("SUCCESS: {} : {:.2f}ms +/- {:.2f}% on luau ".format('duration', mean,std_err))
