import sys, subprocess, resource, time
t = time.time(); r = subprocess.run(sys.argv[2:], stdout=subprocess.DEVNULL, stderr=open(sys.argv[1] + ".err", "w"))
ru = resource.getrusage(resource.RUSAGE_CHILDREN)
open(sys.argv[1], "w").write("%d %.1f %d\n" % (ru.ru_maxrss, time.time() - t, r.returncode))
