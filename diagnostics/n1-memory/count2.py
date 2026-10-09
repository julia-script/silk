import gdb, os, time
gdb.execute("set pagination off"); gdb.execute("set confirm off")
gdb.execute("rbreak ^silk_semantic_Semantic_storeMir", to_string=True)
gdb.execute("rbreak ^silk_semantic_Semantic_Semantic_buildFrom_effect", to_string=True)
gdb.execute("rbreak ^silk_semantic_Semantic_Semantic_revise", to_string=True)
out = open(os.environ["COUNT_OUT"], "w")
limit = int(os.environ.get("LIMIT_KB", "10000000")); every = int(os.environ.get("EVERY", "250"))
n = 0; passes = 0; revs = 0; t0 = time.time()
def rss():
    pid = gdb.selected_inferior().pid
    for l in open("/proc/%d/status" % pid):
        if l.startswith("VmRSS"): return int(l.split()[1])
    return 0
gdb.execute("run", to_string=True)
while True:
    try:
        if not gdb.selected_inferior().pid: break
        f = gdb.selected_frame().name() or ""
        if "storeMir" in f:
            n += 1
            if n % every == 0:
                r = rss(); out.write("mir=%d pass=%d rev=%d rss=%d t=%d\n" % (n, passes, revs, r, int(time.time()-t0))); out.flush()
                if r > limit:
                    out.write("KILLED\n"); out.flush(); gdb.execute("kill"); break
        elif "buildFrom" in f:
            passes += 1
            out.write("PASS %d mir=%d rss=%d t=%d\n" % (passes, n, rss(), int(time.time()-t0))); out.flush()
        elif "revise" in f:
            revs += 1
        gdb.execute("continue", to_string=True)
    except gdb.error as e:
        out.write("ERR %s\n" % e); break
out.write("END mir=%d passes=%d revs=%d\n" % (n, passes, revs)); out.close()
gdb.execute("quit")
