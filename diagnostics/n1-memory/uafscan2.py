import gdb, struct
gdb.execute("set pagination off"); gdb.execute("set confirm off")
gdb.execute("run", to_string=True)
inf = gdb.selected_inferior()
FREED = 0xdeaddeaddeaddead
def sym(a):
    try: return gdb.execute("info symbol 0x%x" % a, to_string=True).split(" in section")[0].strip()
    except Exception: return hex(a)
def header(p):
    try: return struct.unpack("<16Q", bytes(inf.read_memory(p - 128, 128)))
    except Exception: return None
gdb.execute("frame 1", to_string=True)
lo = int(gdb.parse_and_eval("$sp")); gdb.execute("frame 3", to_string=True); hi = int(gdb.parse_and_eval("$sp")) + 4096
data = bytes(inf.read_memory(lo, hi - lo))
found = {}
for i in range(0, len(data), 8):
    v = struct.unpack_from("<Q", data, i)[0]
    if v < 0x10000: continue
    h = header(v)
    if h and h[0] == FREED and v not in found:
        acc = struct.unpack("<Q", bytes(inf.read_memory(v + 8, 8)))[0]
        found[v] = (lo + i, h, acc)
for v, (where, h, acc) in found.items():
    print("FREED block 0x%x (slot 0x%x) size=%d access=0x%x" % (v, where, h[3], acc))
    print("   alloc:", sym(h[1]))
    for r in h[4:]:
        if r: print("   free-stack:", sym(r))
gdb.execute("kill")
