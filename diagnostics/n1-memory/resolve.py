import sys, bisect, re, collections
B = sys.argv[1]; label = sys.argv[2]; dumps = sys.argv[3:]
syms = []
for l in open(B + "/shim/nm.txt"):
    p = l.split(None, 2)
    if len(p) == 3: syms.append((int(p[0], 16), p[2].strip()))
addrs = [a for a, _ in syms]
base = None
for l in open(B + "/shim/%s.maps" % label):
    if l.rstrip().endswith("/N0"):
        base = int(l.split("-")[0], 16); break
def short(n):
    m = re.match(r"(silk_\w*?)__(.*)", n)
    if not m: return n[:120]
    head, rest = m.groups()
    parts = re.findall(r"(\d+)_([0-9a-f]+)", rest)
    dec = []
    for ln, hx in parts[1:3]:
        try: dec.append(bytes.fromhex(hx).decode("utf-8", "replace"))
        except Exception: pass
    return (head + " " + " | ".join(dec))[:230]
def load(path):
    agg = collections.Counter(); cnt = collections.Counter()
    for l in open(path):
        a, live, c = l.split(); a = int(a, 16) - base
        i = bisect.bisect_right(addrs, a) - 1
        n = short(syms[i][1]) if i >= 0 else hex(a)
        agg[n] += int(live); cnt[n] += int(c)
    return agg, cnt
loaded = [load(B + "/shim/%s.%s" % (label, d)) for d in dumps]
last, lastc = loaded[-1]; first, _ = loaded[0]
print("total live MB:", [round(sum(a.values())/1e6) for a, _ in loaded])
rows = sorted(last, key=lambda n: -(last[n] - (first.get(n, 0) if len(loaded) > 1 else 0)))[:30]
for n in rows:
    print("%8.0f MB growth %8.0f MB live %10d blocks  %s" % ((last[n]-first.get(n,0))/1e6, last[n]/1e6, lastc[n], n))
