import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

plt.style.use('_mpl-gallery')

# ---------------START------------------------------DPQS VS QS------------------------START--------------------------
data = pd.read_csv("dual_vs_quick.txt", sep=";")
n = data["n"].unique()

avgComps = data["avg_comparisons"].unique()
avgSwaps = data["avg_swaps"].unique()
constants = data["constant"].unique()
quickAvgComp = avgComps[::2]
dualAvgComp = avgComps[1::2]
quickAvgSwaps = avgSwaps[::2]
dualAvgSwaps = avgSwaps[1::2]
quickConstants = constants[::2]
dualConstants = constants[1::2]


plt.figure(1)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=2)
plt.title("Average # of comparisons in QS and DPQS")
plt.plot(n, quickAvgComp, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, dualAvgComp, "b", linewidth=0.9, label="Dual-Pivot QuickSort")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("dvq_comps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(2)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps in QS and DPQS")
plt.plot(n, quickAvgSwaps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, dualAvgSwaps, "b", linewidth=0.9, label="Dual-Pivot QuickSort")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("dvq_swaps.png", bbox_inches="tight", dpi=500)
plt.close(2)

plt.figure(3)
plt.xlabel("n", fontsize="12")
plt.ylabel("constant", fontsize="12")
plt.title("Approximation of constant in QS and DPQS")
plt.plot(n, quickConstants, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, dualConstants, "b", linewidth=0.9, label="Dual-Pivot QuickSort")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("dvq_constant.png", bbox_inches="tight", dpi=500)
plt.close(3)
# ----------------------END--------------------------DPQS VS QS------------------END---------------------------------

# ------------------START---------------------------HYBRID----------------------START---------------------------------
"""
dataHybrid10 = pd.read_csv("hybrid_all10.txt", sep=";")
n = dataHybrid10["n"].unique()
avgCompsHybrid10 = dataHybrid10["avg_comparisons"].values
avgSwapsHybrid10 = dataHybrid10["avg_swaps"].values

quickAvgComps = avgCompsHybrid10[::3]
insertionAvgComps = avgCompsHybrid10[1::3]
hybridAvgComps = avgCompsHybrid10[2::3]
quickAvgSwaps = avgSwapsHybrid10[::3]
insertionAvgSwaps = avgSwapsHybrid10[1::3]
hybridAvgSwaps = avgSwapsHybrid10[2::3]

plt.figure(32)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.rc('axes', labelsize=2)
plt.title("Average # of comparisons in Hybrid for const = 10")
plt.plot(n, quickAvgComps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, insertionAvgComps, "b", linewidth=0.9, label="InsertionSort")
plt.plot(n, hybridAvgComps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_small_comps10.png", bbox_inches="tight", dpi=500)
plt.close(32)

plt.figure(33)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps in Hybrid for const = 10")
plt.plot(n, quickAvgSwaps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, insertionAvgSwaps, "b", linewidth=0.9, label="InsertionSort")
plt.plot(n, hybridAvgSwaps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_small_swaps10.png", bbox_inches="tight", dpi=500)
plt.close(33)

dataHybrid = pd.read_csv("hybrid_all20.txt", sep=";")
n = dataHybrid["n"].unique()
avgCompsHybrid = dataHybrid["avg_comparisons"].values
avgSwapsHybrid = dataHybrid["avg_swaps"].values

quickAvgComps = avgCompsHybrid[::3]
insertionAvgComps = avgCompsHybrid[1::3]
hybridAvgComps = avgCompsHybrid[2::3]
quickAvgSwaps = avgSwapsHybrid[::3]
insertionAvgSwaps = avgSwapsHybrid[1::3]
hybridAvgSwaps = avgSwapsHybrid[2::3]

plt.figure(34)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.rc('axes', labelsize=2)
plt.title("Average # of comparisons in Hybrid for const = 20")
plt.plot(n, quickAvgComps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, insertionAvgComps, "b", linewidth=0.9, label="InsertionSort")
plt.plot(n, hybridAvgComps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_small_comps20.png", bbox_inches="tight", dpi=500)
plt.close(34)

plt.figure(35)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps in Hybrid for const = 20")
plt.plot(n, quickAvgSwaps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, insertionAvgSwaps, "b", linewidth=0.9, label="InsertionSort")
plt.plot(n, hybridAvgSwaps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_small_swaps20.png", bbox_inches="tight", dpi=500)
plt.close(35)

dataHybrid = pd.read_csv("hybrid_all30.txt", sep=";")
n = dataHybrid["n"].unique()
avgCompsHybrid = dataHybrid["avg_comparisons"].values
avgSwapsHybrid = dataHybrid["avg_swaps"].values

quickAvgComps = avgCompsHybrid[::3]
insertionAvgComps = avgCompsHybrid[1::3]
hybridAvgComps = avgCompsHybrid[2::3]
quickAvgSwaps = avgSwapsHybrid[::3]
insertionAvgSwaps = avgSwapsHybrid[1::3]
hybridAvgSwaps = avgSwapsHybrid[2::3]

plt.figure(36)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.rc('axes', labelsize=2)
plt.title("Average # of comparisons in Hybrid for const = 30")
plt.plot(n, quickAvgComps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, insertionAvgComps, "b", linewidth=0.9, label="InsertionSort")
plt.plot(n, hybridAvgComps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_small_comps30.png", bbox_inches="tight", dpi=500)
plt.close(36)

plt.figure(37)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps in Hybrid for const = 30")
plt.plot(n, quickAvgSwaps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, insertionAvgSwaps, "b", linewidth=0.9, label="InsertionSort")
plt.plot(n, hybridAvgSwaps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_small_swaps30.png", bbox_inches="tight", dpi=500)
plt.close(37)


dataHybrid = pd.read_csv("hybrid_not_all10.txt", sep=";")
n = dataHybrid["n"].unique()
avgCompsHybrid = dataHybrid["avg_comparisons"].values
avgSwapsHybrid = dataHybrid["avg_swaps"].values

quickAvgComps = avgCompsHybrid[::2]
hybridAvgComps = avgCompsHybrid[1::2]
quickAvgSwaps = avgSwapsHybrid[::2]
hybridAvgSwaps = avgSwapsHybrid[1::2]

plt.figure(38)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.rc('axes', labelsize=2)
plt.title("Average # of comparisons in Hybrid for const = 10")
plt.plot(n, quickAvgComps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, hybridAvgComps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_big_comps10.png", bbox_inches="tight", dpi=500)
plt.close(38)

plt.figure(39)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps in Hybrid for const = 10")
plt.plot(n, quickAvgSwaps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, hybridAvgSwaps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_big_swaps10.png", bbox_inches="tight", dpi=500)
plt.close(39)

dataHybrid = pd.read_csv("hybrid_not_all20.txt", sep=";")
n = dataHybrid["n"].unique()
avgCompsHybrid = dataHybrid["avg_comparisons"].values
avgSwapsHybrid = dataHybrid["avg_swaps"].values

quickAvgComps = avgCompsHybrid[::2]
hybridAvgComps = avgCompsHybrid[1::2]
quickAvgSwaps = avgSwapsHybrid[::2]
hybridAvgSwaps = avgSwapsHybrid[1::2]

plt.figure(40)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.rc('axes', labelsize=2)
plt.title("Average # of comparisons in Hybrid for const = 20")
plt.plot(n, quickAvgComps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, hybridAvgComps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_big_comps20.png", bbox_inches="tight", dpi=500)
plt.close(40)

plt.figure(41)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps in Hybrid for const = 20")
plt.plot(n, quickAvgSwaps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, hybridAvgSwaps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_big_swaps20.png", bbox_inches="tight", dpi=500)
plt.close(41)
"""
dataHybrid10 = pd.read_csv("hybrid_not_all10.txt", sep=";")
n = dataHybrid10["n"].unique()
avgCompsHybrid10 = dataHybrid10["avg_comparisons"].values
avgSwapsHybrid10 = dataHybrid10["avg_swaps"].values

hybridAvgComps10 = avgCompsHybrid10[1::2]
hybridAvgSwaps10 = avgSwapsHybrid10[1::2]

dataHybrid20 = pd.read_csv("hybrid_not_all20.txt", sep=";")
avgCompsHybrid20 = dataHybrid20["avg_comparisons"].values
avgSwapsHybrid20 = dataHybrid20["avg_swaps"].values

hybridAvgComps20 = avgCompsHybrid20[1::2]
hybridAvgSwaps20 = avgSwapsHybrid20[1::2]

dataHybrid30 = pd.read_csv("hybrid_not_all30.txt", sep=";")
avgCompsHybrid30 = dataHybrid30["avg_comparisons"].values
avgSwapsHybrid30 = dataHybrid30["avg_swaps"].values

hybridAvgComps30 = avgCompsHybrid30[1::2]
hybridAvgSwaps30 = avgSwapsHybrid30[1::2]

dataHybrid8 = pd.read_csv("hybrid_not_all8.txt", sep=";")
avgCompsHybrid8 = dataHybrid8["avg_comparisons"].values
avgSwapsHybrid8 = dataHybrid8["avg_swaps"].values

hybridAvgComps8 = avgCompsHybrid8[1::2]
hybridAvgSwaps8 = avgSwapsHybrid8[1::2]

dataHybrid5 = pd.read_csv("hybrid_not_all5.txt", sep=";")
avgCompsHybrid5 = dataHybrid5["avg_comparisons"].values
avgSwapsHybrid5 = dataHybrid5["avg_swaps"].values

hybridAvgComps5 = avgCompsHybrid5[1::2]
hybridAvgSwaps5 = avgSwapsHybrid5[1::2]

plt.figure(100)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.rc('axes', labelsize=2)
plt.title("Average # of comparisons in Hybrid for const = 5, 8, 10, 20, 30")
plt.plot(n, hybridAvgComps10, "r", linewidth=0.9, label="const = 10")
plt.plot(n, hybridAvgComps20, "g", linewidth=0.9, label="const = 20")
plt.plot(n, hybridAvgComps30, "b", linewidth=0.9, label="const = 30")
plt.plot(n, hybridAvgComps8, "k", linewidth=0.9, label="const = 8")
plt.plot(n, hybridAvgComps5, "m", linewidth=0.9, label="const = 5")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_optimal_comps.png", bbox_inches="tight", dpi=500)
plt.close(100)

plt.figure(101)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.rc('axes', labelsize=2)
plt.title("Average # of swaps in Hybrid for const = 5, 8, 10, 20, 30")
plt.plot(n, hybridAvgSwaps10, "r", linewidth=0.9, label="const = 10")
plt.plot(n, hybridAvgSwaps20, "g", linewidth=0.9, label="const = 20")
plt.plot(n, hybridAvgSwaps30, "b", linewidth=0.9, label="const = 30")
plt.plot(n, hybridAvgSwaps8, "k", linewidth=0.9, label="const = 8")
plt.plot(n, hybridAvgSwaps5, "m", linewidth=0.9, label="const = 5")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_optimal_swaps.png", bbox_inches="tight", dpi=500)
plt.close(101)

# ---------------------------------------------------------------------------------------------------------
dataHybrid1 = pd.read_csv("hybrid_all5.txt", sep=";")
df = pd.DataFrame(dataHybrid1)

n = dataHybrid1["n"].unique()

avgCompsHybrid1 = dataHybrid1["avg_comparisons"].values
avgSwapsHybrid1 = dataHybrid1["avg_swaps"].values

quickAvgComps = avgCompsHybrid1[::3]
insertionAvgComps = avgCompsHybrid1[1::3]
hybridAvgComps = avgCompsHybrid1[2::3]
quickAvgSwaps = avgSwapsHybrid1[::3]
insertionAvgSwaps = avgSwapsHybrid1[1::3]
hybridAvgSwaps = avgSwapsHybrid1[2::3]

plt.figure(4)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.rc('axes', labelsize=2)
plt.title("Average # of comparisons in QS, IS and Hybrid")
plt.plot(n, quickAvgComps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, insertionAvgComps, "b", linewidth=0.9, label="InsertionSort")
plt.plot(n, hybridAvgComps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_comps.png", bbox_inches="tight", dpi=500)
plt.close(4)

plt.figure(5)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps in QS, IS and Hybrid")
plt.plot(n, quickAvgSwaps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, insertionAvgSwaps, "b", linewidth=0.9, label="InsertionSort")
plt.plot(n, hybridAvgSwaps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_swaps.png", bbox_inches="tight", dpi=500)
plt.close(5)

dataHybrid2 = pd.read_csv("hybrid_not_all5.txt", sep=";")

n = dataHybrid2["n"].unique()

avgCompsHybrid2 = dataHybrid2["avg_comparisons"].values
avgSwapsHybrid2 = dataHybrid2["avg_swaps"].values

quickAvgComps = avgCompsHybrid2[::2]
hybridAvgComps = avgCompsHybrid2[1::2]
quickAvgSwaps = avgSwapsHybrid2[::2]
hybridAvgSwaps = avgSwapsHybrid2[1::2]

plt.figure(6)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.rc('axes', labelsize=2)
plt.title("Average # of comparisons in QS and Hybrid")
plt.plot(n, quickAvgComps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, hybridAvgComps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_qs_comps.png", bbox_inches="tight", dpi=500)
plt.close(6)

plt.figure(7)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps in QS and Hybrid")
plt.plot(n, quickAvgSwaps, "r", linewidth=0.9, label="QuickSort")
plt.plot(n, hybridAvgSwaps, "g", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("hybrid_qs_swaps.png", bbox_inches="tight", dpi=500)
plt.close(7)
# ------------------END---------------------------HYBRID----------------------END---------------------------------

# ------------------START---------------------------EX 2----------------------START-------------------------------
dataAll = pd.read_csv("data_backup.txt", sep=";")
smaller_n = []
bigger_n = []

n = dataAll["n"].unique()
for sizes in n:
    if sizes <= 200:
        smaller_n.append(sizes)
    else:
        bigger_n.append(sizes)
# ------------------START---------------------K = 1---------------------START----------------------------

k1 = dataAll[dataAll["k"] == 1]
insertions1 = k1[k1["type"] == "insertion"]
merges1 = k1[k1["type"] == "merge"]
quicks1 = k1[k1["type"] == "quick"]
duals1 = k1[k1["type"] == "dual"]
hybrids1 = k1[k1["type"] == "hybrid"]
smaller_merges1 = merges1[merges1["n"] <= 200]
smaller_quicks1 = quicks1[quicks1["n"] <= 200]
smaller_duals1 = duals1[duals1["n"] <= 200]
smaller_hybrids1 = hybrids1[hybrids1["n"] <= 200]

comps_insertions1 = insertions1["comps"].values
comps_smaller_merges1 = smaller_merges1["comps"].values
comps_smaller_quicks1 = smaller_quicks1["comps"].values
comps_smaller_duals1 = smaller_duals1["comps"].values
comps_smaller_hybrids1 = smaller_hybrids1["comps"].values

plt.figure(8)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.title("Average # of comparisons for k = 1")

plt.plot(smaller_n, comps_insertions1, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, comps_smaller_merges1, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, comps_smaller_quicks1, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, comps_smaller_duals1, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, comps_smaller_hybrids1, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_comps_small_1.png", bbox_inches="tight", dpi=500)
plt.close(8)

swaps_insertions1 = insertions1["swaps"].values
swaps_smaller_merges1 = smaller_merges1["swaps"].values
swaps_smaller_quicks1 = smaller_quicks1["swaps"].values
swaps_smaller_duals1 = smaller_duals1["swaps"].values
swaps_smaller_hybrids1 = smaller_hybrids1["swaps"].values

plt.figure(9)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps for k = 1")

plt.plot(smaller_n, swaps_insertions1, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, swaps_smaller_merges1, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, swaps_smaller_quicks1, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, swaps_smaller_duals1, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, swaps_smaller_hybrids1, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_swaps_small_1.png", bbox_inches="tight", dpi=500)
plt.close(9)

cn_insertions1 = comps_insertions1 / smaller_n
cn_smaller_merges1 = comps_smaller_merges1 / smaller_n
cn_smaller_quicks1 = comps_smaller_quicks1 / smaller_n
cn_smaller_duals1 = comps_smaller_duals1 / smaller_n
cn_smaller_hybrids1 = comps_smaller_hybrids1 / smaller_n

plt.figure(10)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_cn", fontsize="12")
plt.title("Average # of c/n for k = 1")

plt.plot(smaller_n, cn_insertions1, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, cn_smaller_merges1, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, cn_smaller_quicks1, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, cn_smaller_duals1, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, cn_smaller_hybrids1, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_cn_small_1.png", bbox_inches="tight", dpi=500)
plt.close(10)

sn_insertions1 = swaps_insertions1 / smaller_n
sn_smaller_merges1 = swaps_smaller_merges1 / smaller_n
sn_smaller_quicks1 = swaps_smaller_quicks1 / smaller_n
sn_smaller_duals1 = swaps_smaller_duals1 / smaller_n
sn_smaller_hybrids1 = swaps_smaller_hybrids1 / smaller_n

plt.figure(11)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_sn", fontsize="12")
plt.title("Average # of s/n for k = 1")

plt.plot(smaller_n, sn_insertions1, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, sn_smaller_merges1, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, sn_smaller_quicks1, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, sn_smaller_duals1, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, sn_smaller_hybrids1, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_sn_small_1.png", bbox_inches="tight", dpi=500)
plt.close(11)

bigger_merges1 = merges1[merges1["n"] > 200]
bigger_quicks1 = quicks1[quicks1["n"] > 200]
bigger_duals1 = duals1[duals1["n"] > 200]
bigger_hybrids1 = hybrids1[hybrids1["n"] > 200]

comps_bigger_merges1 = bigger_merges1["comps"].values
comps_bigger_quicks1 = bigger_quicks1["comps"].values
comps_bigger_duals1 = bigger_duals1["comps"].values
comps_bigger_hybrids1 = bigger_hybrids1["comps"].values

plt.figure(12)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.title("Average # of comparisons for k = 1")

plt.plot(bigger_n, comps_bigger_merges1, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, comps_bigger_quicks1, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, comps_bigger_duals1, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, comps_bigger_hybrids1, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_comps_big_1.png", bbox_inches="tight", dpi=500)
plt.close(12)

swaps_bigger_merges1 = bigger_merges1["swaps"].values
swaps_bigger_quicks1 = bigger_quicks1["swaps"].values
swaps_bigger_duals1 = bigger_duals1["swaps"].values
swaps_bigger_hybrids1 = bigger_hybrids1["swaps"].values

plt.figure(13)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps for k = 1")

plt.plot(bigger_n, swaps_bigger_merges1, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, swaps_bigger_quicks1, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, swaps_bigger_duals1, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, swaps_bigger_hybrids1, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_swaps_big_1.png", bbox_inches="tight", dpi=500)
plt.close(13)

cn_bigger_merges1 = comps_bigger_merges1 / bigger_n
cn_bigger_quicks1 = comps_bigger_quicks1 / bigger_n
cn_bigger_duals1 = comps_bigger_duals1 / bigger_n
cn_bigger_hybrids1 = comps_bigger_hybrids1 / bigger_n

plt.figure(14)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_cn", fontsize="12")
plt.title("Average # of c/n for k = 1")

plt.plot(bigger_n, cn_bigger_merges1, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, cn_bigger_quicks1, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, cn_bigger_duals1, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, cn_bigger_hybrids1, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_cn_big_1.png", bbox_inches="tight", dpi=500)
plt.close(14)

sn_bigger_merges1 = swaps_bigger_merges1 / bigger_n
sn_bigger_quicks1 = swaps_bigger_quicks1 / bigger_n
sn_bigger_duals1 = swaps_bigger_duals1 / bigger_n
sn_bigger_hybrids1 = swaps_bigger_hybrids1 / bigger_n

plt.figure(15)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_sn", fontsize="12")
plt.title("Average # of s/n for k = 1")

plt.plot(bigger_n, sn_bigger_merges1, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, sn_bigger_quicks1, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, sn_bigger_duals1, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, sn_bigger_hybrids1, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_sn_big_1.png", bbox_inches="tight", dpi=500)
plt.close(15)
# ------------------END--------------------------K = 1-----------------END---------------------------------

# ------------------START---------------------K = 10---------------------START----------------------------
k10 = dataAll[dataAll["k"] == 10]
insertions10 = k10[k10["type"] == "insertion"]
merges10 = k10[k10["type"] == "merge"]
quicks10 = k10[k10["type"] == "quick"]
duals10 = k10[k10["type"] == "dual"]
hybrids10 = k10[k10["type"] == "hybrid"]
smaller_merges10 = merges10[merges10["n"] <= 200]
smaller_quicks10 = quicks10[quicks10["n"] <= 200]
smaller_duals10 = duals10[duals10["n"] <= 200]
smaller_hybrids10 = hybrids10[hybrids10["n"] <= 200]
bigger_merges10 = merges10[merges10["n"] > 200]
bigger_quicks10 = quicks10[quicks10["n"] > 200]
bigger_duals10 = duals10[duals10["n"] > 200]
bigger_hybrids10 = hybrids10[hybrids10["n"] > 200]

comps_insertions10 = insertions10["comps"].values
comps_smaller_merges10 = smaller_merges10["comps"].values
comps_smaller_quicks10 = smaller_quicks10["comps"].values
comps_smaller_duals10 = smaller_duals10["comps"].values
comps_smaller_hybrids10 = smaller_hybrids10["comps"].values

avg_comps_insertions10 = np.array_split(comps_insertions10, 20)
avg_comps_smaller_merges10 = np.array_split(comps_smaller_merges10, 20)
avg_comps_smaller_quicks10 = np.array_split(comps_smaller_quicks10, 20)
avg_comps_smaller_duals10 = np.array_split(comps_smaller_duals10, 20)
avg_comps_smaller_hybrids10 = np.array_split(comps_smaller_hybrids10, 20)
for indices in range(0, 20):
    avg_comps_insertions10[indices] = sum(avg_comps_insertions10[indices]) / 10.0
    avg_comps_smaller_merges10[indices] = sum(avg_comps_smaller_merges10[indices]) / 10.0
    avg_comps_smaller_quicks10[indices] = sum(avg_comps_smaller_quicks10[indices]) / 10.0
    avg_comps_smaller_duals10[indices] = sum(avg_comps_smaller_duals10[indices]) / 10.0
    avg_comps_smaller_hybrids10[indices] = sum(avg_comps_smaller_hybrids10[indices]) / 10.0

plt.figure(16)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.title("Average # of comparisons for k = 10")

plt.plot(smaller_n, avg_comps_insertions10, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, avg_comps_smaller_merges10, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, avg_comps_smaller_quicks10, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, avg_comps_smaller_duals10, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, avg_comps_smaller_hybrids10, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_comps_small_10.png", bbox_inches="tight", dpi=500)
plt.close(16)

swaps_insertions10 = insertions10["swaps"].values
swaps_smaller_merges10 = smaller_merges10["swaps"].values
swaps_smaller_quicks10 = smaller_quicks10["swaps"].values
swaps_smaller_duals10 = smaller_duals10["swaps"].values
swaps_smaller_hybrids10 = smaller_hybrids10["swaps"].values

avg_swaps_insertions10 = np.array_split(swaps_insertions10, 20)
avg_swaps_smaller_merges10 = np.array_split(swaps_smaller_merges10, 20)
avg_swaps_smaller_quicks10 = np.array_split(swaps_smaller_quicks10, 20)
avg_swaps_smaller_duals10 = np.array_split(swaps_smaller_duals10, 20)
avg_swaps_smaller_hybrids10 = np.array_split(swaps_smaller_hybrids10, 20)
for indices in range(0, 20):
    avg_swaps_insertions10[indices] = sum(avg_swaps_insertions10[indices]) / 10.0
    avg_swaps_smaller_merges10[indices] = sum(avg_swaps_smaller_merges10[indices]) / 10.0
    avg_swaps_smaller_quicks10[indices] = sum(avg_swaps_smaller_quicks10[indices]) / 10.0
    avg_swaps_smaller_duals10[indices] = sum(avg_swaps_smaller_duals10[indices]) / 10.0
    avg_swaps_smaller_hybrids10[indices] = sum(avg_swaps_smaller_hybrids10[indices]) / 10.0

plt.figure(17)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps for k = 10")

plt.plot(smaller_n, avg_swaps_insertions10, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, avg_swaps_smaller_merges10, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, avg_swaps_smaller_quicks10, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, avg_swaps_smaller_duals10, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, avg_swaps_smaller_hybrids10, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_swaps_small_10.png", bbox_inches="tight", dpi=500)
plt.close(17)


cn_insertions10 = avg_comps_insertions10 / np.array(smaller_n)
cn_smaller_merges10 = avg_comps_smaller_merges10 / np.array(smaller_n)
cn_smaller_quicks10 = avg_comps_smaller_quicks10 / np.array(smaller_n)
cn_smaller_duals10 = avg_comps_smaller_duals10 / np.array(smaller_n)
cn_smaller_hybrids10 = avg_comps_smaller_hybrids10 / np.array(smaller_n)

plt.figure(18)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_cn", fontsize="12")
plt.title("Average # of c/n for k = 10")

plt.plot(smaller_n, cn_insertions10, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, cn_smaller_merges10, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, cn_smaller_quicks10, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, cn_smaller_duals10, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, cn_smaller_hybrids10, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_cn_small_10.png", bbox_inches="tight", dpi=500)
plt.close(18)


sn_insertions10 = avg_swaps_insertions10 / np.array(smaller_n)
sn_smaller_merges10 = avg_swaps_smaller_merges10 / np.array(smaller_n)
sn_smaller_quicks10 = avg_swaps_smaller_quicks10 / np.array(smaller_n)
sn_smaller_duals10 = avg_swaps_smaller_duals10 / np.array(smaller_n)
sn_smaller_hybrids10 = avg_swaps_smaller_hybrids10 / np.array(smaller_n)

plt.figure(19)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_sn", fontsize="12")
plt.title("Average # of s/n for k = 10")

plt.plot(smaller_n, sn_insertions10, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, sn_smaller_merges10, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, sn_smaller_quicks10, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, sn_smaller_duals10, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, sn_smaller_hybrids10, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_sn_small_10.png", bbox_inches="tight", dpi=500)
plt.close(19)

comps_bigger_merges10 = bigger_merges10["comps"].values
comps_bigger_quicks10 = bigger_quicks10["comps"].values
comps_bigger_duals10 = bigger_duals10["comps"].values
comps_bigger_hybrids10 = bigger_hybrids10["comps"].values

avg_comps_bigger_merges10 = np.array_split(comps_bigger_merges10, 20)
avg_comps_bigger_quicks10 = np.array_split(comps_bigger_quicks10, 20)
avg_comps_bigger_duals10 = np.array_split(comps_bigger_duals10, 20)
avg_comps_bigger_hybrids10 = np.array_split(comps_bigger_hybrids10, 20)
for indices in range(0, 20):
    avg_comps_bigger_merges10[indices] = sum(avg_comps_bigger_merges10[indices]) / 10.0
    avg_comps_bigger_quicks10[indices] = sum(avg_comps_bigger_quicks10[indices]) / 10.0
    avg_comps_bigger_duals10[indices] = sum(avg_comps_bigger_duals10[indices]) / 10.0
    avg_comps_bigger_hybrids10[indices] = sum(avg_comps_bigger_hybrids10[indices]) / 10.0

plt.figure(20)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.title("Average # of comparisons for k = 10")

plt.plot(bigger_n, avg_comps_bigger_merges10, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, avg_comps_bigger_quicks10, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, avg_comps_bigger_duals10, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, avg_comps_bigger_hybrids10, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_comps_big_10.png", bbox_inches="tight", dpi=500)
plt.close(20)


swaps_bigger_merges10 = bigger_merges10["swaps"].values
swaps_bigger_quicks10 = bigger_quicks10["swaps"].values
swaps_bigger_duals10 = bigger_duals10["swaps"].values
swaps_bigger_hybrids10 = bigger_hybrids10["swaps"].values

avg_swaps_bigger_merges10 = np.array_split(swaps_bigger_merges10, 20)
avg_swaps_bigger_quicks10 = np.array_split(swaps_bigger_quicks10, 20)
avg_swaps_bigger_duals10 = np.array_split(swaps_bigger_duals10, 20)
avg_swaps_bigger_hybrids10 = np.array_split(swaps_bigger_hybrids10, 20)
for indices in range(0, 20):
    avg_swaps_bigger_merges10[indices] = sum(avg_swaps_bigger_merges10[indices]) / 10.0
    avg_swaps_bigger_quicks10[indices] = sum(avg_swaps_bigger_quicks10[indices]) / 10.0
    avg_swaps_bigger_duals10[indices] = sum(avg_swaps_bigger_duals10[indices]) / 10.0
    avg_swaps_bigger_hybrids10[indices] = sum(avg_swaps_bigger_hybrids10[indices]) / 10.0

plt.figure(21)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps for k = 10")

plt.plot(bigger_n, avg_swaps_bigger_merges10, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, avg_swaps_bigger_quicks10, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, avg_swaps_bigger_duals10, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, avg_swaps_bigger_hybrids10, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_swaps_big_10.png", bbox_inches="tight", dpi=500)
plt.close(21)


cn_bigger_merges10 = avg_comps_bigger_merges10 / np.array(bigger_n)
cn_bigger_quicks10 = avg_comps_bigger_quicks10 / np.array(bigger_n)
cn_bigger_duals10 = avg_comps_bigger_duals10 / np.array(bigger_n)
cn_bigger_hybrids10 = avg_comps_bigger_hybrids10 / np.array(bigger_n)

plt.figure(22)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_cn", fontsize="12")
plt.title("Average # of c/n for k = 10")

plt.plot(bigger_n, cn_bigger_merges10, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, cn_bigger_quicks10, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, cn_bigger_duals10, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, cn_bigger_hybrids10, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_cn_big_10.png", bbox_inches="tight", dpi=500)
plt.close(22)


sn_bigger_merges10 = avg_swaps_bigger_merges10 / np.array(bigger_n)
sn_bigger_quicks10 = avg_swaps_bigger_quicks10 / np.array(bigger_n)
sn_bigger_duals10 = avg_swaps_bigger_duals10 / np.array(bigger_n)
sn_bigger_hybrids10 = avg_swaps_bigger_hybrids10 / np.array(bigger_n)

plt.figure(23)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_sn", fontsize="12")
plt.title("Average # of s/n for k = 10")

plt.plot(bigger_n, sn_bigger_merges10, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, sn_bigger_quicks10, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, sn_bigger_duals10, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, sn_bigger_hybrids10, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_sn_big_10.png", bbox_inches="tight", dpi=500)
plt.close(23)
# ------------------END--------------------------K = 10-----------------END---------------------------------

# ------------------START---------------------K = 100---------------------START----------------------------
k100 = dataAll[dataAll["k"] == 100]
insertions100 = k100[k100["type"] == "insertion"]
merges100 = k100[k100["type"] == "merge"]
quicks100 = k100[k100["type"] == "quick"]
duals100 = k100[k100["type"] == "dual"]
hybrids100 = k100[k100["type"] == "hybrid"]
smaller_merges100 = merges100[merges100["n"] <= 200]
smaller_quicks100 = quicks100[quicks100["n"] <= 200]
smaller_duals100 = duals100[duals100["n"] <= 200]
smaller_hybrids100 = hybrids100[hybrids100["n"] <= 200]
bigger_merges100 = merges100[merges100["n"] > 200]
bigger_quicks100 = quicks100[quicks100["n"] > 200]
bigger_duals100 = duals100[duals100["n"] > 200]
bigger_hybrids100 = hybrids100[hybrids100["n"] > 200]

comps_insertions100 = insertions100["comps"].values
comps_smaller_merges100 = smaller_merges100["comps"].values
comps_smaller_quicks100 = smaller_quicks100["comps"].values
comps_smaller_duals100 = smaller_duals100["comps"].values
comps_smaller_hybrids100 = smaller_hybrids100["comps"].values

avg_comps_insertions100 = np.array_split(comps_insertions100, 20)
avg_comps_smaller_merges100 = np.array_split(comps_smaller_merges100, 20)
avg_comps_smaller_quicks100 = np.array_split(comps_smaller_quicks100, 20)
avg_comps_smaller_duals100 = np.array_split(comps_smaller_duals100, 20)
avg_comps_smaller_hybrids100 = np.array_split(comps_smaller_hybrids100, 20)
for indices in range(0, 20):
    avg_comps_insertions100[indices] = sum(avg_comps_insertions100[indices]) / 100.0
    avg_comps_smaller_merges100[indices] = sum(avg_comps_smaller_merges100[indices]) / 100.0
    avg_comps_smaller_quicks100[indices] = sum(avg_comps_smaller_quicks100[indices]) / 100.0
    avg_comps_smaller_duals100[indices] = sum(avg_comps_smaller_duals100[indices]) / 100.0
    avg_comps_smaller_hybrids100[indices] = sum(avg_comps_smaller_hybrids100[indices]) / 100.0

plt.figure(24)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.title("Average # of comparisons for k = 100")

plt.plot(smaller_n, avg_comps_insertions100, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, avg_comps_smaller_merges100, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, avg_comps_smaller_quicks100, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, avg_comps_smaller_duals100, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, avg_comps_smaller_hybrids100, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_comps_small_100.png", bbox_inches="tight", dpi=500)
plt.close(24)


swaps_insertions100 = insertions100["swaps"].values
swaps_smaller_merges100 = smaller_merges100["swaps"].values
swaps_smaller_quicks100 = smaller_quicks100["swaps"].values
swaps_smaller_duals100 = smaller_duals100["swaps"].values
swaps_smaller_hybrids100 = smaller_hybrids100["swaps"].values

avg_swaps_insertions100 = np.array_split(swaps_insertions100, 20)
avg_swaps_smaller_merges100 = np.array_split(swaps_smaller_merges100, 20)
avg_swaps_smaller_quicks100 = np.array_split(swaps_smaller_quicks100, 20)
avg_swaps_smaller_duals100 = np.array_split(swaps_smaller_duals100, 20)
avg_swaps_smaller_hybrids100 = np.array_split(swaps_smaller_hybrids100, 20)
for indices in range(0, 20):
    avg_swaps_insertions100[indices] = sum(avg_swaps_insertions100[indices]) / 100.0
    avg_swaps_smaller_merges100[indices] = sum(avg_swaps_smaller_merges100[indices]) / 100.0
    avg_swaps_smaller_quicks100[indices] = sum(avg_swaps_smaller_quicks100[indices]) / 100.0
    avg_swaps_smaller_duals100[indices] = sum(avg_swaps_smaller_duals100[indices]) / 100.0
    avg_swaps_smaller_hybrids100[indices] = sum(avg_swaps_smaller_hybrids100[indices]) / 100.0

plt.figure(25)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps for k = 100")

plt.plot(smaller_n, avg_swaps_insertions100, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, avg_swaps_smaller_merges100, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, avg_swaps_smaller_quicks100, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, avg_swaps_smaller_duals100, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, avg_swaps_smaller_hybrids100, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_swaps_small_100.png", bbox_inches="tight", dpi=500)
plt.close(25)


cn_insertions100 = avg_comps_insertions100 / np.array(smaller_n)
cn_smaller_merges100 = avg_comps_smaller_merges100 / np.array(smaller_n)
cn_smaller_quicks100 = avg_comps_smaller_quicks100 / np.array(smaller_n)
cn_smaller_duals100 = avg_comps_smaller_duals100 / np.array(smaller_n)
cn_smaller_hybrids100 = avg_comps_smaller_hybrids100 / np.array(smaller_n)

plt.figure(26)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_cn", fontsize="12")
plt.title("Average # of c/n for k = 100")

plt.plot(smaller_n, cn_insertions100, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, cn_smaller_merges100, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, cn_smaller_quicks100, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, cn_smaller_duals100, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, cn_smaller_hybrids100, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_cn_small_100.png", bbox_inches="tight", dpi=500)
plt.close(26)


sn_insertions100 = avg_swaps_insertions100 / np.array(smaller_n)
sn_smaller_merges100 = avg_swaps_smaller_merges100 / np.array(smaller_n)
sn_smaller_quicks100 = avg_swaps_smaller_quicks100 / np.array(smaller_n)
sn_smaller_duals100 = avg_swaps_smaller_duals100 / np.array(smaller_n)
sn_smaller_hybrids100 = avg_swaps_smaller_hybrids100 / np.array(smaller_n)

plt.figure(27)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_sn", fontsize="12")
plt.title("Average # of s/n for k = 100")

plt.plot(smaller_n, sn_insertions100, "r", linewidth=0.9, label="InsertionSort")
plt.plot(smaller_n, sn_smaller_merges100, "b", linewidth=0.9, label="MergeSort")
plt.plot(smaller_n, sn_smaller_quicks100, "g", linewidth=0.9, label="QuickSort")
plt.plot(smaller_n, sn_smaller_duals100, "y", linewidth=0.9, label="DPQS")
plt.plot(smaller_n, sn_smaller_hybrids100, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_sn_small_100.png", bbox_inches="tight", dpi=500)
plt.close(27)


comps_bigger_merges100 = bigger_merges100["comps"].values
comps_bigger_quicks100 = bigger_quicks100["comps"].values
comps_bigger_duals100 = bigger_duals100["comps"].values
comps_bigger_hybrids100 = bigger_hybrids100["comps"].values

avg_comps_bigger_merges100 = np.array_split(comps_bigger_merges100, 20)
avg_comps_bigger_quicks100 = np.array_split(comps_bigger_quicks100, 20)
avg_comps_bigger_duals100 = np.array_split(comps_bigger_duals100, 20)
avg_comps_bigger_hybrids100 = np.array_split(comps_bigger_hybrids100, 20)
for indices in range(0, 20):
    avg_comps_bigger_merges100[indices] = sum(avg_comps_bigger_merges100[indices]) / 100.0
    avg_comps_bigger_quicks100[indices] = sum(avg_comps_bigger_quicks100[indices]) / 100.0
    avg_comps_bigger_duals100[indices] = sum(avg_comps_bigger_duals100[indices]) / 100.0
    avg_comps_bigger_hybrids100[indices] = sum(avg_comps_bigger_hybrids100[indices]) / 100.0

plt.figure(28)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_comparisons", fontsize="12")
plt.title("Average # of comparisons for k = 100")

plt.plot(bigger_n, avg_comps_bigger_merges100, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, avg_comps_bigger_quicks100, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, avg_comps_bigger_duals100, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, avg_comps_bigger_hybrids100, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_comps_big_100.png", bbox_inches="tight", dpi=500)
plt.close(28)


swaps_bigger_merges100 = bigger_merges100["swaps"].values
swaps_bigger_quicks100 = bigger_quicks100["swaps"].values
swaps_bigger_duals100 = bigger_duals100["swaps"].values
swaps_bigger_hybrids100 = bigger_hybrids100["swaps"].values

avg_swaps_bigger_merges100 = np.array_split(swaps_bigger_merges100, 20)
avg_swaps_bigger_quicks100 = np.array_split(swaps_bigger_quicks100, 20)
avg_swaps_bigger_duals100 = np.array_split(swaps_bigger_duals100, 20)
avg_swaps_bigger_hybrids100 = np.array_split(swaps_bigger_hybrids100, 20)
for indices in range(0, 20):
    avg_swaps_bigger_merges100[indices] = sum(avg_swaps_bigger_merges100[indices]) / 100.0
    avg_swaps_bigger_quicks100[indices] = sum(avg_swaps_bigger_quicks100[indices]) / 100.0
    avg_swaps_bigger_duals100[indices] = sum(avg_swaps_bigger_duals100[indices]) / 100.0
    avg_swaps_bigger_hybrids100[indices] = sum(avg_swaps_bigger_hybrids100[indices]) / 100.0

plt.figure(29)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_swaps", fontsize="12")
plt.title("Average # of swaps for k = 100")

plt.plot(bigger_n, avg_swaps_bigger_merges100, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, avg_swaps_bigger_quicks100, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, avg_swaps_bigger_duals100, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, avg_swaps_bigger_hybrids100, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_swaps_big_100.png", bbox_inches="tight", dpi=500)
plt.close(29)


cn_bigger_merges100 = avg_comps_bigger_merges100 / np.array(bigger_n)
cn_bigger_quicks100 = avg_comps_bigger_quicks100 / np.array(bigger_n)
cn_bigger_duals100 = avg_comps_bigger_duals100 / np.array(bigger_n)
cn_bigger_hybrids100 = avg_comps_bigger_hybrids100 / np.array(bigger_n)

plt.figure(30)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_cn", fontsize="12")
plt.title("Average # of c/n for k = 100")

plt.plot(bigger_n, cn_bigger_merges100, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, cn_bigger_quicks100, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, cn_bigger_duals100, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, cn_bigger_hybrids100, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_cn_big_100.png", bbox_inches="tight", dpi=500)
plt.close(30)


sn_bigger_merges100 = avg_swaps_bigger_merges100 / np.array(bigger_n)
sn_bigger_quicks100 = avg_swaps_bigger_quicks100 / np.array(bigger_n)
sn_bigger_duals100 = avg_swaps_bigger_duals100 / np.array(bigger_n)
sn_bigger_hybrids100 = avg_swaps_bigger_hybrids100 / np.array(bigger_n)

plt.figure(31)
plt.xlabel("n", fontsize="12")
plt.ylabel("avg_sn", fontsize="12")
plt.title("Average # of s/n for k = 100")

plt.plot(bigger_n, sn_bigger_merges100, "b", linewidth=0.9, label="MergeSort")
plt.plot(bigger_n, sn_bigger_quicks100, "g", linewidth=0.9, label="QuickSort")
plt.plot(bigger_n, sn_bigger_duals100, "y", linewidth=0.9, label="DPQS")
plt.plot(bigger_n, sn_bigger_hybrids100, "k", linewidth=0.9, label="HybridSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("all_sn_big_100.png", bbox_inches="tight", dpi=500)
plt.close(31)
# ------------------END--------------------------K = 100-----------------END---------------------------------

# ------------------END---------------------------EX 2----------------------END---------------------------------
