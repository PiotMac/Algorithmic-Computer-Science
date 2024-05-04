import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


# -------------------------------- SELECT VS RANDOM SELECT --------------------------------

"""s_vs_r_data = pd.read_csv("data/select_vs_random.csv", sep=";")
n_values = s_vs_r_data["n"].unique()
algorithms = s_vs_r_data["type"].unique()

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Select vs RandomSelect", fontsize="8")

for alg in algorithms:
    comps = s_vs_r_data["comps"][s_vs_r_data["type"] == alg]
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/select_vs_random/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Select vs RandomSelect", fontsize="8")

for alg in algorithms:
    swaps = s_vs_r_data["swaps"][s_vs_r_data["type"] == alg]
    plt.plot(n_values, swaps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/select_vs_random/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)"""

# -------------------------------- SELECT VS RANDOM SELECT --------------------------------

# -------------------------------- EXPERIMENTAL SELECT --------------------------------

"""experimental_data = pd.read_csv("data/experimental_select.csv", sep=";")
n_values = experimental_data["n"].unique()
algorithms = experimental_data["type"].unique()

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Experimental Select", fontsize="8")

for alg in algorithms:
    comps = experimental_data["comps"][experimental_data["type"] == alg]
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/experimental_select/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Experimental Select", fontsize="8")

for alg in algorithms:
    swaps = experimental_data["swaps"][experimental_data["type"] == alg]
    plt.plot(n_values, swaps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/experimental_select/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time in ms", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Experimental Select", fontsize="8")

for alg in algorithms:
    time = experimental_data["time"][experimental_data["type"] == alg]
    plt.plot(n_values, time, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/experimental_select/time.png", bbox_inches="tight", dpi=500)
plt.close(1)"""

# -------------------------------- EXPERIMENTAL SELECT --------------------------------

# -------------------------------- BINARY SEARCH --------------------------------

"""experimental_data = pd.read_csv("data/binary_search.csv", sep=";")
n_values = experimental_data["n"].unique()
further_n_values = n_values[9:]
algorithms = experimental_data["type"].unique()

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Binary Search", fontsize="8")

for alg in algorithms:
    comps = experimental_data["comps"][experimental_data["type"] == alg]
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/binary_search/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time in ns", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Binary Search", fontsize="8")

for alg in algorithms:
    time = experimental_data["time"][experimental_data["type"] == alg][experimental_data["n"] >= 10000]
    plt.plot(further_n_values, time, label=alg)

plt.legend(loc=1, fontsize="xx-small")
plt.savefig(f"charts/binary_search/time.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons / log\u2082(n)", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Binary Search", fontsize="8")

for alg in algorithms:
    comps = experimental_data["comps"][experimental_data["type"] == alg]
    comps /= np.log2(n_values)
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/binary_search/comparisons_coefficient.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time in ns / log\u2082(n)", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Binary Search", fontsize="8")

for alg in algorithms:
    time = experimental_data["time"][experimental_data["type"] == alg][experimental_data["n"] >= 10000]
    time /= np.log2(further_n_values)
    plt.plot(further_n_values, time, label=alg)

plt.legend(loc=1, fontsize="xx-small")
plt.savefig(f"charts/binary_search/time_coefficient.png", bbox_inches="tight", dpi=500)
plt.close(1)"""

# -------------------------------- BINARY SEARCH --------------------------------

# -------------------------------- QUICK SELECT --------------------------------

"""avg_data = pd.read_csv("data/quickselect_vs_quicksort_average.csv", sep=";")
worst_data = pd.read_csv("data/quickselect_vs_quicksort_worst.csv", sep=";")
n_values = avg_data["n"].unique()
algorithms = avg_data["type"].unique()

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("QuickSelect vs QuickSort in Average Case", fontsize="8")

for alg in algorithms:
    comps = avg_data["comps"][avg_data["type"] == alg]
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/quickselect_vs_quicksort/average/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("QuickSelect vs QuickSort in Average Case", fontsize="8")

for alg in algorithms:
    swaps = avg_data["swaps"][avg_data["type"] == alg]
    plt.plot(n_values, swaps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/quickselect_vs_quicksort/average/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time in ms", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("QuickSelect vs QuickSort in Average Case", fontsize="8")

for alg in algorithms:
    time = avg_data["time"][avg_data["type"] == alg]
    plt.plot(n_values, time, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/quickselect_vs_quicksort/average/time.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("QuickSelect vs QuickSort in Worst Case", fontsize="8")

for alg in algorithms:
    comps = worst_data["comps"][worst_data["type"] == alg]
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/quickselect_vs_quicksort/worst/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("QuickSelect vs QuickSort in Worst Case", fontsize="8")

for alg in algorithms:
    swaps = worst_data["swaps"][worst_data["type"] == alg]
    plt.plot(n_values, swaps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/quickselect_vs_quicksort/worst/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time in ms", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("QuickSelect vs QuickSort in Worst Case", fontsize="8")

for alg in algorithms:
    time = worst_data["time"][worst_data["type"] == alg]
    plt.plot(n_values, time, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/quickselect_vs_quicksort/worst/time.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons / (n * log\u2082(n))", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("QuickSelect vs QuickSort in Average Case", fontsize="8")

for alg in algorithms:
    comps = avg_data["comps"][avg_data["type"] == alg]
    comps /= (n_values * np.log2(n_values))
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/quickselect_vs_quicksort/average/comparisons_coefficient.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons / n\u00B2", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("QuickSelect vs QuickSort in Worst Case", fontsize="8")

for alg in algorithms:
    comps = worst_data["comps"][worst_data["type"] == alg]
    comps /= (n_values * n_values)
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/quickselect_vs_quicksort/worst/comparisons_coefficient.png", bbox_inches="tight", dpi=500)
plt.close(1)"""

# -------------------------------- QUICK SELECT --------------------------------


# -------------------------------- DUAL SELECT --------------------------------

avg_data = pd.read_csv("data/dualselect_vs_dualsort_average.csv", sep=";")
worst_data = pd.read_csv("data/dualselect_vs_dualsort_worst.csv", sep=";")
n_values = avg_data["n"].unique()
algorithms = avg_data["type"].unique()

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("DualSelect vs DualSort in Average Case", fontsize="8")

for alg in algorithms:
    comps = avg_data["comps"][avg_data["type"] == alg]
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/dualselect_vs_dualsort/average/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("DualSelect vs DualSort in Average Case", fontsize="8")

for alg in algorithms:
    swaps = avg_data["swaps"][avg_data["type"] == alg]
    plt.plot(n_values, swaps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/dualselect_vs_dualsort/average/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time in ms", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("DualSelect vs DualSort in Average Case", fontsize="8")

for alg in algorithms:
    time = avg_data["time"][avg_data["type"] == alg]
    plt.plot(n_values, time, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/dualselect_vs_dualsort/average/time.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("DualSelect vs DualSort in Worst Case", fontsize="8")

for alg in algorithms:
    comps = worst_data["comps"][worst_data["type"] == alg]
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/dualselect_vs_dualsort/worst/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("DualSelect vs DualSort in Worst Case", fontsize="8")

for alg in algorithms:
    swaps = worst_data["swaps"][worst_data["type"] == alg]
    plt.plot(n_values, swaps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/dualselect_vs_dualsort/worst/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time in ms", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("DualSelect vs DualSort in Worst Case", fontsize="8")

for alg in algorithms:
    time = worst_data["time"][worst_data["type"] == alg]
    plt.plot(n_values, time, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/dualselect_vs_dualsort/worst/time.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons / (n * log\u2082(n))", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("DualSelect vs DualSort in Average Case", fontsize="8")

for alg in algorithms:
    comps = avg_data["comps"][avg_data["type"] == alg]
    comps /= (n_values * np.log2(n_values))
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/dualselect_vs_dualsort/average/comparisons_coefficient.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons / n\u00B2", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("DualSelect vs DualSort in Worst Case", fontsize="8")

for alg in algorithms:
    comps = worst_data["comps"][worst_data["type"] == alg]
    comps /= (n_values * n_values)
    plt.plot(n_values, comps, label=alg)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/dualselect_vs_dualsort/worst/comparisons_coefficient.png", bbox_inches="tight", dpi=500)
plt.close(1)

# -------------------------------- DUAL SELECT --------------------------------
