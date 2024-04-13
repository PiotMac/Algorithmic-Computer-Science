import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

data = pd.read_csv("data/thresholds/hybrid_testing.csv", sep=";")
n = data[data["size"] >= 40000]["size"].unique()
thresholds = data["threshold"].unique()

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons / n", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Testing thresholds for HybridSort", fontsize="8")
for threshold in thresholds:
    comps = data["comps"][data["threshold"] == threshold][data["size"] >= 40000]
    plt.plot(n, comps / n, label=threshold)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("data/thresholds/hybrid_thresholds_comps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(2)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps / n", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Testing thresholds for HybridSort", fontsize="8")
for threshold in thresholds:
    swaps = data["swaps"][data["threshold"] == threshold][data["size"] >= 40000]
    plt.plot(n, swaps / n, label=threshold)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("data/thresholds/hybrid_thresholds_swaps.png", bbox_inches="tight", dpi=500)
plt.close(2)