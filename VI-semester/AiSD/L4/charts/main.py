import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# -------------------------------- BINARY SEARCH TREE --------------------------------

"""bst_data = pd.read_csv("data/bst.csv", sep=";")
n_values = bst_data["n"].unique()
avg_worst = ["avg", "worst"]
max_avg_worst = ["max_avg", "max_worst"]
all = ["avg", "max_avg", "worst", "max_worst"]

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#comparisons in BST", fontsize="8")

for item in all:
    comps = bst_data["comps"][bst_data["type"] == item]
    plt.semilogy(n_values, comps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/bst/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#swaps in BST", fontsize="8")

for item in all:
    swaps = bst_data["swaps"][bst_data["type"] == item]
    plt.semilogy(n_values, swaps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/bst/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("height in BST", fontsize="8")

for item in all:
    height = bst_data["height"][bst_data["type"] == item]
    plt.semilogy(n_values, height, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/bst/height.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average #comparisons in worst- and average-case in BST", fontsize="8")

for item in avg_worst:
    comps = bst_data["comps"][bst_data["type"] == item]
    plt.semilogy(n_values, comps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/bst/avg/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average #swaps in worst- and average-case in BST", fontsize="8")

for item in avg_worst:
    swaps = bst_data["swaps"][bst_data["type"] == item]
    plt.semilogy(n_values, swaps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/bst/avg/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average height in worst- and average-case in BST", fontsize="8")

for item in avg_worst:
    height = bst_data["height"][bst_data["type"] == item]
    plt.semilogy(n_values, height, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/bst/avg/height.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Max #comparisons in worst- and average-case in BST", fontsize="8")

for item in max_avg_worst:
    comps = bst_data["comps"][bst_data["type"] == item]
    plt.semilogy(n_values, comps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/bst/max/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Max #swaps in worst- and average-case in BST", fontsize="8")

for item in max_avg_worst:
    swaps = bst_data["swaps"][bst_data["type"] == item]
    plt.semilogy(n_values, swaps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/bst/max/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Max height in worst- and average-case in BST", fontsize="8")

for item in max_avg_worst:
    height = bst_data["height"][bst_data["type"] == item]
    plt.semilogy(n_values, height, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/bst/max/height.png", bbox_inches="tight", dpi=500)
plt.close(1)"""

# -------------------------------- BINARY SEARCH TREE --------------------------------

# -------------------------------- RED-BLACK TREE --------------------------------

rbt_data = pd.read_csv("data/rbt.csv", sep=";")
n_values = rbt_data["n"].unique()
avg_worst = ["avg", "worst"]
max_avg_worst = ["max_avg", "max_worst"]
all = ["avg", "max_avg", "worst", "max_worst"]

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#comparisons in RBT", fontsize="8")

for item in all:
    comps = rbt_data["comps"][rbt_data["type"] == item]
    plt.plot(n_values, comps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rbt/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#swaps in RBT", fontsize="8")

for item in all:
    swaps = rbt_data["swaps"][rbt_data["type"] == item]
    plt.plot(n_values, swaps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rbt/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("height in RBT", fontsize="8")

for item in all:
    height = rbt_data["height"][rbt_data["type"] == item]
    plt.plot(n_values, height, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rbt/height.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average #comparisons in worst- and average-case in RBT", fontsize="8")

for item in avg_worst:
    comps = rbt_data["comps"][rbt_data["type"] == item]
    plt.plot(n_values, comps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rbt/avg/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average #swaps in worst- and average-case in RBT", fontsize="8")

for item in avg_worst:
    swaps = rbt_data["swaps"][rbt_data["type"] == item]
    plt.plot(n_values, swaps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rbt/avg/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average height in worst- and average-case in RBT", fontsize="8")

for item in avg_worst:
    height = rbt_data["height"][rbt_data["type"] == item]
    plt.plot(n_values, height, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rbt/avg/height.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Max #comparisons in worst- and average-case in RBT", fontsize="8")

for item in max_avg_worst:
    comps = rbt_data["comps"][rbt_data["type"] == item]
    plt.plot(n_values, comps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rbt/max/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Max #swaps in worst- and average-case in RBT", fontsize="8")

for item in max_avg_worst:
    swaps = rbt_data["swaps"][rbt_data["type"] == item]
    plt.plot(n_values, swaps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rbt/max/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Max height in worst- and average-case in RBT", fontsize="8")

for item in max_avg_worst:
    height = rbt_data["height"][rbt_data["type"] == item]
    plt.plot(n_values, height, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rbt/max/height.png", bbox_inches="tight", dpi=500)
plt.close(1)

# -------------------------------- RED-BLACK TREE --------------------------------

# -------------------------------- SPLAY TREE --------------------------------

"""splay_data = pd.read_csv("data/splay.csv", sep=";")
n_values = splay_data["n"].unique()
avg_worst = ["avg", "worst"]
max_avg_worst = ["max_avg", "max_worst"]

all = ["avg", "max_avg", "worst", "max_worst"]

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#comparisons in Splay Tree", fontsize="8")

for item in all:
    comps = splay_data["comps"][splay_data["type"] == item]
    plt.plot(n_values, comps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/splay/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#swaps in Splay Tree", fontsize="8")

for item in all:
    swaps = splay_data["swaps"][splay_data["type"] == item]
    plt.plot(n_values, swaps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/splay/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("height in Splay Tree", fontsize="8")

for item in all:
    height = splay_data["height"][splay_data["type"] == item]
    plt.plot(n_values, height, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/splay/height.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average #comparisons in worst- and average-case in Splay Tree", fontsize="8")

for item in avg_worst:
    comps = splay_data["comps"][splay_data["type"] == item]
    plt.plot(n_values, comps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/splay/avg/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average #swaps in worst- and average-case in Splay Tree", fontsize="8")

for item in avg_worst:
    swaps = splay_data["swaps"][splay_data["type"] == item]
    plt.plot(n_values, swaps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/splay/avg/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average height in worst- and average-case in Splay Tree", fontsize="8")

for item in avg_worst:
    height = splay_data["height"][splay_data["type"] == item]
    plt.plot(n_values, height, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/splay/avg/height.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Max #comparisons in worst- and average-case in Splay Tree", fontsize="8")

for item in max_avg_worst:
    comps = splay_data["comps"][splay_data["type"] == item]
    plt.plot(n_values, comps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/splay/max/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Max #swaps in worst- and average-case in Splay Tree", fontsize="8")

for item in max_avg_worst:
    swaps = splay_data["swaps"][splay_data["type"] == item]
    plt.plot(n_values, swaps, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/splay/max/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Max height in worst- and average-case in Splay Tree", fontsize="8")

for item in max_avg_worst:
    height = splay_data["height"][splay_data["type"] == item]
    plt.plot(n_values, height, label=item)

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/splay/max/height.png", bbox_inches="tight", dpi=500)
plt.close(1)"""

# -------------------------------- SPLAY TREE --------------------------------

# -------------------------------- ALL --------------------------------

"""bst_data = pd.read_csv("data/bst.csv", sep=";")
rbt_data = pd.read_csv("data/rbt.csv", sep=";")
splay_data = pd.read_csv("data/splay.csv", sep=";")
n_values = splay_data["n"].unique()
avg_worst = ["avg", "worst"]
max_avg_worst = ["max_avg", "max_worst"]

avg = ["avg", "max_avg"]
worst = ["worst", "max_worst"]

tree_data = [bst_data, rbt_data, splay_data]
names = ["BST", "RBT", "SplayTree"]
all = ["avg", "max_avg", "worst", "max_worst"]

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#comparisons in all trees in average-case", fontsize="8")

for i in range(len(tree_data)):
    for item in avg:
        comps = tree_data[i]["comps"][tree_data[i]["type"] == item]
        plt.plot(n_values, comps, label=item + names[i])

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/all/avg_case/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#swaps in all trees in average-case", fontsize="8")

for i in range(len(tree_data)):
    for item in avg:
        swaps = tree_data[i]["swaps"][tree_data[i]["type"] == item]
        plt.plot(n_values, swaps, label=item + names[i])

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/all/avg_case/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("height in all trees in average-case", fontsize="8")

for i in range(len(tree_data)):
    for item in avg:
        height = tree_data[i]["height"][tree_data[i]["type"] == item]
        plt.plot(n_values, height, label=item + names[i])

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/all/avg_case/height.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#comparisons in all trees in worst-case", fontsize="8")

for i in range(len(tree_data)):
    for item in worst:
        comps = tree_data[i]["comps"][tree_data[i]["type"] == item]
        plt.semilogy(n_values, comps, label=item + names[i])

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/all/worst_case/comparisons.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("#swaps in all trees in worst-case", fontsize="8")

for i in range(len(tree_data)):
    for item in worst:
        swaps = tree_data[i]["swaps"][tree_data[i]["type"] == item]
        plt.semilogy(n_values, swaps, label=item + names[i])

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/all/worst_case/swaps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("height", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("height in all trees in worst-case", fontsize="8")

for i in range(len(tree_data)):
    for item in worst:
        height = tree_data[i]["height"][tree_data[i]["type"] == item]
        plt.semilogy(n_values, height, label=item + names[i])

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/all/worst_case/height.png", bbox_inches="tight", dpi=500)
plt.close(1)"""

# -------------------------------- ALL --------------------------------
