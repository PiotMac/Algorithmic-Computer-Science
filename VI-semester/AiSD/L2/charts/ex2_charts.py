import math

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

smaller_data = pd.read_csv("data/smaller_data/smaller_data.csv", sep=";")
# bigger_data = pd.read_csv("data/bigger_data/bigger_data.csv", sep=";")
bigger_data = pd.read_csv("data/bigger_data/mvp.csv", sep=";")
smaller_n = smaller_data["n"].unique()
bigger_n = bigger_data["n"].unique()
ks = smaller_data["k"].unique()
algorithms = smaller_data["type"].unique()
bigger_algorithms = bigger_data["type"].unique()

"""for k in ks:
    plt.figure(1)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#comparisons", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#comparisons in the algorithms", fontsize="8")

    for alg in algorithms:
        comps = smaller_data["comps"][smaller_data["type"] == alg][smaller_data["k"] == k]
        plt.plot(smaller_n, comps, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/smaller_data/smaller_comps{k}.png", bbox_inches="tight", dpi=500)
    plt.close(1)

    plt.figure(2)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#swaps", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#swaps in the algorithms", fontsize="8")

    for alg in algorithms:
        swaps = smaller_data["swaps"][smaller_data["type"] == alg][smaller_data["k"] == k]
        plt.plot(smaller_n, swaps, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/smaller_data/smaller_swaps{k}.png", bbox_inches="tight", dpi=500)
    plt.close(2)

    plt.figure(3)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#comparisons / n", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#comparisons / n in the algorithms", fontsize="8")

    for alg in algorithms:
        comps = smaller_data["comps"][smaller_data["type"] == alg][smaller_data["k"] == k]
        plt.plot(smaller_n, comps / smaller_n, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/smaller_data/smaller_comps_n{k}.png", bbox_inches="tight", dpi=500)
    plt.close(3)

    plt.figure(4)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#swaps / n", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#swaps / n in the algorithms", fontsize="8")

    for alg in algorithms:
        swaps = smaller_data["swaps"][smaller_data["type"] == alg][smaller_data["k"] == k]
        plt.plot(smaller_n, swaps / smaller_n, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/smaller_data/smaller_swaps_n{k}.png", bbox_inches="tight", dpi=500)
    plt.close(4)

for k in ks:
    plt.figure(1)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#comparisons", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#comparisons in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        if alg == "quick" or alg == "dual":
            comps = bigger_data["comps"][bigger_data["type"] == alg][bigger_data["k"] == k]
            plt.plot(bigger_n, comps, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_comps{k}.png", bbox_inches="tight", dpi=500)
    plt.close(1)

    plt.figure(2)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#swaps", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#swaps in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        if alg == "quick" or alg == "dual":
            swaps = bigger_data["swaps"][bigger_data["type"] == alg][bigger_data["k"] == k]
            plt.plot(bigger_n, swaps, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_swaps_qvd_constant{k}.png", bbox_inches="tight", dpi=500)
    plt.close(2)

    plt.figure(3)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#comparisons / n", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#comparisons / (1.8 * n * log(n)) in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        if alg == "quick" or alg == "dual":
            comps = bigger_data["comps"][bigger_data["type"] == alg][bigger_data["k"] == k]
            plt.plot(bigger_n, comps / (1.8 * bigger_n * np.log(bigger_n)), label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_comps_qvd_constant_nln{k}.png", bbox_inches="tight", dpi=500)
    plt.close(3)

    plt.figure(4)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#swaps / n", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#swaps / (1.8 * n * log(n))  in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        if alg == "quick" or alg == "dual":
            swaps = bigger_data["swaps"][bigger_data["type"] == alg][bigger_data["k"] == k]
            plt.plot(bigger_n, swaps / (1.8 * bigger_n * np.log(bigger_n)), label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_swaps_qvd_constant_nln{k}.png", bbox_inches="tight", dpi=500)
    plt.close(4)"""
"""
for k in ks:
    plt.figure(1)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#comparisons", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#comparisons in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        comps = bigger_data["comps"][bigger_data["type"] == alg][bigger_data["k"] == k]
        plt.plot(bigger_n, comps, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_comps{k}.png", bbox_inches="tight", dpi=500)
    plt.close(1)

    plt.figure(2)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#swaps", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#swaps in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        swaps = bigger_data["swaps"][bigger_data["type"] == alg][bigger_data["k"] == k]
        plt.plot(bigger_n, swaps, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_swaps{k}.png", bbox_inches="tight", dpi=500)
    plt.close(2)

    plt.figure(3)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#comparisons / n", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#comparisons / n in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        comps = bigger_data["comps"][bigger_data["type"] == alg][bigger_data["k"] == k]
        plt.plot(bigger_n, comps / (1.8 * bigger_n * np.log(bigger_n)), label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_comps_n{k}.png", bbox_inches="tight", dpi=500)
    plt.close(3)

    plt.figure(4)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#swaps / n", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#swaps / n  in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        swaps = bigger_data["swaps"][bigger_data["type"] == alg][bigger_data["k"] == k]
        plt.plot(bigger_n, swaps / (1.8 * bigger_n * np.log(bigger_n)), label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_swaps_n{k}.png", bbox_inches="tight", dpi=500)
    plt.close(4)"""

for k in ks:
    plt.figure(1)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#comparisons", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#comparisons in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        if alg == "merge" or alg == "my":
            comps = bigger_data["comps"][bigger_data["type"] == alg][bigger_data["k"] == k]
            plt.plot(bigger_n, comps, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_comps_mvp{k}.png", bbox_inches="tight", dpi=500)
    plt.close(1)

    plt.figure(2)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#swaps", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#swaps in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        if alg == "merge" or alg == "my":
            swaps = bigger_data["swaps"][bigger_data["type"] == alg][bigger_data["k"] == k]
            plt.plot(bigger_n, swaps, label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_swaps_mvp{k}.png", bbox_inches="tight", dpi=500)
    plt.close(2)

    plt.figure(3)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#comparisons / n", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#comparisons / n in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        if alg == "merge" or alg == "my":
            comps = bigger_data["comps"][bigger_data["type"] == alg][bigger_data["k"] == k]
            plt.plot(bigger_n, comps / (1.8 * bigger_n * np.log(bigger_n)), label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_comps_n_mvp{k}.png", bbox_inches="tight", dpi=500)
    plt.close(3)

    plt.figure(4)
    plt.rc('xtick', labelsize=5)
    plt.rc('ytick', labelsize=5)
    plt.xlabel("n", fontsize="10")
    plt.ylabel("#swaps / n", fontsize="10")
    plt.rc('legend', fontsize=2)
    plt.rc('axes', labelsize=1)
    plt.title("#swaps / n  in the algorithms", fontsize="8")

    for alg in bigger_algorithms:
        if alg == "merge" or alg == "my":
            swaps = bigger_data["swaps"][bigger_data["type"] == alg][bigger_data["k"] == k]
            plt.plot(bigger_n, swaps / (1.8 * bigger_n * np.log(bigger_n)), label=alg)

    plt.legend(loc=4, fontsize="xx-small")
    plt.savefig(f"data/bigger_data/bigger_swaps_n_mvp{k}.png", bbox_inches="tight", dpi=500)
    plt.close(4)