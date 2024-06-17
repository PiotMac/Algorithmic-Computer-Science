import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

prim_vs_kruskal = pd.read_csv("data/primVSkruskal.csv", sep=";")
n_values = prim_vs_kruskal["n"].unique()
prim = prim_vs_kruskal["prim"]
kruskal = prim_vs_kruskal["kruskal"]

smaller_p_vs_k = prim_vs_kruskal[prim_vs_kruskal["n"] <= 500]
smaller_n_values = smaller_p_vs_k["n"]
smaller_prim = smaller_p_vs_k["prim"]
smaller_kruskal = smaller_p_vs_k["kruskal"]

rounds = pd.read_csv("data/rounds.csv", sep=";")
rounds_max = rounds["max"]
rounds_min = rounds["min"]
rounds_avg = rounds["avg"]
rounds_n_values = rounds["n"].unique()

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("#rounds", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title(r"#rounds for n $\in$ {100, 200, ..., 9000}", fontsize="8")

plt.plot(rounds_n_values, rounds_max, label="max")
plt.plot(rounds_n_values, rounds_avg, label="avg")
plt.plot(rounds_n_values, rounds_min, label="min")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig(f"charts/rounds.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(2)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time[s]", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title(r"Average time needed to perform MST algorithms for n $\in$ {100, 200, ..., 9000}", fontsize="8")

plt.plot(n_values, prim, label="prim")
plt.plot(n_values, kruskal, label="kruskal")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/prim_vs_kruskal.png", bbox_inches="tight", dpi=500)
plt.close(2)

plt.figure(3)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time[s]", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title(r"Average time needed to perform MST algorithms for n $\in$ {100, 200, ..., 500}", fontsize="8")

plt.plot(smaller_n_values, smaller_prim, label="prim")
plt.plot(smaller_n_values, smaller_kruskal, label="kruskal")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/smaller_prim_vs_kruskal.png", bbox_inches="tight", dpi=500)
plt.close(3)

plt.figure(4)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time[s]", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title(r"Average time needed to perform Prim algorithm for n $\in$ {100, 200, ..., 9000}", fontsize="8")

plt.plot(n_values, prim, label="prim")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/prim.png", bbox_inches="tight", dpi=500)
plt.close(4)

plt.figure(5)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("time[s]", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title(r"Average time needed to perform Kruskal algorithm for n $\in$ {100, 200, ..., 9000}", fontsize="8")

plt.plot(n_values, kruskal, label="kruskal")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig(f"charts/kruskal.png", bbox_inches="tight", dpi=500)
plt.close(5)
