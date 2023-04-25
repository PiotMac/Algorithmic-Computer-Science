import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

plt.style.use('_mpl-gallery')

data = pd.read_csv("MD.txt", sep=";")

MDdepth = data["depth"].values
MDdepth_avg = sum(MDdepth) / 50
infoo = []
for i in range(0, 50):
    infoo.append(MDdepth_avg)

MDvisited = data["visited"].values
MDvisited_avg = sum(MDvisited) / 50
infoo1 = []
for i in range(0, 50):
    infoo1.append(MDvisited_avg)

plt.figure(1)
plt.xlabel("depth", fontsize="12")
plt.ylabel("visited_states", fontsize="12")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=2)
plt.title("Only Manhattan Distance")
plt.scatter(MDdepth, MDvisited, s=10, label="MD")
plt.plot(MDdepth, infoo, "r", linewidth=1.5, label="Avg_depth")
plt.plot(MDdepth, infoo1, "g", linewidth=1.5, label="Avg_visited")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("MD.png", bbox_inches="tight", dpi=500)
plt.close(1)

data = pd.read_csv("MD_LC.txt", sep=";")

MD_LCdepth = data["depth"].values
MD_LCdepth_avg = sum(MD_LCdepth) / 50
infoo2 = []
for i in range(0, 50):
    infoo2.append(MD_LCdepth_avg)

MD_LCvisited = data["visited"].values
MD_LCvisited_avg = sum(MD_LCvisited) / 50
infoo3 = []
for i in range(0, 50):
    infoo3.append(MD_LCvisited_avg)

plt.figure(2)
plt.xlabel("depth", fontsize="12")
plt.ylabel("visited_states", fontsize="12")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=2)
plt.title("Manhattan Distance + Linear Conflicts")
plt.scatter(MD_LCdepth, MD_LCvisited, s=10, label="MD_LC")
plt.plot(MD_LCdepth, infoo2, "r", linewidth=1.5, label="Avg_depth")
plt.plot(MD_LCdepth, infoo3, "g", linewidth=1.5, label="Avg_visited")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("MD_LC.png", bbox_inches="tight", dpi=500)
plt.close(2)


plt.figure(3)
plt.xlabel("depth", fontsize="12")
plt.ylabel("visited_states", fontsize="12")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=2)
plt.title("Comparison of MD and MD + LC")
plt.scatter(MDdepth, MDvisited, s=10, color='grey', label="MD")
plt.scatter(MD_LCdepth, MD_LCvisited, s=10, label="MD_LC")
plt.plot(MD_LCdepth, infoo2, "r", linewidth=1.5, label="Avg_depth_MD_LC")
plt.plot(MD_LCdepth, infoo3, "g", linewidth=1.5, label="Avg_visited_MD_LC")
plt.plot(MDdepth, infoo, "k", linewidth=1.5, label="Avg_depth_MD")
plt.plot(MDdepth, infoo1, "m", linewidth=1.5, label="Avg_visited_MD")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("comparison.png", bbox_inches="tight", dpi=500)
plt.close(3)
