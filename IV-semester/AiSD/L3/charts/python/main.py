import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

plt.style.use('_mpl-gallery')

"""
data = pd.read_csv("random_vs_select.csv", sep=";")
n = data["n"].unique()

random_select = data[data["type"] == "randomSelect"]
random_select_comps = random_select["comps"]
random_select_swaps = random_select["swaps"]
random_select_avg_comps = []
random_select_avg_swaps = []

select = data[data["type"] == "select"]
select_comps = select["comps"]
select_swaps = select["swaps"]
select_avg_comps = []
select_avg_swaps = []

for i in range(0, 100):
    random_select_avg_comps.append(random_select_comps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    random_select_avg_swaps.append(random_select_swaps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    select_avg_comps.append(select_comps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    select_avg_swaps.append(select_swaps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)

plt.figure(1)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of comparisons in SELECT and RANDOMIZED SELECT", fontsize="8")
plt.plot(n, select_avg_comps, "r", linewidth=0.9, label="Select")
plt.plot(n, random_select_avg_comps, "b", linewidth=0.9, label="Randomized Select")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("r_vs_s_comps.png", bbox_inches="tight", dpi=500)
plt.close(1)

plt.figure(2)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of swaps in SELECT and RANDOMIZED SELECT", fontsize="8")
plt.plot(n, select_avg_swaps, "r", linewidth=0.9, label="Select")
plt.plot(n, random_select_avg_swaps, "b", linewidth=0.9, label="Randomized Select")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("r_vs_s_swaps.png", bbox_inches="tight", dpi=500)
plt.close(2)
"""
# ---------------------------------------------------EX 3--------------------------------------------------------------

"""
data = pd.read_csv("ex3.csv", sep=";")
n = data["n"].unique()

exp3 = data[data["k"] == 3]
exp3_comps = exp3["comps"]
exp3_swaps = exp3["swaps"]
exp3_avg_comps = []
exp3_avg_swaps = []

exp5 = data[data["k"] == 5]
exp5_comps = exp5["comps"]
exp5_swaps = exp5["swaps"]
exp5_avg_comps = []
exp5_avg_swaps = []

exp7 = data[data["k"] == 7]
exp7_comps = exp7["comps"]
exp7_swaps = exp7["swaps"]
exp7_avg_comps = []
exp7_avg_swaps = []

exp9 = data[data["k"] == 9]
exp9_comps = exp9["comps"]
exp9_swaps = exp9["swaps"]
exp9_avg_comps = []
exp9_avg_swaps = []

for i in range(0, 100):
    exp3_avg_comps.append(exp3_comps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    exp3_avg_swaps.append(exp3_swaps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    exp5_avg_comps.append(exp5_comps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    exp5_avg_swaps.append(exp5_swaps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    exp7_avg_comps.append(exp7_comps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    exp7_avg_swaps.append(exp7_swaps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    exp9_avg_comps.append(exp9_comps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)
    exp9_avg_swaps.append(exp9_swaps.iloc[100 * i:100 * (i + 1)].sum() / 100.0)

plt.figure(3)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of comparisons in SELECT for k \u2208 {3, 5, 7, 9}", fontsize="8")
plt.plot(n, exp3_avg_comps, "r", linewidth=0.9, label="k = 3")
plt.plot(n, exp5_avg_comps, "b", linewidth=0.9, label="k = 5")
plt.plot(n, exp7_avg_comps, "g", linewidth=0.9, label="k = 7")
plt.plot(n, exp9_avg_comps, "y", linewidth=0.9, label="k = 9")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("exp_comps.png", bbox_inches="tight", dpi=500)
plt.close(3)

plt.figure(4)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of swaps in SELECT for k \u2208 {3, 5, 7, 9}", fontsize="8")
plt.plot(n, exp3_avg_swaps, "r", linewidth=0.9, label="k = 3")
plt.plot(n, exp5_avg_swaps, "b", linewidth=0.9, label="k = 5")
plt.plot(n, exp7_avg_swaps, "g", linewidth=0.9, label="k = 7")
plt.plot(n, exp9_avg_swaps, "y", linewidth=0.9, label="k = 9")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("exp_swaps.png", bbox_inches="tight", dpi=500)
plt.close(4)
"""

# -----------------------------------------------EX 4-------------------------------------------------
"""
data = pd.read_csv("ex4.csv", sep=";")
n = data["n"].unique()

normal_data = data[data["type"] == "normal"]
coefficient_data = data[data["type"] == "coefficient"]

beginning_normal_data = normal_data[normal_data["position"] == "beginning"]
middle_normal_data = normal_data[normal_data["position"] == "middle"]
end_normal_data = normal_data[normal_data["position"] == "end"]
nowhere_normal_data = normal_data[normal_data["position"] == "nowhere"]
random_normal_data = normal_data[normal_data["position"] == "random"]

comps_beginning_normal_data = beginning_normal_data["comps"]
comps_middle_normal_data = middle_normal_data["comps"]
comps_end_normal_data = end_normal_data["comps"]
comps_nowhere_normal_data = nowhere_normal_data["comps"]
comps_random_normal_data = random_normal_data["comps"]
time_beginning_normal_data = beginning_normal_data["time"]
time_middle_normal_data = middle_normal_data["time"]
time_end_normal_data = end_normal_data["time"]
time_nowhere_normal_data = nowhere_normal_data["time"]
time_random_normal_data = random_normal_data["time"]

beginning_coefficient_data = coefficient_data[coefficient_data["position"] == "beginning"]
middle_coefficient_data = coefficient_data[coefficient_data["position"] == "middle"]
end_coefficient_data = coefficient_data[coefficient_data["position"] == "end"]
nowhere_coefficient_data = coefficient_data[coefficient_data["position"] == "nowhere"]
random_coefficient_data = coefficient_data[coefficient_data["position"] == "random"]

comps_beginning_coefficient_data = beginning_coefficient_data["comps"]
comps_middle_coefficient_data = middle_coefficient_data["comps"]
comps_end_coefficient_data = end_coefficient_data["comps"]
comps_nowhere_coefficient_data = nowhere_coefficient_data["comps"]
comps_random_coefficient_data = random_coefficient_data["comps"]
time_beginning_coefficient_data = beginning_coefficient_data["time"]
time_middle_coefficient_data = middle_coefficient_data["time"]
time_end_coefficient_data = end_coefficient_data["time"]
time_nowhere_coefficient_data = nowhere_coefficient_data["time"]
time_random_coefficient_data = random_coefficient_data["time"]

comps_estimated_coefficient = (sum(comps_beginning_coefficient_data) + sum(comps_middle_coefficient_data)
                               + sum(comps_end_coefficient_data) + sum(comps_nowhere_coefficient_data)
                               + sum(comps_random_coefficient_data)) / 500
time_estimated_coefficient = (sum(time_beginning_coefficient_data) + sum(time_middle_coefficient_data)
                              + sum(time_end_coefficient_data) + sum(time_nowhere_coefficient_data)
                              + sum(time_random_coefficient_data)) / 500

plt.figure(5)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of comparisons in BINARY SEARCH in different parts of array", fontsize="8")
plt.plot(n, comps_beginning_normal_data, "r", linewidth=0.9, label="beginning")
plt.plot(n, comps_middle_normal_data, "b", linewidth=0.9, label="middle")
plt.plot(n, comps_end_normal_data, "g", linewidth=0.9, label="end")
plt.plot(n, comps_nowhere_normal_data, "y", linewidth=0.9, label="nowhere")
plt.plot(n, comps_random_normal_data, "k", linewidth=0.9, label="random")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("binary_search_normal_comps.png", bbox_inches="tight", dpi=500)
plt.close(5)

plt.figure(6)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg time in \u00B5s", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average time usage of BINARY SEARCH in different parts of array", fontsize="8")
plt.plot(n, time_beginning_normal_data, "r", linewidth=0.9, label="beginning")
plt.plot(n, time_middle_normal_data, "b", linewidth=0.9, label="middle")
plt.plot(n, time_end_normal_data, "g", linewidth=0.9, label="end")
plt.plot(n, time_nowhere_normal_data, "y", linewidth=0.9, label="nowhere")
plt.plot(n, time_random_normal_data, "k", linewidth=0.9, label="random")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("binary_search_normal_time.png", bbox_inches="tight", dpi=500)
plt.close(6)

plt.figure(7)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of comparisons in BINARY SEARCH / log\u2082(n)", fontsize="8")
plt.plot(n, comps_beginning_coefficient_data, "r", linewidth=0.9, label="beginning")
plt.plot(n, comps_middle_coefficient_data, "b", linewidth=0.9, label="middle")
plt.plot(n, comps_end_coefficient_data, "g", linewidth=0.9, label="end")
plt.plot(n, comps_nowhere_coefficient_data, "y", linewidth=0.9, label="nowhere")
plt.plot(n, comps_random_coefficient_data, "m", linewidth=0.9, label="random")
plt.axhline(y=comps_estimated_coefficient, color="k", linewidth=2.0, label="coefficient")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("binary_search_coefficient_comps.png", bbox_inches="tight", dpi=500)
plt.close(7)

plt.figure(8)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg time in \u00B5s", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average time usage of BINARY SEARCH / log\u2082(n)", fontsize="8")
plt.plot(n, time_beginning_coefficient_data, "r", linewidth=0.9, label="beginning")
plt.plot(n, time_middle_coefficient_data, "b", linewidth=0.9, label="middle")
plt.plot(n, time_end_coefficient_data, "g", linewidth=0.9, label="end")
plt.plot(n, time_nowhere_coefficient_data, "y", linewidth=0.9, label="nowhere")
plt.plot(n, time_random_coefficient_data, "m", linewidth=0.9, label="random")
plt.axhline(y=time_estimated_coefficient, color="k", linewidth=2.0, label="coefficient")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("binary_search_coefficient_time.png", bbox_inches="tight", dpi=500)
plt.close(8)

"""
# -----------------------------------------------EX 5----------------------------------------------------

data = pd.read_csv("ex5.csv", sep=";")
n = data["n"].unique()

random_type_of_data = data[data["data"] == "random"]
worst_type_of_data = data[data["data"] == "worst"]

quickSortSelectRandomData = random_type_of_data[random_type_of_data["type"] == "quickSortSelect"]
quickSortRandomData = random_type_of_data[random_type_of_data["type"] == "quickSort"]
quickSortSelectWorstData = worst_type_of_data[worst_type_of_data["type"] == "quickSortSelect"]
quickSortWorstData = worst_type_of_data[worst_type_of_data["type"] == "quickSort"]

time_qsSR = quickSortSelectRandomData["time"]
comps_qsSR = quickSortSelectRandomData["comps"]
swaps_qsSR = quickSortSelectRandomData["swaps"]

time_qsR = quickSortRandomData["time"]
comps_qsR = quickSortRandomData["comps"]
swaps_qsR = quickSortRandomData["swaps"]

time_qsSW = quickSortSelectWorstData["time"]
comps_qsSW = quickSortSelectWorstData["comps"]
swaps_qsSW = quickSortSelectWorstData["swaps"]

time_qsW = quickSortWorstData["time"]
comps_qsW = quickSortWorstData["comps"]
swaps_qsW = quickSortWorstData["swaps"]

plt.figure(9)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg time in \u00B5s", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average time usage in QuickSort and QuickSelect in average case", fontsize="8")
plt.plot(n, time_qsSR, "r", linewidth=0.9, label="QuickSelect")
plt.plot(n, time_qsR, "b", linewidth=0.9, label="QuickSort")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("quick_vs_select_average_time.png", bbox_inches="tight", dpi=500)
plt.close(9)

plt.figure(10)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of comparisons in QuickSort and QuickSelect in average case", fontsize="8")
plt.plot(n, comps_qsSR, "r", linewidth=0.9, label="QuickSelect")
plt.plot(n, comps_qsR, "b", linewidth=0.9, label="QuickSort")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("quick_vs_select_average_comps.png", bbox_inches="tight", dpi=500)
plt.close(10)

plt.figure(11)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of swaps in QuickSort and QuickSelect in average case", fontsize="8")
plt.plot(n, swaps_qsSR, "r", linewidth=0.9, label="QuickSelect")
plt.plot(n, swaps_qsR, "b", linewidth=0.9, label="QuickSort")

plt.legend(loc=4, fontsize="xx-small")
plt.savefig("quick_vs_select_average_swaps.png", bbox_inches="tight", dpi=500)
plt.close(11)

plt.figure(12)
plt.rc('xtick', labelsize=5)
plt.rc('ytick', labelsize=5)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg time in ms", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average time usage in QuickSort and QuickSelect in worst case", fontsize="8")
plt.plot(n, time_qsSW / 1000.0, "r", linewidth=0.9, label="QuickSelect")
plt.plot(n, time_qsW / 1000.0, "b", linewidth=0.9, label="QuickSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("quick_vs_select_worst_time.png", bbox_inches="tight", dpi=500)
plt.close(12)

plt.figure(13)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_comparisons", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of comparisons in QuickSort and QuickSelect in worst case", fontsize="8")
plt.plot(n, comps_qsSW, "r", linewidth=0.9, label="QuickSelect")
plt.plot(n, comps_qsW, "b", linewidth=0.9, label="QuickSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("quick_vs_select_worst_comps.png", bbox_inches="tight", dpi=500)
plt.close(13)

plt.figure(14)
plt.xlabel("n", fontsize="10")
plt.ylabel("avg_swaps", fontsize="10")
plt.rc('legend', fontsize=2)
plt.rc('axes', labelsize=1)
plt.title("Average # of swaps in QuickSort and QuickSelect in worst case", fontsize="8")
plt.plot(n, swaps_qsSW, "r", linewidth=0.9, label="QuickSelect")
plt.plot(n, swaps_qsW, "b", linewidth=0.9, label="QuickSort")

plt.legend(loc=2, fontsize="xx-small")
plt.savefig("quick_vs_select_worst_swaps.png", bbox_inches="tight", dpi=500)
plt.close(14)
