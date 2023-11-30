# Description
This directory refers to usage of **LocalSearch** algorithm in the TSP. My graph structure is inside the **graph.hpp** header file, helper functions are in the **utils.hpp** header file and
implementation of the **LocalSearch** algorithm is in **functions.hpp** header file. In the **local-search.cpp** the program is divided into three tasks:
1. Calculate the minimum spanning tree and perform ⌈√n⌉ times (where n is the number of vertices) the following algorithm:
  - Choose a vertex randomly.
  - Construct a cycle by traversing the tree starting from the randomly chosen vertex.
  - Apply the Local Search algorithm to the obtained cycle.

For each set of data, provide the weight of the minimum spanning tree, the average value of the obtained solution, the average number of improvement steps, and the best solution obtained.

2. Execute the Local Search algorithm for n random permutations. For each set of data, provide the average value of the obtained solution, the average number of improvement steps, and the best solution obtained.
3. To speed up calculations, instead of testing the entire neighborhood, we select only the best neighbor from n randomly chosen ones. Execute the modified Local Search algorithm for n random permutations. For each set of data, provide the average value of the obtained solution, the average number of improvement steps, and the best solution obtained.
# Usage
Simply run it using `./local-search`. If you want to visualize the cycles and statistics use `./plots.py`
