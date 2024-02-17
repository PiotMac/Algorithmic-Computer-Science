# Description
The task was to implement two metaheuristic algorithms that try to find and return the best solution for the TSP problem:
1. Simulated Annealing
2. Tabu Search

# Simulated Annealing
I will not go in detail about this metaheuristic but for a good explanation click [here](https://www.baeldung.com/cs/simulated-annealing).
My `ex1.cpp` file is divided into two functions. The first one finds solutions for the graphs with less than 1000 vertices (from data/smallerTSPs folder) and tests different starting parameters.
I then looked for which parameters the metaheurtistic does the best job and properly set those parameters for the next part. The second function does a similar thing as the first one but uses those
best parameters and performs the Simulated Annealing algorithm on more dense graphs that can be found in the data/biggerTSPs folder. It also repeats 100 times for each graph and then returns the best
found solution (with the smallest weight) and the average of all found solutions.
My implementation of this metaheuristic is in the `simulated_annealing.hpp` file.

# Tabu Search
For more information about the metaheuristic click [here](https://en.wikipedia.org/wiki/Tabu_search).
I use it in `ex2.cpp` file and the program is identical to the one above but simply uses Tabu Search instead of Simulated Annealing.
My implementation is in the `tabu_search.hpp` file.

# Usage
1. Simulated Annealing - compile using `g++ ex1.cpp -o ex1` and run by writing a command `./ex1`
2. Tabu Search - compile using `g++ ex2.cpp -o ex2` and run by writing a command `./ex2`
