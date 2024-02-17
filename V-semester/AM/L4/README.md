# Description
The task was to implement a Genetic Algorithm (more info [here](https://www.geeksforgeeks.org/genetic-algorithms/)). My implementation of the metaheuristic is in the
`genetic_algorithm.hpp` file and it consists of five major steps:

# Creating the initial population
In the TSP problem my single specimen has two attributes: chromosome (solution for the TSP) and its fitness (the weight of the solution).
I then proceed to create a certain number of specimen (a population) and also use the island algorithm, where on each island there is a different population that has a limited
way to interact with other populations.

# Evaluation
Each individual has the fitness attribute and based on this attribute I evaluate everyone in the population.

# Selection
The better the fitness attribute (in terms of TSP cycle weight), the better the chances of being picked for the parent.

# Crossover
Here I used two techniques: Partially Mapped Crossover (PMX) and Order Crossover (OX) (for more information click [here](https://en.wikipedia.org/wiki/Crossover_(genetic_algorithm)))

# Mutation
With a certain mutation probability I swap two vertices in an individual and then with the same probability reverse the vertices between two randomly chosen vertices.
I also used a memetic algorithm here, which is a combination of a genetic algorithm with the local solver (in my case I used Local Search algorithm for this).

My program runs on multiple threads to minimize the time of receiving the results.
The whole report can be found in my .pdf file! (it is in Polish) I compared the effectiveness of two kinds of crossover approaches and compared all of my previous
algorithms in terms of finding the best solutions for the TSP problem.

# Usage
Compile: `g++ main.cpp -o main`
Run: `./main`
