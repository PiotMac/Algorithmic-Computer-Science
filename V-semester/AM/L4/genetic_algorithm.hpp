#include <vector>
#include <utility>
#include <map>
#include <thread>
#include <random>
#include <algorithm>
#include "functions.hpp"

using namespace std;

std::pair<Specimen, Specimen> pmx(const Specimen specimen1, const Specimen specimen2, std::mt19937 &generator) {
    std::uniform_int_distribution<int> dist(1, specimen1.chromosome.size() - 2);
    int index1 = dist(generator);
    int index2 = dist(generator);
    while (index1 == index2) {
        index2 = dist(generator);
    }
    if (index1 > index2) {
        std::swap(index1, index2);
    }

    Specimen child1;
    Specimen child2;
    child1.chromosome = std::vector<int>(specimen1.chromosome.size(), 0);
    child2.chromosome = std::vector<int>(specimen1.chromosome.size(), 0);
    child1.fitness = 0;
    child2.fitness = 0;


    std::map<int, int> mapping1{};
    std::map<int, int> mapping2{};
    for (int i = index1; i <= index2; i++) {
        child1.chromosome[i] = specimen2.chromosome[i];
        child2.chromosome[i] = specimen1.chromosome[i];

        mapping2[specimen1.chromosome[i]] = specimen2.chromosome[i];
        mapping1[specimen2.chromosome[i]] = specimen1.chromosome[i];
    }


    for (int i = 0; i < index1; i++) {
        int index = specimen1.chromosome[i];
        if (mapping1.find(index) == mapping1.end()) {
            child1.chromosome[i] = index;
        } else {
            while (mapping1.find(index) != mapping1.end()) {
                index = mapping1[index];
            }
            child1.chromosome[i] = index;
        }

        index = specimen2.chromosome[i];
        if (mapping2.find(index) == mapping2.end()) {
            child2.chromosome[i] = index;
        } else {
            while (mapping2.find(index) != mapping2.end()) {
                index = mapping2[index];
            }
            child2.chromosome[i] = index;
        }
    }


    for (int i = index2 + 1; i < specimen1.chromosome.size(); i++) {
        int index = specimen1.chromosome[i];
        if (mapping1.find(index) == mapping1.end()) {
            child1.chromosome[i] = index;
        } else {
            while (mapping1.find(index) != mapping1.end()) {
                index = mapping1[index];
            }
            child1.chromosome[i] = index;
        }

        index = specimen2.chromosome[i];
        if (mapping2.find(index) == mapping2.end()) {
            child2.chromosome[i] = index;
        } else {
            while (mapping2.find(index) != mapping2.end()) {
                index = mapping2[index];
            }
            child2.chromosome[i] = index;
        }
    }


    return std::pair(child1, child2);
}

std::pair<Specimen, Specimen> ox(const Specimen specimen1, const Specimen specimen2, std::mt19937 &generator) {
    std::uniform_int_distribution<int> dist(1, specimen1.chromosome.size() - 2);
    int index1 = dist(generator);
    int index2 = dist(generator);
    while (index1 == index2) {
        index2 = dist(generator);
    }

    if (index1 > index2) {
        std::swap(index1, index2);
    }

    Specimen child1;
    Specimen child2;
    child1.chromosome = std::vector<int>(specimen1.chromosome.size(), 0);
    child2.chromosome = std::vector<int>(specimen1.chromosome.size(), 0);
    child1.fitness = 0;
    child2.fitness = 0;

    std::map<int, bool> mapping1{};
    std::map<int, bool> mapping2{};
    for (int i = index1; i <= index2; i++) {
        child1.chromosome[i] = specimen1.chromosome[i];
        child2.chromosome[i] = specimen2.chromosome[i];

        mapping1[specimen1.chromosome[i]] = true;
        mapping2[specimen2.chromosome[i]] = true;
    }

    std::vector<int> missing1{};
    std::vector<int> missing2{};

    for (int i = 0; i < specimen1.chromosome.size(); i++) {
        int index = (index2 + i + 1) % specimen1.chromosome.size();
        if (mapping1.find(specimen2.chromosome[index]) == mapping1.end()) {
            missing1.push_back(specimen2.chromosome[index]);
        }

        if (mapping2.find(specimen1.chromosome[index]) == mapping2.end()) {
            missing2.push_back(specimen1.chromosome[index]);
        }
    }

    for (int i = 0; i < missing1.size(); i++) {
        int index = (index2 + i + 1) % specimen1.chromosome.size();
        child1.chromosome[index] = missing1[i];
        child2.chromosome[index] = missing2[i];
    }

    return std::pair(child1, child2);
}

void geneticThread(std::vector<Specimen>& population, std::map<int, std::pair<int, int>> coordinates, int** costs,
                   int iterations_in_epoch, int crossType, double crossProbability, double mutationProbability,
                   std::mt19937 &gen) {

    for (int iteration = 0; iteration < iterations_in_epoch; iteration++) {
        std::sort(population.begin(), population.end(),
                  [](const Specimen& a, const Specimen& b) { return a.fitness < b.fitness; });

        std::uniform_real_distribution<double> dist(0.0, 1.0);

        std::vector<Specimen> parents{};
        int parentsNumber{population.size() / 2};
        while (parents.size() < parentsNumber) {
            for (int specimen = 0; specimen < population.size(); specimen++) {
                if (parents.size() >= parentsNumber) {
                    break;
                }

                if (dist(gen) < (crossProbability / (1.0 + static_cast<double>(specimen)))) {
                    parents.push_back(population[specimen]);
                }
            }
        }

        std::vector<Specimen> newPopulation{};
        for (int i = 0; i < population.size() / 2; i++) {
            newPopulation.push_back(population[i]);
        }

        std::uniform_int_distribution<int> parentDist(0, parents.size() - 1);
        for (int i = 0; i < population.size() / 4; i++) {
            int parent1Index = parentDist(gen);
            int parent2Index = parentDist(gen);
            while (parent1Index == parent2Index) {
                parent2Index = parentDist(gen);
            }

            std::pair<Specimen, Specimen> children;

            if (crossType == 1) {
                children = pmx(parents[parent1Index], parents[parent2Index], gen);
            } 
            else if (crossType == 2) {
                children = ox(parents[parent1Index], parents[parent2Index], gen);
            }
            else {
                throw std::runtime_error("Unknown cross type");
            }
            
            children.first.fitness = calculate_tsp_cycle_weight(coordinates, children.first.chromosome, costs);
            children.second.fitness = calculate_tsp_cycle_weight(coordinates, children.second.chromosome, costs);
            newPopulation.push_back(children.first);
            newPopulation.push_back(children.second);
        }

        for (auto &specimen: newPopulation) {
            std::uniform_real_distribution<double> mutDist(0.0, 1.0);
            std::uniform_int_distribution<int> indexDist(1, specimen.chromosome.size() - 2);

            if (mutDist(gen) < mutationProbability) {
                int index1 = indexDist(gen);
                int index2 = indexDist(gen);

                if (index1 > index2) {
                    std::swap(index1, index2);
                }

                std::reverse(specimen.chromosome.begin() + index1, specimen.chromosome.begin() + index2 + 1);
                specimen.fitness = calculate_tsp_cycle_weight(coordinates, specimen.chromosome, costs);
            }

            if (mutDist(gen) < mutationProbability) {
                int index1 = indexDist(gen);
                int index2 = indexDist(gen);

                if (index1 > index2) {
                    std::swap(index1, index2);
                }

                std::swap(specimen.chromosome[index1], specimen.chromosome[index2]);
                specimen.fitness = calculate_tsp_cycle_weight(coordinates, specimen.chromosome, costs);
            }
        }

        for (auto &specimen: newPopulation) {
            auto [solution_weight, no_of_steps] = local_search_random(coordinates, specimen.chromosome, costs, gen);
            specimen.fitness = solution_weight;
        }

        population = newPopulation;
    }
}

Specimen genetic_algorithm(std::map<int, std::pair<int, int>> coordinates, int** costs, std::vector<std::vector<Specimen>> islands_populations,
             int epoch_size, int iterations_in_epoch, double cross_probability, double mutation_probability, int cross_type, std::mt19937 &gen) {

    int population_size = islands_populations[0].size();

    for (int epoch = 0; epoch < epoch_size; epoch++) {
        std::vector<std::thread> threads{};
        for (int island = 0; island < islands_populations.size(); island++) {
            threads.emplace_back(geneticThread, std::ref(islands_populations[island]), std::ref(coordinates), std::ref(costs),
                                 iterations_in_epoch, cross_type, cross_probability, mutation_probability,
                                 std::ref(gen));
        }

        for (auto &thread: threads) {
            thread.join();
        }

        for (int island = 0; island < islands_populations.size(); island++) {
            std::sort(islands_populations[island].begin(), islands_populations[island].end(),
                      [](const Specimen& a, const Specimen& b) { return a.fitness < b.fitness; });
        }

        for (int island = 0; island < islands_populations.size(); island++) {
            islands_populations[island].erase(islands_populations[island].begin() + population_size / 2,
                                             islands_populations[island].end());
        }

        std::uniform_int_distribution<int> dist1(0, islands_populations.size() - 1);
        std::uniform_int_distribution<int> dist2(0, population_size / 2 - 1);

        for (int island = 0; island < islands_populations.size(); island++) {
            for (int i = 0; i < population_size / 2; i++) {
                int randomIsland = dist1(gen);
                while (randomIsland == island) {
                    randomIsland = dist1(gen);
                }

                int randomIndex = dist2(gen);
                islands_populations[island].push_back(islands_populations[randomIsland][randomIndex]);
            }
        }
    }

    Specimen bestCycle{};
    int bestCycleWeight{std::numeric_limits<int>::max()};
    for (int island = 0; island < islands_populations.size(); island++) {
        for (int i = 0; i < population_size; i++) {
            if (islands_populations[island][i].fitness < bestCycleWeight) {
                bestCycleWeight = islands_populations[island][i].fitness;
                bestCycle = islands_populations[island][i];
            }
        }
    }

    return bestCycle;
}