#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <math.h>
#include <set>
#include <filesystem>
#include <chrono>
#include <random>
#include <limits.h>
#include "genetic_algorithm.hpp"

using namespace std;

void testParameters() {
    string tsp_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L4/data/biggerTSPs/";
    string parameters_data_file = "/home/monek/Desktop/VSCode/Vsem/AM/L4/csv/test_parameters.csv";
    std::ofstream file(parameters_data_file);
    file << "Filename;TheBestSolutionWeight;AverageSolutionWeight;Iterations;IslandSize;PopulationSize;EpochSize;IterationsInEpoch;CrossProbability;MutationProbability;CrossType\n";
    // Iterate over files in the "data" folder
    for (const auto& entry : filesystem::directory_iterator(tsp_directory)) {
        // Check if the file has the ".tsp" extension
        if (entry.path().extension() == ".tsp") {
            // Read TSP file
            string tsp_file_name = entry.path().stem().string();
            if (tsp_file_name != "xqf131") {
                continue;
            }
            string whole_file_name = tsp_directory + tsp_file_name + ".tsp";
            map<int, pair<int, int>> coordinates = read_tsp_file(whole_file_name);
            int no_of_points = coordinates.size();
            int** costs = new int*[no_of_points];

            for (int i = 0; i < no_of_points; i++) {
                costs[i] = new int[no_of_points];
                for (int j = 0; j < no_of_points; j++) {
                    if (i>j)
                        costs[i][j] = costs[j][i];
                    else
                        costs[i][j] = calculate_distance(coordinates[i + 1],coordinates[j + 1]);
                }
            }

            std::mt19937 gen{std::random_device{}()};

            int cross_type = 1;

            std::cout << "Testing for: " << tsp_file_name << endl;
            int progress = 0;
            int iterations = 100;
            int pop_size = 32;
            int island_size = 8;
            int epoch_size = 5;
            int epoch_iterations_size = 100;
            double cross_probability = 0.8;
            double mutation_probability = 0.05;

            std::vector<int> results;
            for (int i = 0; i < iterations; i++) {
                progress++;
                std::cout << "Progress: " << progress << "/" << iterations << endl;
                // Creating populations on islands
                std::vector<std::vector<Specimen>> islands_populations;
                for (int j = 1; j <= island_size; j++) {
                    std::vector<Specimen> population;
                    for (int k = 1; k <= pop_size; k++) {
                        Specimen specimen;
                        for (int point = 1; point <= no_of_points; point++) {
                            specimen.chromosome.push_back(point);
                        }
                        shuffle(specimen.chromosome.begin(), specimen.chromosome.end(), gen);
                        specimen.fitness = calculate_tsp_cycle_weight(coordinates, specimen.chromosome, costs);
                        population.push_back(specimen);
                    }
                    islands_populations.push_back(population);
                }
                Specimen the_best_specimen_found;
                the_best_specimen_found = genetic_algorithm(coordinates, costs, islands_populations, epoch_size, 
                                                            epoch_iterations_size, cross_probability, mutation_probability, 
                                                            cross_type, gen);
                results.push_back(the_best_specimen_found.fitness);
            }

            char* name = &tsp_file_name[0];
            double avg_best_solution = 0.0;
            int best_solution_found = std::numeric_limits<int>::max();
            for (int result : results) {
                if (result < best_solution_found) {
                    best_solution_found = result;
                }
                avg_best_solution += result;
            }
            avg_best_solution /= results.size();
            file << name << ";" << best_solution_found << ";" << avg_best_solution << ";" << iterations << ";" 
            << island_size <<  ";" << pop_size << ";" << epoch_size << ";" << epoch_iterations_size << ";"
            << cross_probability << ";" << mutation_probability << ";" << cross_type << endl;
        }
    }
    file.close();

    return;
}

int main() {
    testParameters();
    return 0;
}