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
#include "utils.hpp"
#include "tabu_search.hpp"

using namespace std;

void testParameters() {
    string tsp_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L3/data/smallerTSPs/";
    string cycles_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L3/csv/ex2/";
    string parameters_data_file = "/home/monek/Desktop/VSCode/Vsem/AM/L3/csv/ex2/data2.csv";
    std::ofstream file(parameters_data_file);
    file << "Filename;The_best_solution_weight;Average_solution_weight;TabuListSize;Max_no._iterations_with_worse_solutions\n";
    // Iterate over files in the "data" folder
    //cout << "Filename & The best solution weight & Average solution weight & Temperature & Coefficient & Number of attempts & Max no. epochs with worse solutions" << "\\\\" << endl << "\\hline" << endl;
    for (const auto& entry : filesystem::directory_iterator(tsp_directory)) {
        // Check if the file has the ".tsp" extension
        if (entry.path().extension() == ".tsp") {
            // Read TSP file
            string tsp_file_name = entry.path().stem().string();
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

            // A variable for storing the best solution
            int best_solution_weight = INT_MAX;
            // Variables for storing average solution weight and average number of steps needed to make the solution better
            double avg_solution_weight = 0.0;

            vector<int> tabu_list_sizes;
            vector<int> max_worse_iterations_in_a_row_vector;

             for (int i = 1; i <= 10; i++) {
                tabu_list_sizes.push_back(static_cast<int>(no_of_points * 0.1 * i));
                max_worse_iterations_in_a_row_vector.push_back(static_cast<int>(no_of_points * 0.1 * i));
             }

            std::mt19937 gen{std::random_device{}()};

            std::cout << "Testing for: " << tsp_file_name << endl;
            int progress = 0;

            for (int tabu_list_size : tabu_list_sizes) {
                int c = 0;
                progress++;
                std::cout << "Progress: " << progress << "/" << tabu_list_sizes.size() << endl;
                for (int max_worse_iterations_in_a_row : max_worse_iterations_in_a_row_vector) {
                    c++;
                    best_solution_weight = INT_MAX;
                    double avg_solution_weight = 0.0;
                    auto start_time = std::chrono::high_resolution_clock::now();
                    for (int iteration = 1; iteration <= 50; iteration++) {
                        vector<int> tsp_cycle_nodes_ids;
                        // Creating a random cycle
                        for (int i = 1; i <= no_of_points; i++) {
                            tsp_cycle_nodes_ids.push_back(i);
                        }
                        shuffle(tsp_cycle_nodes_ids.begin(), tsp_cycle_nodes_ids.end(), gen);

                        int current_weight = calculate_tsp_cycle_weight(coordinates, tsp_cycle_nodes_ids, costs);

                        int solution_weight = tabu_search(coordinates, tsp_cycle_nodes_ids, current_weight, costs, tabu_list_size, max_worse_iterations_in_a_row);

                        avg_solution_weight += solution_weight;

                        if (solution_weight < best_solution_weight) {
                            best_solution_weight = solution_weight;
                        }
                    }
                    auto end_time = std::chrono::high_resolution_clock::now();
                    auto duration = std::chrono::duration_cast<std::chrono::seconds>(end_time - start_time);

                    char* name = &tsp_file_name[0];
                    avg_solution_weight /= 50.0;
                    file << name << ";" << best_solution_weight << ";" << avg_solution_weight << ";" << 0.1 * progress << ";" << 0.1 * c << endl;
                    file << duration.count() << endl;
                }
            }
        }
    }
    file.close();

    return;
}

void calculateWeightsForBigExamples(double tabu_size, double max_worse_iterations) {
    string tsp_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L3/data/biggerTSPs/";
    string cycles_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L3/csv/ex2/";
    
    // Iterate over files in the "data" folder
    std::cout << "Filename & The best solution weight & Average solution weight" << "\\\\" << endl << "\\hline" << endl;
    for (const auto& entry : filesystem::directory_iterator(tsp_directory)) {
        // Check if the file has the ".tsp" extension
        if (entry.path().extension() == ".tsp") {
            // Read TSP file
            string tsp_file_name = entry.path().stem().string();
            string whole_file_name = tsp_directory + tsp_file_name + ".tsp";

            map<int, pair<int, int>> coordinates = read_tsp_file(whole_file_name);
            int no_of_points = coordinates.size();
            int tabu_list_size = static_cast<int>(no_of_points * tabu_size);
            int max_worse_iterations_in_a_row = static_cast<int>(no_of_points * max_worse_iterations);
            //map<pair<int, int>, int> edge_costs;
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

            string csv_file_name = cycles_directory + tsp_file_name + ".csv";
            string csv_file = &csv_file_name[0];

            // A variable for storing the best solution
            int best_solution_weight = INT_MAX;
            // A variable for storing average solution weight
            double avg_solution_weight = 0.0;

            std::mt19937 gen{std::random_device{}()};

            for (int j = 1; j <= 100; j++) {
                std::cout << j << endl;
                vector<int> tsp_cycle_nodes_ids;
                // Creating a random cycle
                for (int i = 1; i <= no_of_points; i++) {
                    tsp_cycle_nodes_ids.push_back(i);
                }
                shuffle(tsp_cycle_nodes_ids.begin(), tsp_cycle_nodes_ids.end(), gen);

                int current_weight = calculate_tsp_cycle_weight(coordinates, tsp_cycle_nodes_ids, costs);

                int solution_weight = tabu_search(coordinates, tsp_cycle_nodes_ids, current_weight, costs, tabu_list_size, max_worse_iterations_in_a_row);

                avg_solution_weight += solution_weight;

                if (solution_weight < best_solution_weight) {
                    best_solution_weight = solution_weight;
                    print_cycle_to_csv(coordinates, tsp_cycle_nodes_ids, csv_file);
                }
            }

            avg_solution_weight /= 100.0; 
            char* name = &tsp_file_name[0];
            std::cout << name << " & " <<  best_solution_weight << " & " << avg_solution_weight << "\\\\" << endl << "\\hline" << endl;
        }
    }

    return;
}



int main() {
    //testParameters();
    //calculateWeightsForBigExamples(0.4, 0.5);
    calculateWeightsForBigExamples(0.1, 0.9);
    return 0;
}
