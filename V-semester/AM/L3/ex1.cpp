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
#include "simulated_annealing.hpp"

using namespace std;

void testParameters() {
    string tsp_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L3/data/smallerTSPs/";
    string cycles_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L3/csv/ex1/";
    string parameters_data_file = "/home/monek/Desktop/VSCode/Vsem/AM/L3/csv/data2.csv";
    std::ofstream file(parameters_data_file);
    file << "Filename;The best solution weight;Average solution weight;Temperature;Coefficient;Number of attempts;Max no. epochs with worse solutions\n";
    // Iterate over files in the "data" folder
    cout << "Filename & The best solution weight & Average solution weight & Temperature & Coefficient & Number of attempts & Max no. epochs with worse solutions" << "\\\\" << endl << "\\hline" << endl;
    for (const auto& entry : filesystem::directory_iterator(tsp_directory)) {
        // Check if the file has the ".tsp" extension
        if (entry.path().extension() == ".tsp") {
            // Read TSP file
            string tsp_file_name = entry.path().stem().string();
            string whole_file_name = tsp_directory + tsp_file_name + ".tsp";
            map<int, pair<int, int>> coordinates = read_tsp_file(whole_file_name);
            int no_of_points = coordinates.size();
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

            // A variable for storing the best solution
            int best_solution_weight = INT_MAX;
            // Variables for storing average solution weight and average number of steps needed to make the solution better
            double avg_solution_weight = 0.0;

            vector<double> temperatures;
            //double temperature = 70.0;
            vector<double> coefficients;
            //double coefficient = 0.99;
            vector<int> max_worse_epochs_in_a_row_vector;
            int max_worse_epochs_in_a_row = 100;
            vector<int> no_of_attempts;
            int attempts_in_epoch = no_of_points;

             int num_parts = 10;
             double interval_width = 0.2 / num_parts;
             for (int i = 1; i <= num_parts; i++) {
                 temperatures.push_back(100 * i);
                 if (i == num_parts) {
                     coefficients.push_back(0.99);
                 }
                 else {
                     coefficients.push_back(0.8 + 0.02 * (i - 1));
                 }
                 max_worse_epochs_in_a_row_vector.push_back(100);
                 //no_of_attempts.push_back(no_of_points);
             }

            std::mt19937 gen{std::random_device{}()};

            std::cout << "Testing for: " << tsp_file_name << endl;
            int progress = 0;
            for (double temperature : temperatures) {
                progress++;
                std::cout << "Progress: " << progress << "/" << temperatures.size() << endl;
                for (double coefficient : coefficients) {
                    //for (int max_worse_epochs_in_a_row : max_worse_epochs_in_a_row_vector) {
                        int best_best_solution_weight = INT_MAX;
                        double avg_avg_solution_weight = 0.0;
                        for (int iteration = 1; iteration <= 50; iteration++) {
                            vector<int> tsp_cycle_nodes_ids;
                            // Creating a random cycle
                            for (int i = 1; i <= no_of_points; i++) {
                            tsp_cycle_nodes_ids.push_back(i);
                            }
                            shuffle(tsp_cycle_nodes_ids.begin(), tsp_cycle_nodes_ids.end(), gen);

                            int worse_epochs_in_a_row = 0;
                            int counter = 0;
                            double chosen_temperature = temperature;
                            best_solution_weight = INT_MAX;
                            avg_solution_weight = 0.0;

                            int current_weight = calculate_tsp_cycle_weight(coordinates, tsp_cycle_nodes_ids, costs);

                            while (worse_epochs_in_a_row < max_worse_epochs_in_a_row) {
                                counter++;

                                int solution_weight = simulated_annealing(coordinates, tsp_cycle_nodes_ids, current_weight, costs, chosen_temperature, attempts_in_epoch);

                                if (solution_weight >= current_weight) {
                                    worse_epochs_in_a_row++;
                                }
                                else {
                                    worse_epochs_in_a_row = 0;
                                }

                                avg_solution_weight += solution_weight;

                                if (solution_weight < best_solution_weight) {
                                    best_solution_weight = solution_weight;
                                }

                                chosen_temperature *= coefficient;
                                current_weight = solution_weight;
                            }

                            if (best_solution_weight < best_best_solution_weight) {
                                best_best_solution_weight = best_solution_weight;
                            }

                            //string csv_file_name = cycles_directory + tsp_file_name + ".csv";
                            //string csv_file = &csv_file_name[0];

                            //print_cycle_to_csv(coordinates, tsp_cycle_nodes_ids, csv_file);


                            avg_solution_weight /= counter;

                            avg_avg_solution_weight += avg_solution_weight;


                            //cout << name << " & " <<  best_solution_weight << " & " << avg_solution_weight << " & " << temperature << " & " << coefficient << " & " << attempts_in_epoch <<  " & " << max_worse_epochs_in_a_row << "\\\\" << endl << "\\hline" << endl;
                        }
                        char* name = &tsp_file_name[0];
                        avg_avg_solution_weight /= 50.0;
                        file << name << ";" << best_best_solution_weight << ";" << avg_avg_solution_weight << ";" << temperature << ";" << coefficient <<  ";" << max_worse_epochs_in_a_row << endl;
                    //}
                }
            }
        }
    }
    file.close();

    return;
}

void calculateWeightsForBigExamples(double temperature, double coefficient, int max_worse_epochs_in_a_row) {
    string tsp_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L3/data/biggerTSPs/";
    string cycles_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L3/csv/ex1/";
    
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
            int best_best_solution_weight = INT_MAX;

            int attempts_in_epoch = 5 * no_of_points;

            std::mt19937 gen{std::random_device{}()};

            for (int j = 1; j <= 100; j++) {
                best_solution_weight = INT_MAX;
                vector<int> tsp_cycle_nodes_ids;
                // Creating a random cycle
                for (int i = 1; i <= no_of_points; i++) {
                    tsp_cycle_nodes_ids.push_back(i);
                }
                shuffle(tsp_cycle_nodes_ids.begin(), tsp_cycle_nodes_ids.end(), gen);

                int worse_epochs_in_a_row = 0;
                double chosen_temperature = temperature;

                int current_weight = calculate_tsp_cycle_weight(coordinates, tsp_cycle_nodes_ids, costs);

                while (worse_epochs_in_a_row < max_worse_epochs_in_a_row) {
                    int solution_weight = simulated_annealing(coordinates, tsp_cycle_nodes_ids, current_weight, costs, chosen_temperature, attempts_in_epoch);

                    if (solution_weight >= current_weight) {
                        worse_epochs_in_a_row++;
                    }
                    else {
                        worse_epochs_in_a_row = 0;
                    }

                    if (solution_weight < best_solution_weight) {
                        best_solution_weight = solution_weight;
                    }

                    chosen_temperature *= coefficient;
                    current_weight = solution_weight;
                }

                avg_solution_weight += best_solution_weight;

                if (best_solution_weight < best_best_solution_weight) {
                    best_best_solution_weight = best_solution_weight;
                    print_cycle_to_csv(coordinates, tsp_cycle_nodes_ids, csv_file);
                }
            }
            avg_solution_weight /= 100.0; 
            char* name = &tsp_file_name[0];
            std::cout << name << " & " <<  best_best_solution_weight << " & " << avg_solution_weight << "\\\\" << endl << "\\hline" << endl;
        }
    }

    return;
}



int main() {
    //testParameters();
    calculateWeightsForBigExamples(100.0, 0.9, 100);
    return 0;
}
