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
#include "utils.hpp"
#include "functions.hpp"

using namespace std;

void ex1() {
    string tsp_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L1/data/";
    string mst_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L1/mst/";
    string cycles_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L1/cycles/ex1/";
    // Iterate over files in the "data" folder
    cout << "Filename & MST weight & Average solution weight & Average number of steps & The best solution weight & Iterations" << "\\\\" << endl << "\\hline" << endl;
    for (const auto& entry : filesystem::directory_iterator(tsp_directory)) {
        // Check if the file has the ".tsp" extension
        if (entry.path().extension() == ".tsp") {
            // Read TSP file
            string tsp_file_name = entry.path().stem().string();
            string whole_file_name = tsp_directory + tsp_file_name + ".tsp";
            map<int, pair<int, int>> coordinates = read_tsp_file(whole_file_name);
            int no_of_points = coordinates.size();
            map<pair<int, int>, int> edge_costs;
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
            // Iterate through all pairs of vertices
            for (const auto& vertex1 : coordinates) {
                for (const auto& vertex2 : coordinates) {
                    if (vertex1.first != vertex2.first) {
                        // Calculate the distance/cost between the two vertices
                        int cost = calculate_distance(vertex1.second, vertex2.second);

                        // Store the cost in the map using a pair of vertex IDs as the key
                        edge_costs[{vertex1.first, vertex2.first}] = cost;
                    }
                }
            }

            string csv_file_name = mst_directory + tsp_file_name + ".csv";
            char* csv_file = &csv_file_name[0];

            // Creating minimum spanning tree
            auto [mst_edges, mst_weight] = kruskal_mst(coordinates);

            // A variable for storing the best solution
            double best_solution_weight = numeric_limits<double>::infinity();
            // Variables for storing average solution weight and average number of steps needed to make the solution better
            double avg_solution_weight = 0.0, avg_no_of_steps = 0.0;

            int iterations = (ceil(sqrt(no_of_points)));

            vector<int> tsp_cycle_nodes_ids;
            
            for (int i = 1; i <= iterations; i++) {
                // Randomly choosing a starting vertex
                int randomly_chosen_vertex = rand() % no_of_points + 1;
                // Creating a cycle using dfs
                tsp_cycle_nodes_ids = create_tsp_cycle_dfs(mst_edges, randomly_chosen_vertex);
                // Use Local Search for this cycle
                auto [solution_weight, no_of_steps] = local_search(coordinates, tsp_cycle_nodes_ids, edge_costs, costs);
                avg_solution_weight += solution_weight;
                avg_no_of_steps += no_of_steps;
                if (solution_weight < best_solution_weight) {
                    best_solution_weight = solution_weight;
                }
            }

            csv_file_name = cycles_directory + tsp_file_name + ".csv";
            csv_file = &csv_file_name[0];

            print_cycle_to_csv(coordinates, tsp_cycle_nodes_ids, csv_file);


            avg_solution_weight /= iterations;
            avg_no_of_steps /= iterations;

            char* name = &tsp_file_name[0];

            cout << name << " & " << mst_weight << " & " << avg_solution_weight << " & " << avg_no_of_steps << " & " << best_solution_weight << " & " << iterations << "\\\\" << endl << "\\hline";
        }
    }
    return;
}

void ex2() {
    string tsp_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L1/data/";
    // Iterate over files in the "data" folder
    cout << "Filename & Average solution weight & Average number of steps & The best solution weight & Iterations" << "\\\\" << endl << "\\hline" << endl;
    for (const auto& entry : filesystem::directory_iterator(tsp_directory)) {
        // Check if the file has the ".tsp" extension
        if (entry.path().extension() == ".tsp") {
            // Read TSP file
            string tsp_file_name = entry.path().stem().string();
            string whole_file_name = tsp_directory + tsp_file_name + ".tsp";
            map<int, pair<int, int>> coordinates = read_tsp_file(whole_file_name);
            int no_of_points = coordinates.size();
            map<pair<int, int>, int> edge_costs;

            int** costs = new int*[no_of_points];

            for (int i = 0; i < no_of_points; i++) {
                costs[i] = new int[no_of_points];
                for (int j = 0; j < no_of_points; j++) {
                    if (i>j)
                        costs[i][j] = costs[j][i];
                    else
                        costs[i][j] = calculate_distance(coordinates[i],coordinates[j]);
                }
            }

            // Iterate through all pairs of vertices
            for (const auto& vertex1 : coordinates) {
                for (const auto& vertex2 : coordinates) {
                    if (vertex1.first != vertex2.first) {
                        // Calculate the distance/cost between the two vertices
                        int cost = calculate_distance(vertex1.second, vertex2.second);

                        // Store the cost in the map using a pair of vertex IDs as the key
                        edge_costs[{vertex1.first, vertex2.first}] = cost;
                    }
                }
            }

            std::mt19937 gen{std::random_device{}()};

            // A variable for storing the best solution
            double best_solution_weight = numeric_limits<double>::infinity();
            // Variables for storing average solution weight and average number of steps needed to make the solution better
            double avg_solution_weight = 0.0, avg_no_of_steps = 0.0;

            int iterations = no_of_points;
            if (no_of_points > 1000) {
                iterations = 100;
            }
            
            for (int i = 1; i <= iterations; i++) {
                // Randomly choosing a starting vertex
                int randomly_chosen_vertex = rand() % no_of_points + 1;
                vector<int> tsp_cycle_nodes_ids;
                for (int i = 1; i <= no_of_points; i++) {
                    tsp_cycle_nodes_ids.push_back(i);
                }
                shuffle(tsp_cycle_nodes_ids.begin(), tsp_cycle_nodes_ids.end(), gen);
                // Use Local Search for this cycle
                auto [solution_weight, no_of_steps] = local_search(coordinates, tsp_cycle_nodes_ids, edge_costs, costs);
                avg_solution_weight += solution_weight;
                avg_no_of_steps += no_of_steps;
                if (solution_weight < best_solution_weight) {
                    best_solution_weight = solution_weight;
                }
            }

            avg_solution_weight /= iterations;
            avg_no_of_steps /= iterations;

            char* name = &tsp_file_name[0];

            cout << name << " & " << avg_solution_weight << " & " << avg_no_of_steps << " & " << best_solution_weight << " & " << iterations << "\\\\" << endl << "\\hline" << endl;
        }
    }
    return;
}

void ex3() {
    string tsp_directory = "/home/monek/Desktop/VSCode/Vsem/AM/L1/data/";
    // Iterate over files in the "data" folder
    cout << "Filename & Average solution weight & Average number of steps & The best solution weight & Iterations" << "\\\\" << endl << "\\hline" << endl;
    for (const auto& entry : filesystem::directory_iterator(tsp_directory)) {
        // Check if the file has the ".tsp" extension
        if (entry.path().extension() == ".tsp") {
            // Read TSP file
            string tsp_file_name = entry.path().stem().string();
            string whole_file_name = tsp_directory + tsp_file_name + ".tsp";
            map<int, pair<int, int>> coordinates = read_tsp_file(whole_file_name);
            int no_of_points = coordinates.size();
            map<pair<int, int>, int> edge_costs;

            int** costs = new int*[no_of_points];

            for (int i = 0; i < no_of_points; i++) {
                costs[i] = new int[no_of_points];
                for (int j = 0; j < no_of_points; j++) {
                    if (i>j)
                        costs[i][j] = costs[j][i];
                    else
                        costs[i][j] = calculate_distance(coordinates[i],coordinates[j]);
                }
            }

            // Iterate through all pairs of vertices
            for (const auto& vertex1 : coordinates) {
                for (const auto& vertex2 : coordinates) {
                    if (vertex1.first != vertex2.first) {
                        // Calculate the distance/cost between the two vertices
                        int cost = calculate_distance(vertex1.second, vertex2.second);

                        // Store the cost in the map using a pair of vertex IDs as the key
                        edge_costs[{vertex1.first, vertex2.first}] = cost;
                    }
                }
            }

            std::mt19937 gen{std::random_device{}()};

            // A variable for storing the best solution
            double best_solution_weight = numeric_limits<double>::infinity();
            // Variables for storing average solution weight and average number of steps needed to make the solution better
            double avg_solution_weight = 0.0, avg_no_of_steps = 0.0;

            int iterations = no_of_points;
            if (no_of_points > 1000) {
                iterations = 100;
            }
            
            for (int i = 1; i <= iterations; i++) {
                // Randomly choosing a starting vertex
                int randomly_chosen_vertex = rand() % no_of_points + 1;
                vector<int> tsp_cycle_nodes_ids;
                for (int i = 1; i <= no_of_points; i++) {
                    tsp_cycle_nodes_ids.push_back(i);
                }
                shuffle(tsp_cycle_nodes_ids.begin(), tsp_cycle_nodes_ids.end(), gen);
                // Use Local Search for this cycle
                auto [solution_weight, no_of_steps] = local_search_random(coordinates, tsp_cycle_nodes_ids, edge_costs, costs);
                avg_solution_weight += solution_weight;
                avg_no_of_steps += no_of_steps;
                if (solution_weight < best_solution_weight) {
                    best_solution_weight = solution_weight;
                }
            }

            avg_solution_weight /= iterations;
            avg_no_of_steps /= iterations;

            char* name = &tsp_file_name[0];

            cout << name << " & " << avg_solution_weight << " & " << avg_no_of_steps << " & " << best_solution_weight << " & " << iterations << "\\\\" << endl << "\\hline" << endl;
        }
    }
    return;
}

int main() {
    //cout << "ZADANIE 1" << endl;
    //ex1();
    cout << "ZADANIE 2" << endl;
    ex2();
    //cout << "ZADANIE 3" << endl;
    //ex3();
    return 0;
}
