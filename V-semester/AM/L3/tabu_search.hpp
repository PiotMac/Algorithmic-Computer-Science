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
#include <list>
#include "functions.hpp"

int tabu_search(std::map<int, std::pair<int, int>>& coordinates, std::vector<int>& current_cycle, int current_weight, int** costs, int tabu_list_size, int max_iterations_without_improvement) {
    const int size_of_current_cycle = static_cast<int>(current_cycle.size());

    std::mt19937 gen{ static_cast<std::mt19937::result_type>(
                     std::chrono::steady_clock::now().time_since_epoch().count()
             )};
    std::uniform_int_distribution<size_t> dist(0, size_of_current_cycle - 2);

    list<pair<int, int>> tabu_list;

    int the_best_weight_so_far = current_weight;
    std::vector<int> the_best_cycle = current_cycle;

    int counter = 0;

     // Generate neighbours (invert moves)
    while (counter != max_iterations_without_improvement) {
        int smallestFoundWeightChange = 0;
        int best_i_index;
        int best_j_index;

        for (int inv = 0; inv < size_of_current_cycle; inv++) {
            int i = dist(gen);
            int j = dist(gen);
            if(i > j) {
            int temp;
                temp = i;
                i = j;
                j = temp;
            }

            // Solution in the tabu list - skip
                if (std::find(tabu_list.begin(), tabu_list.end(), pair{i, j}) != tabu_list.end()) {
                    continue;
                }

                int currentlyFoundWeightChange = 0;

                currentlyFoundWeightChange = - costs[current_cycle[i] - 1][current_cycle[i + 1] - 1]
                                             - costs[current_cycle[j] - 1][current_cycle[j + 1] - 1]
                                             + costs[current_cycle[i] - 1][current_cycle[j] - 1]
                                             + costs[current_cycle[i + 1] - 1][current_cycle[j + 1] - 1];

                if(currentlyFoundWeightChange < smallestFoundWeightChange) {
                    smallestFoundWeightChange = currentlyFoundWeightChange;
                    best_i_index = i;
                    best_j_index = j;
                }
        }

        if (smallestFoundWeightChange < 0) {
            std::reverse(current_cycle.begin() + best_i_index + 1, current_cycle.begin() + best_j_index + 1);
            //current_weight += smallestFoundWeightChange;
            current_weight =  calculate_tsp_cycle_weight(coordinates, current_cycle, costs);

            if (tabu_list.size() == tabu_list_size) {
                tabu_list.pop_front();
            }

            // Adding the solution to the tabu list
            tabu_list.push_back(pair{best_i_index, best_j_index});

            if (current_weight < the_best_weight_so_far) {
                the_best_cycle = current_cycle;
                the_best_weight_so_far = current_weight;
                counter = 0;
            }
        }
        else {
            counter++;
        }
    }

    current_cycle = the_best_cycle;

    return the_best_weight_so_far;
}