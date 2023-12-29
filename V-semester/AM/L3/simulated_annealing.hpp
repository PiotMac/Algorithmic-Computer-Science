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
#include "functions.hpp"

int simulated_annealing(std::map<int, std::pair<int, int>>& coordinates, std::vector<int>& current_cycle, int current_weight, int** costs, double temperature, int epoch_iterations) {
    const int size_of_current_cycle = static_cast<int>(current_cycle.size());

    std::mt19937 gen{ static_cast<std::mt19937::result_type>(
                     std::chrono::steady_clock::now().time_since_epoch().count()
             )};
    std::uniform_int_distribution<size_t> dist(0, size_of_current_cycle - 2);
    std::uniform_real_distribution<double> real_dist(0.0, 1.0);


    int counter = 0;

     // Generate neighbours (invert moves)
    while (counter != epoch_iterations) {
        counter++;

        int i = dist(gen);
        int j = dist(gen);
        if(i > j) {
            int temp;
            temp = i;
            i = j;
            j = temp;
        }

        int currentlyFoundWeightChange = 0;

            currentlyFoundWeightChange = - costs[current_cycle[i] - 1][current_cycle[i + 1] - 1]
                                         - costs[current_cycle[j] - 1][current_cycle[j + 1] - 1]
                                         + costs[current_cycle[i] - 1][current_cycle[j] - 1]
                                         + costs[current_cycle[i + 1] - 1][current_cycle[j + 1] - 1];

    
        if (currentlyFoundWeightChange < 0) {
            std::reverse(current_cycle.begin() + i + 1, current_cycle.begin() + j + 1);
            current_weight =  calculate_tsp_cycle_weight(coordinates, current_cycle, costs);
        }
        else {
            double random = real_dist(gen);
            double boltzmann = std::exp(-currentlyFoundWeightChange / temperature);
            if (random < boltzmann) {
                std::reverse(current_cycle.begin() + i + 1, current_cycle.begin() + j + 1);
                current_weight =  calculate_tsp_cycle_weight(coordinates, current_cycle, costs);
            }
        }
    }

    return current_weight;
}