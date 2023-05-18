#include <unordered_map>
#include <limits>
#include <iostream>
#include <fstream>
#include "A_star.hpp"
#include "useful.hpp"

int inversionDistanceHeuristic(std::vector<Board*> visited, std::vector<int> desiredState, int newBoards, Board* current) {
    int smallestIndex = 0;
    int smallest = std::numeric_limits<int>::max();

    for (int i = 0; i < visited.size(); i++) {
        if (visited[i]->cost > smallest) {
            continue;
        }

        int inversionDistance = calculateInversionDistance(visited[i]->state);

        if ((visited[i]->cost + inversionDistance) < smallest) {
            smallest = visited[i]->cost + inversionDistance;
            smallestIndex = i;
        }
        visited[i]->cost += inversionDistance;
    }
    return smallestIndex;
}

int mdlcHeuristic(std::vector<Board*> visited, std::vector<int> desiredState, int newBoards, Board* current, int variant) {
    int smallestIndex = 0;
    int smallest = std::numeric_limits<int>::max();

    for (int i = 0; i < visited.size(); i++) {
        if (visited[i]->cost > smallest) {
            continue;
        }
        smallest = visited[i]->cost;
        smallestIndex = i;
        /*
        if (variant == 1) {
            int manhattanDistance = wholeManhattan(visited[i]->state);
            int linearConflictsCount = linearConflicts(visited[i]->state);
            if ((visited[i]->cost + manhattanDistance + linearConflictsCount) < smallest) {
                smallest = visited[i]->cost + manhattanDistance + linearConflictsCount;
                smallestIndex = i;
            }
            if (visited[i]->parentBoard == NULL) {

            }
            else {
                int oldCost = visited[i]->parentBoard->cost + manhattanDistance + linearConflictsCount;
                int newCost = visited[i]->cost + manhattanDistance + linearConflictsCount;
                if (oldCost > newCost) {
                    visited[i]->cost = newCost;
                }
                else {
                    visited[i]->cost = oldCost;
                }
                //visited[i]->cost += manhattanDistance + linearConflictsCount;
            }
        }
        else {
            int manhattanDistance = wholeManhattan(visited[i]->state);
            if ((visited[i]->cost + manhattanDistance) < smallest) {
                smallest = visited[i]->cost + manhattanDistance;
                smallestIndex = i;
            }
            if (visited[i]->parentBoard == NULL) {

            }
            else {
                visited[i]->cost += manhattanDistance;
            }
        }
        */
    }
    return smallestIndex;
}

void A_star_algorithm (std::vector<int> startingState, std::vector<int> desiredState, int heuristic) {
    int explored = 0;
    int maxBoards = 0;
    int visitedCount = 0;
    std::vector<Board*> visited;

    std::vector<Board*> exploredList;

    Board *startingBoard = new Board(startingState, NULL, Direction::None, 0, 0);

    //addExplored(exploredList, startingBoard);

    int newNodes = 0;

    visited.push_back(startingBoard);
    Board *current = visited.front();

    int its = 0;
    while(true) {
        int index;
        if (heuristic == 1) {
            index = mdlcHeuristic(visited, desiredState, newNodes, current, heuristic);
        }
        else if (heuristic == 2) {
            index = inversionDistanceHeuristic(visited, desiredState, newNodes, current);
        }
        else if (heuristic == 3) {
            index = mdlcHeuristic(visited, desiredState, newNodes, current, heuristic);
        }
        //printf("test %d\n", index);
        current = visited[index];
        if (its % 1 == 0) {
            printBoard(current->state);
        }
        //newNodes = addChildren(exploredList, visited, current, visitedCount);
        addChildren(exploredList, visited, current, visitedCount);
        explored++;
        //maxBoards += newNodes;

        if (current->isSolved(desiredState)) {
            break;
        }
        else {

        }

        visited.erase(visited.begin() + index);

        exploredList.push_back(current);
        its++;
    }

    std::vector<Board*> solution;
    Board* temp = current;

    while (current->parentBoard != NULL) {
        solution.push_back(current);
        current = current->parentBoard;
    }

    for (int i = solution.size() - 1; i >= 0; i--) {
        printBoard(solution[i]->state);
    }

    //std::ofstream myFile;
    //myFile.open("MD_LC.txt", std::fstream::app);
    //myFile << "Depth;Visited" << std::endl;
    //myFile << temp->depth << ";" << explored + 1 << std::endl;
    printf("\nCost of solution: %d\n", temp->cost);
    printf("Depth of solution: %d\n", temp->depth);
    printf("Total visited: %d\n", visitedCount + 1);
    printf("Total explored: %d\n", explored + 1);
    //printf("Max # of boards stored in memory: %d\n", maxBoards + 1);
    std::cout << "-------------PUZZLE HAS BEEN SOLVED---------------" << std::endl;
}