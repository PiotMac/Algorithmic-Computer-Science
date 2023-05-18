#include <algorithm>
#include <time.h>
#include <random>
#include "main.hpp"
#include "useful.hpp"
#include "A_star.hpp"

int main(int, char**) {
    srand(time(NULL));
    std::vector<int> desiredState = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0};
    std::vector<int> startingState = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    std::vector<int> testingState = {1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 15, 9, 13, 14, 0};

    /*
    int moves = 0;
    while (moves != 20) {
        int zeroIndex = getEmptyTile(testingState);
        int move = rand() % 4;
        if (moves == 19) {
            testingState[zeroIndex] = testingState[15];
            testingState[15] = 0;
            moves++;
        }
        //UP
        else if (move == 0 && !(zeroIndex <= 3)) {
            testingState[zeroIndex] = testingState[zeroIndex - 4];
            testingState[zeroIndex - 4] = 0;
            moves++;
        }
        //DOWN
        else if (move == 1 && !(zeroIndex >= 12)) {
            testingState[zeroIndex] = testingState[zeroIndex + 4];
            testingState[zeroIndex + 4] = 0;
            moves++;
        }
        //LEFT
        else if (move == 2 && zeroIndex % 4 != 0) {
            testingState[zeroIndex] = testingState[zeroIndex - 1];
            testingState[zeroIndex - 1] = 0;
            moves++;
        }
        //RIGHT
        else if (move == 3 && !zeroIndex % 4 != 3) {
            testingState[zeroIndex] = testingState[zeroIndex + 1];
            testingState[zeroIndex + 1] = 0;
            moves++;
        }
    }
    */

    std::default_random_engine random(std::random_device{}());
    shuffle(startingState.begin(), startingState.end(), random);
    startingState.push_back(0);
    /*
    for (int i = 0; i < testingState.size(); i++) {
            printf("%d ", testingState[i]);
        }
        printf("\n");
        

    
    if (isSolvable(testingState)) {
        printBoard(testingState);
        A_star_algorithm(testingState, desiredState, 1);
    }
    else {
        printBoard(testingState);
        printf("\n------This board is NOT solvable!-----\n");
    }   
    */
    
    if (isSolvable(startingState)) {
        printBoard(startingState);
        A_star_algorithm(startingState, desiredState, 1);
    }
    else {
        printBoard(startingState);
        printf("\n------This board is NOT solvable!-----\n");
    }
    
}
