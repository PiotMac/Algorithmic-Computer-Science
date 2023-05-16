#ifndef MAIN_HPP
#define MAIN_HPP

#include <iostream>
#include <vector>

int universal_counter = 0;

enum Direction { UP, DOWN, LEFT, RIGHT, None};

class Board {
    public:
        std::vector<int> state;
        enum Direction parentMove;
        int depth;
        int cost;
        int id;
        Board *parentBoard;

    Board(std::vector<int> state, Board *parentBoard, enum Direction parentMove, int depth, int cost) {
        this->state = state;
        this->parentMove = parentMove;
        this->depth = depth;
        this->cost = cost;
        this->parentBoard = parentBoard;
    }

    ~Board() {}

    bool isSolved(std::vector<int> desiredState) {
        return this->state == desiredState;
    }
};

#endif