#include <cmath>
#include <string>
#include <unordered_map>
#include <sstream>
#include <algorithm>
#include "main.hpp"

std::string stateToString(std::vector<int> state) {
    char hex[16] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
    std::string str = "0123456789abcdef";
    for (int i = 0; i < 16; i++) {
        str[i] = hex[state[i]];
    }
    return str;
}

int getEmptyTile(std::vector<int> state) {
    for (int i = 0; i < state.size(); i++) {
        if (state[i] == 0) {
            return i;
        }
    }
}

bool isSolvable(std::vector<int> startingState) {
    int inversions_count = 0;
    for (int i = 0; i < 15; i++) {
        for (int j = i + 1; j < 16; j++) {
            if (startingState[i] > startingState[j] && startingState[i] != 0 && startingState[j] != 0) {
                inversions_count++;
            }
        }
    }
    if (inversions_count % 2 == 0) {
        return true;
    }
    return false;
}

void printBoard(std::vector<int> state) {
    printf("--------------------------------------\n");
    printf("|   %2d   |   %2d   |   %2d   |   %2d   |\n", state[0], state[1], state[2], state[3]);
    printf("|   %2d   |   %2d   |   %2d   |   %2d   |\n", state[4], state[5], state[6], state[7]);
    printf("|   %2d   |   %2d   |   %2d   |   %2d   |\n", state[8], state[9], state[10], state[11]);
    printf("|   %2d   |   %2d   |   %2d   |   %2d   |\n", state[12], state[13], state[14], state[15]);
    printf("--------------------------------------\n");
}

int horizontalInversionDistance(std::vector<int> tiles) {
    int horizontalID = 0;
    std::vector<int> rotatedVector;
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            rotatedVector.push_back(tiles[4 * j + i]);
        }
    }
    for (int i = 0; i < rotatedVector.size() - 1; i++) {
        if(rotatedVector[i] > rotatedVector[i + 1] && rotatedVector[i] != 0 && rotatedVector[i + 1] != 0) {
            horizontalID++;
        }
    }

    return horizontalID / 3 + horizontalID % 3;
}

int verticalInversionDistance(std::vector<int> tiles) {
    int verticalID = 0;
    for (int i = 0; i < tiles.size() - 1; i++) {
        if(tiles[i] > tiles[i + 1] && tiles[i] != 0 && tiles[i + 1] != 0) {
            verticalID++;
        }
    }
    return verticalID / 3 + verticalID % 3;
}

int calculateInversionDistance(std::vector<int> tiles) {
    int horizontalID = horizontalInversionDistance(tiles);
    int verticalID = verticalInversionDistance(tiles);

    return horizontalID + verticalID;
}

int rowConflicts(std::vector<int> tiles) {
    int grid[4][4];
    int rowConflicts = 0;
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            grid[i][j] = tiles[4 * i + j];
        }
    }
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 3; j++) {
            if (grid[i][j] > grid[i][j + 1] && (grid[i][j] - 1) / 4 == i && (grid[i][j + 1] - 1) / 4 == i) {
                rowConflicts++;
            }
        }
    }

    return rowConflicts;
}

int columnConflicts(std::vector<int> tiles) {
    int grid[4][4];
    int columnConflicts = 0;
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            grid[i][j] = tiles[4 * i + j];
        }
    }
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 4; j++) {
            if (grid[i][j] > grid[i + 1][j] && (grid[i][j] - 1) % 4 == i && (grid[i + 1][j] - 1) % 4 == i) {
                columnConflicts++;
            }
        }
    }

    return columnConflicts;
}

int linearConflicts(std::vector<int> tiles) {
    int rowConflictsCount = rowConflicts(tiles);
    int columnConflictsCount = columnConflicts(tiles);

    return rowConflictsCount + columnConflictsCount;
}

int manhattan(int x1, int y1, int x2, int y2) {
    return std::abs(x2 - x1) + std::abs(y2 - y1);
}

int wholeManhattan(std::vector<int> tiles) {
    int total = 0;
    for (int i = 0; i < 16; i++) {
        if (tiles[i] != 0) {
            int goalRow = (tiles[i] - 1) / 4;
            int goalColumn = (tiles[i] - 1) % 4;
            int actualRow = i / 4;
            int actualColumn = i % 4;
            int distance = manhattan(actualRow, actualColumn, goalRow, goalColumn);
            total += distance;
        }
    }
    return total;
}

int isExplored(std::vector<Board*> &explored, Board* board) {
    for (int i = 0; i < explored.size(); i++) {
        if (explored[i]->state == board->state) {
            return i;
        }
    }
    //delete board;
    return -1;
}

void checkExplored(std::vector<Board*> &explored, std::vector<Board*> &visited, Board* board, int index) {
    if(explored[index]->cost < board->cost) {
        //delete board;
    }
    else {
        visited.push_back(board);
        //explored[index] = board;
    }
}
int isVisited(std::vector<Board*> &visited, Board* board) {
    for (int i = 0; i < visited.size(); i++) {
        if (visited[i]->state == board->state) {
            return i;
        }
    }
    //delete board;
    return -1;
}

void checkVisited(std::vector<Board*> &visited, Board* board, int index) {
    if(visited[index]->cost < board->cost) {
        //delete board;
    }
    else {
        visited[index] = board;
    }
}

void addExplored(std::vector<Board*> &explored, Board* board) {
    explored.push_back(board);
    //map.insert(std::pair<std::string, bool>(stateToString(state), true));
}

void addIfNotExplored(std::vector<Board*> &explored, std::vector<Board*> &visited, Board* board) {
    int index = isVisited(visited, board);
    if (index >= 0) {
        checkVisited(visited, board, index);
    }
    index = isExplored(explored, board);
    printf("test\n");
    if (index >= 0) {
        checkExplored(explored, visited, board, index);
    }
}

int addChildren(std::vector<Board*> &explored, std::vector<Board*> &visited, Board* board, int &count) {
    int index = getEmptyTile(board->state);
    int exploredCount = 0;

    //Moving UP
    if (!(index <= 3)) {
        Board* newBoard = new Board(board->state, board, Direction::UP, board->depth + 1, board->cost + 1);

        newBoard->state[index] = newBoard->state[index - 4];
        newBoard->state[index - 4] = 0;
        int newBoardsCost = wholeManhattan(newBoard->state) + linearConflicts(newBoard->state);
        newBoard->cost += newBoardsCost;
        count++;
        addIfNotExplored(explored, visited, newBoard);
    }

    //Moving DOWN
    if (!(index >= 12)) {
        Board* newBoard = new Board(board->state, board, Direction::DOWN, board->depth + 1, board->cost + 1);

        newBoard->state[index] = newBoard->state[index + 4];
        newBoard->state[index + 4] = 0;

        int newBoardsCost = wholeManhattan(newBoard->state) + linearConflicts(newBoard->state);
        newBoard->cost += newBoardsCost;
        count++;
        addIfNotExplored(explored, visited, newBoard);
    }

    //Moving LEFT
    if (!(index % 4 == 0)) {
        Board* newBoard = new Board(board->state, board, Direction::LEFT, board->depth + 1, board->cost + 1);

        newBoard->state[index] = newBoard->state[index - 1];
        newBoard->state[index - 1] = 0;

        int newBoardsCost = wholeManhattan(newBoard->state) + linearConflicts(newBoard->state);
        newBoard->cost += newBoardsCost;
        count++;
        addIfNotExplored(explored, visited, newBoard);
    }

    //Moving RIGHT
    if (!(index % 4 == 3)) {
        Board* newBoard = new Board(board->state, board, Direction::RIGHT, board->depth + 1, board->cost + 1);

        newBoard->state[index] = newBoard->state[index + 1];
        newBoard->state[index + 1] = 0;

        int newBoardsCost = wholeManhattan(newBoard->state) + linearConflicts(newBoard->state);
        newBoard->cost += newBoardsCost;
        count++;
        addIfNotExplored(explored, visited, newBoard);
    }
    addExplored(explored, board);
    return exploredCount;
}
