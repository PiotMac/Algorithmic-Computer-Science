#ifndef USEFUL_HPP
#define USEFUL_HPP

#include <string>
#include <unordered_map>
#include "useful.cpp"
bool isSolvable(std::vector<int> startingState);
void printBoard(std::vector<int> state);
int horizontalInversionDistance(std::vector<int> tiles);
int verticalInversionDistance(std::vector<int> tiles);
int calculateInversionDistance(std::vector<int> tiles);
int rowConflicts(std::vector<int> tiles);
int columnConflicts(std::vector<int> tiles);
int wholeManhattan(std::vector<int> tiles);
int manhattan(int x1, int y1, int x2, int y2);
int isVisited(std::vector<Board*> &visited, Board* board);
void checkVisited(std::vector<Board*> &visited, Board* board, int index);
int isExplored(std::vector<Board*> &explored, Board* board);
void checkExplored(std::vector<Board*> &explored, std::vector<Board*> &visited, Board* board, int index);
void addExplored(std::vector<Board*> &explored, Board* board);
void addIfNotExplored(std::vector<Board*> &explored, std::vector<Board*> &visited, Board* board);
std::string stateToString(std::vector<int> state);
int getEmptyTile(std::vector<int> state);
int addChildren(std::vector<Board*> &explored, std::vector<Board*> &visited, Board* board, int &count);

#endif