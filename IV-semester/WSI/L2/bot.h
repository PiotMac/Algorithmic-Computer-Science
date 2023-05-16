#include <string.h>
#include <stdlib.h>
#include "board.h"

int depth, turn;
int MAXIMIZER = 1;
int MINIMIZER = 2;
int INFINITY_MAX = 100;
int INFINITY_MIN = -100;
int minimax(int board[5][5], int player, int move, int alpha, int beta);
int evaluateBoard(int board[5][5], int player, int depth);
bool open4(int player);
bool open5(int player);
bool opponent_two_in_line(int player);

//Function finding the best move on the board
int bestMove(int player) {
    turn = player;
    int best_move = 0;
    int counter = 0;
    int max;
    max = (turn == MAXIMIZER) ? INFINITY_MIN : INFINITY_MAX;
    for(int i = 0; i < 5; i++) {
        for(int j = 0; j < 5; j++) {
            if(board[i][j] == 0) {
                board[i][j] = player;
                int temp = minimax(board, 3 - turn, 1, INFINITY_MIN, INFINITY_MAX);
                board[i][j] = 0;
                if (turn == MAXIMIZER) {
                    if(temp > max) {
                        counter++;
                        max = temp;
                        best_move = i * 10 + j;
                    }
                }
                else {
                    if(temp < max) {
                        counter++;
                        max = temp;
                        best_move = i * 10 + j;
                    }
                }
            }
        }
    }
    if (counter == 0) {
        printf("EVERY MOVE LOSES!\nChoosing first legal move...\n");
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                if (board[i][j] == 0) {
                    best_move = i * 10 + j;
                    return best_move + 11;
                }
            }
        }
    }
    return best_move + 11;
}


//Minimax algorithm using alpha-beta pruning
int minimax(int board[5][5], int player, int move, int alpha, int beta) {
    //Checking for terminal nodes
    //Checking if the player has won
    if(winCheck(3 - player)) {
        return (3 - player == MAXIMIZER) ? (int) (INFINITY_MAX * (1.0 - 0.1 * (move - 1))) : (int) (INFINITY_MIN * (1.0 - 0.1 * (move - 1)));
    }
    //Checking if the player has lost
    if(loseCheck(3 - player))  {
        return (3 - player == MAXIMIZER) ? (int) (INFINITY_MIN * (1.0 - 0.1 * (move - 1))) : (int) (INFINITY_MAX * (1.0 - 0.1 * (move - 1)));
    }
    //If the node is a leaf -> evaluate the board
    if(move == depth) {
        return evaluateBoard(board, 3 - player, move - 1);
    }

    //Making next move
    int best = (player == MAXIMIZER) ? INFINITY_MIN : INFINITY_MAX;
    //Meanwhile checking if there are any moves possible
    bool possible_move_check = false;
    if(player == MAXIMIZER) {
        for(int i = 0; i < 5; i++) {
            for(int j = 0; j < 5; j++) {
                if(board[i][j] == 0) {
                    possible_move_check = true;

                    board[i][j] = player;
                    int temp = minimax(board, 3 - player, move + 1, alpha, beta);
                    board[i][j] = 0;
                    if (temp > best) {
                        best = temp;
                    }
                    if (best > alpha) {
                        alpha = best;
                    }
                    if (beta <= alpha) {
                        break;
                    }
                }
            }
        }
        if(!possible_move_check) return 0;
        return best;
    }
    else {
        for(int i = 0; i < 5; i++) {
            for(int j = 0; j < 5; j++) {
                if(board[i][j] == 0) {
                    possible_move_check = true;

                    board[i][j] = player;
                    int temp = minimax(board, 3 - player, move + 1, alpha, beta);
                    board[i][j] = 0;

                    if (temp < best) {
                        best = temp;// + 10 * move;
                    }
                    if (best < beta) {
                        beta = best;
                    }
                    if (beta <= alpha) {
                        break;
                    }
                }
            }
        }
        if(!possible_move_check) return 0;
        return best;
    }
}

//Function to evaluate the board
int evaluateBoard(int board[5][5], int player, int depth) {
    //Firstly, checking if on the current board there is a loss-threat (finding pattern e.g. | O | | - | | 0 | | 0 |)
    if(open4(3 - player)) {
        return (player == MAXIMIZER) ? (int) (INFINITY_MIN * (1.0 - 0.1 * depth)) : (int) (INFINITY_MAX * (1.0 - 0.1 * depth));
    }
    //Then, checking if the player that has made a move has a pattern e.g. | X | | - | | X | | X |
    //It is a big advantage and makes the opponent block our line
    if (open4(player)) {
        return (player == MAXIMIZER) ? (int) (0.8 * INFINITY_MAX * (1.0 - 0.1 * depth)) : (int) (0.8 * INFINITY_MIN * (1.0 - 0.1 * depth));
    }
    //Secondly, checking if the opponent has a pattern e.g. | O | | - | | O | | - | | O |
    //For the opponent it is a disadvantage because he cannot put his symbol on two squares
    //It makes his game harder, that is why for us it is a advantage
    if (open5(3 - player)) {
        return (player == MAXIMIZER) ? (int) (0.5 * INFINITY_MAX * (1.0 - 0.1 * depth)) : (int) (0.5 * INFINITY_MIN * (1.0 - 0.1 * depth));
    }
    //Thirdly, checking the same as above but for us
    //If it is true, then it is a disadvantage
    if (open5(player)) {
        return (player == MAXIMIZER) ? (int) (0.4 * INFINITY_MIN * (1.0 - 0.1 * depth)) : (int) (0.4 * INFINITY_MAX * (1.0 - 0.1 * depth));
    }
    //Then, if we don't have any better moves and we are under no threat
    //We want to block any attempt of our opponent at creating such patterns: (e.g.) | O | | - | | O | | O |
    //If we detect that our opponent has two symbols in line (like this: | O | | O | | - | | - |)
    //Then, we put our symbol in this line (e.g.: | O | | O | | - | | X |) effectively eliminating further threats
    if (opponent_two_in_line(player)) {
        return (player == MAXIMIZER) ? (int) (0.3 * INFINITY_MIN * (1.0 - 0.1 * depth)) : (int) (0.3 * INFINITY_MAX * (1.0 - 0.1 * depth));
    }
    //Same as above, but for us
    //It is advantageous as we can then create threats using those two-in-line line-ups
    if (opponent_two_in_line(3 - player)) {
        return (player == MAXIMIZER) ? (int) (0.2 * INFINITY_MAX * (1.0 - 0.1 * depth)) : (int) (0.2 * INFINITY_MIN * (1.0 - 0.1 * depth));
    }
    return 0;
}

//Finding open-4-in-line pattern (e.g. | X | | X | | - | | X |)
bool open4(int player) {
  bool o=false;
  for (int i=0; i<28; i++) {
    if( (board[win[i][0][0]][win[i][0][1]] == player) && (board[win[i][1][0]][win[i][1][1]]==0) && (board[win[i][2][0]][win[i][2][1]]==player) && (board[win[i][3][0]][win[i][3][1]]==player) ) {
      o=true;
      return o;
    }
    if ( (board[win[i][0][0]][win[i][0][1]] == player) && (board[win[i][1][0]][win[i][1][1]]==player) && (board[win[i][2][0]][win[i][2][1]]==0) && (board[win[i][3][0]][win[i][3][1]]==player) ) {
      o=true;
      return o;
    }
  }
  return o;
}

//Finding if opponent has 2-in-line pattern (e.g. | O | | O | | - | | - |)
bool opponent_two_in_line(int player) {
  bool t=false;
  for (int i=0; i<28; i++) {
    if ( (board[win[i][0][0]][win[i][0][1]] == 3 - player) && (board[win[i][1][0]][win[i][1][1]]==3 - player) && (board[win[i][2][0]][win[i][2][1]]== 0) && (board[win[i][3][0]][win[i][3][1]]== 0) ) {
      t=true;
      return t;
    }
    if ( (board[win[i][0][0]][win[i][0][1]] == 0) && (board[win[i][1][0]][win[i][1][1]]== 0) && (board[win[i][2][0]][win[i][2][1]]== 3 - player) && (board[win[i][3][0]][win[i][3][1]]== 3 - player) ) {
      t=true;
      return t;
    }
    if ( (board[win[i][0][0]][win[i][0][1]] == 3 - player) && (board[win[i][1][0]][win[i][1][1]]== 0) && (board[win[i][2][0]][win[i][2][1]]== 3 - player) && (board[win[i][3][0]][win[i][3][1]]== 0) ) {
      t=true;
      return t;
    }
    if ( (board[win[i][0][0]][win[i][0][1]] == 0) && (board[win[i][1][0]][win[i][1][1]]== 3 - player) && (board[win[i][2][0]][win[i][2][1]]== 0) && (board[win[i][3][0]][win[i][3][1]]== 3 - player) ) {
      t=true;
      return t;
    }
    if ( (board[win[i][0][0]][win[i][0][1]] == 3 - player) && (board[win[i][1][0]][win[i][1][1]]== 0) && (board[win[i][2][0]][win[i][2][1]]== 0) && (board[win[i][3][0]][win[i][3][1]]== 3 - player) ) {
      t=true;
      return t;
    }
  }

  return t;
}

//Finding if the player has open-5-in-line pattern (e.g. | X | | - | | X | | - | | X |)
bool open5(int player) {
  bool o=false;
  for (int i = 0;i < 5; i++) {
    if (board[i][0] == player && board[i][1] == 0 && board[i][2] == player && board[i][3] == 0 && board[i][4] == player) {
      o=true;
      return o;
    }
    if (board[0][i] == player && board[1][i] == 0 && board[2][i] == player && board[3][i] == 0 && board[4][i] == player) {
      o=true;
      return o;
    }
  }
  if (board[0][0] == player && board[1][1] == 0 && board[2][2] == player && board[3][3] == 0 && board[4][4] == player) {
    o=true;
    return o;
  }
  if (board[0][4] == player && board[1][3] == 0 && board[2][2] == player && board[3][1] == 0 && board[4][0] == player) {
    o=true;
    return o;
  }
  return o;
}
