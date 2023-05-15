/****************************
Maciej Gębala (CC BY-NC 4.0)
Plansza ver. 0.1
2023-03-30
****************************/
#pragma once
#include <stdio.h>
#include <stdbool.h>

int board[5][5];

const int win[28][4][2] = { 
  { {0,0}, {0,1}, {0,2}, {0,3} },
  { {1,0}, {1,1}, {1,2}, {1,3} },
  { {2,0}, {2,1}, {2,2}, {2,3} },
  { {3,0}, {3,1}, {3,2}, {3,3} },
  { {4,0}, {4,1}, {4,2}, {4,3} },
  { {0,1}, {0,2}, {0,3}, {0,4} },
  { {1,1}, {1,2}, {1,3}, {1,4} },
  { {2,1}, {2,2}, {2,3}, {2,4} },
  { {3,1}, {3,2}, {3,3}, {3,4} },
  { {4,1}, {4,2}, {4,3}, {4,4} },
  { {0,0}, {1,0}, {2,0}, {3,0} },
  { {0,1}, {1,1}, {2,1}, {3,1} },
  { {0,2}, {1,2}, {2,2}, {3,2} },
  { {0,3}, {1,3}, {2,3}, {3,3} },
  { {0,4}, {1,4}, {2,4}, {3,4} },
  { {1,0}, {2,0}, {3,0}, {4,0} },
  { {1,1}, {2,1}, {3,1}, {4,1} },
  { {1,2}, {2,2}, {3,2}, {4,2} },
  { {1,3}, {2,3}, {3,3}, {4,3} },
  { {1,4}, {2,4}, {3,4}, {4,4} },
  { {0,1}, {1,2}, {2,3}, {3,4} },
  { {0,0}, {1,1}, {2,2}, {3,3} },
  { {1,1}, {2,2}, {3,3}, {4,4} },
  { {1,0}, {2,1}, {3,2}, {4,3} },
  { {0,3}, {1,2}, {2,1}, {3,0} },
  { {0,4}, {1,3}, {2,2}, {3,1} },
  { {1,3}, {2,2}, {3,1}, {4,0} },
  { {1,4}, {2,3}, {3,2}, {4,1} }
};

const int lose[48][3][2] = {
  { {0,0}, {0,1}, {0,2} }, { {0,1}, {0,2}, {0,3} }, { {0,2}, {0,3}, {0,4} }, 
  { {1,0}, {1,1}, {1,2} }, { {1,1}, {1,2}, {1,3} }, { {1,2}, {1,3}, {1,4} }, 
  { {2,0}, {2,1}, {2,2} }, { {2,1}, {2,2}, {2,3} }, { {2,2}, {2,3}, {2,4} }, 
  { {3,0}, {3,1}, {3,2} }, { {3,1}, {3,2}, {3,3} }, { {3,2}, {3,3}, {3,4} }, 
  { {4,0}, {4,1}, {4,2} }, { {4,1}, {4,2}, {4,3} }, { {4,2}, {4,3}, {4,4} }, 
  { {0,0}, {1,0}, {2,0} }, { {1,0}, {2,0}, {3,0} }, { {2,0}, {3,0}, {4,0} }, 
  { {0,1}, {1,1}, {2,1} }, { {1,1}, {2,1}, {3,1} }, { {2,1}, {3,1}, {4,1} }, 
  { {0,2}, {1,2}, {2,2} }, { {1,2}, {2,2}, {3,2} }, { {2,2}, {3,2}, {4,2} }, 
  { {0,3}, {1,3}, {2,3} }, { {1,3}, {2,3}, {3,3} }, { {2,3}, {3,3}, {4,3} }, 
  { {0,4}, {1,4}, {2,4} }, { {1,4}, {2,4}, {3,4} }, { {2,4}, {3,4}, {4,4} }, 
  { {0,2}, {1,3}, {2,4} }, { {0,1}, {1,2}, {2,3} }, { {1,2}, {2,3}, {3,4} }, 
  { {0,0}, {1,1}, {2,2} }, { {1,1}, {2,2}, {3,3} }, { {2,2}, {3,3}, {4,4} }, 
  { {1,0}, {2,1}, {3,2} }, { {2,1}, {3,2}, {4,3} }, { {2,0}, {3,1}, {4,2} }, 
  { {0,2}, {1,1}, {2,0} }, { {0,3}, {1,2}, {2,1} }, { {1,2}, {2,1}, {3,0} }, 
  { {0,4}, {1,3}, {2,2} }, { {1,3}, {2,2}, {3,1} }, { {2,2}, {3,1}, {4,0} }, 
  { {1,4}, {2,3}, {3,2} }, { {2,3}, {3,2}, {4,1} }, { {2,4}, {3,3}, {4,2} }
};

void setBoard()
{
  for(int i=0; i<5; i++)
    for(int j=0; j<5; j++)
      board[i][j]=0;
}

void printBoard()
{
  printf("  1 2 3 4 5\n");
  for(int i=0; i<5; i++) {
    printf("%d",i+1);
    for(int j=0; j<5; j++ )
      switch(board[i][j]) {
        case 0: printf(" -"); break;
        case 1: printf(" O"); break;
        case 2: printf(" X"); break;
      }
    printf("\n");
  }
  printf("\n");
}

bool setMove(int move, int player)
{
  int i,j;
  i = (move/10)-1;
  j = (move%10)-1;
  if( (i<0) || (i>4) || (j<0) || (j>4) ) return false; 
  if( board[i][j]!=0 ) return false;
  board[i][j] = player;
  return true;
}

bool winCheck(int player)
{
  bool w=false;
  for(int i=0; i<28; i++) {
    if( (board[win[i][0][0]][win[i][0][1]]==player) && (board[win[i][1][0]][win[i][1][1]]==player) && (board[win[i][2][0]][win[i][2][1]]==player) && (board[win[i][3][0]][win[i][3][1]]==player) ) {
      w=true;
      break;
    }
  }
  return w;
}

bool loseCheck(int player)
{
  bool l=false;
  for(int i=0; i<48; i++) {
    if( (board[lose[i][0][0]][lose[i][0][1]]==player) && (board[lose[i][1][0]][lose[i][1][1]]==player) && (board[lose[i][2][0]][lose[i][2][1]]==player) ) {
      l=true;
      break;
    }
  }
  return l;
}

//Finding open-4-in-line pattern (e.g. | X | | X | | - | | X |)
bool open4(int player) 
{
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
