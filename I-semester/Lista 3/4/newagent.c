#include <stdio.h>
#include "agents.h"
struct agent newagent(int x, int y)
{
 struct agent newagent;
 newagent.xwsp=x;
 newagent.ywsp=y;
 return newagent;
};
