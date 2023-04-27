#include <math.h>
#include "agents.h"
double distance(struct agent a1, struct agent a2)
{
 return sqrt(pow(a1.xwsp-a2.xwsp, 2) + pow(a1.ywsp-a2.ywsp, 2));
}
