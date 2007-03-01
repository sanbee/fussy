#include <vector>
#include "defns.h"

Int dummy()
{
  return 0;
}

int main()
{
  extern VMac Prog;
  long int N=10;
  Prog.resize(N);
  Prog[0]=(Instruction)dummy;
}
