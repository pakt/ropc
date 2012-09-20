/**

   Tester for pivot parsing.

*/

#include "pivot.h"
#include <fstream>
#include <iostream>

int main() {
  fstream f;
  pivot_set ps;
  pivot_set::iterator i;

  f.open("in");
  if (!f.is_open()) {
    cerr << "Could not open input file" << endl;
    exit(1);
  }

  ps = PIVOT_parseinput(f);

  for (i = ps.begin(); i != ps.end(); i++) {
    cerr << *i << endl;
  }
  
}
