#include "test.h"

int main() {

  ((unsigned long)-100)%9;

  // ASSERT(2147483598, ((unsigned)-100)/2);
  // ASSERT(9223372036854775758, ((unsigned long)-100)/2);
  // ASSERT(2, ((unsigned)-100)%7);
  // ASSERT(6, ((unsigned long)-100)%9);


  // printf("OK\n");
  return 0;
}
