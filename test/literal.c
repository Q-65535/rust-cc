#include "test.h"

int main() {
  ASSERT(97, 'a');
  ASSERT(10, '\n');
  ASSERT(128, '\x80');
  ASSERT(-128,  ({ char a = '\x80'; a;}));

  printf("OK\n");
  return 0;
}
