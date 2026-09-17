#include "test.h"

int main() {
  0.0;
  1.0;
  3e+8;
  0x10.1p0;
  .1E4f;

  // ASSERT(4, sizeof(8f));
  ASSERT(4, sizeof(0.3F));
  ASSERT(8, sizeof(0.));
  ASSERT(8, sizeof(.0));
  ASSERT(8, sizeof(5.l));
  ASSERT(8, sizeof(2.0L));

  printf("OK\n");
  return 0;
}
