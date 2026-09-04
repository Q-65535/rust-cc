#include "test.h"

int main() {

  extern int ext3;
  if (ext3 == 7) {
    printf("OK\n");
  }
  return 0;
}
