#include "test.h"

_Bool true_fn();
_Bool false_fn();
char char_fn();
short short_fn();

int main() {
  ASSERT(1, true_fn());
  ASSERT(0, false_fn());
  ASSERT(3, char_fn());
  ASSERT(5, short_fn());

  printf("OK\n");
  return 0;
}
