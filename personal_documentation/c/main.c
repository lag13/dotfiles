#include <stdio.h>

#include "library.h"

int main(int argc, char **argv) {
  printf("hello world!!!\n");
  // For this small example its not too hard to figure out where these
  // identifiers are coming from but you can see how that it can
  // quickly get hard to keep track of since the identifier conveys no
  // information about where it came from.
  if (!dir_equal(UP, DOWN)) {
    printf("the directions are NOT equal\n");
  }
  if (dir_equal(UP, UP)) {
    printf("the directions are equal\n");
  }
}
