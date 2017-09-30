#include <stdbool.h>

#include "library.h"

const dir UP = { .x = 0, .y = -1 };
const dir DOWN = { .x = 0, .y = 1 };
const dir LEFT = { .x = -1, .y = 0 };
const dir RIGHT = { .x = 1, .y = 0 };
const dir PAUSED = { .x = 0, .y = 0 };

bool dir_equal(dir a, dir b) {
  return a.x == b.x && a.y == b.y;
}
