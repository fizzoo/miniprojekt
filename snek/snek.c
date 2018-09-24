#include "game.h"
#include "simple_io.h"
#include <stdio.h>
#include <time.h>

int main() {
  struct game *game = game_new(8, 8);
  int inp, len;

  srand(time(NULL));
  game_reset(game);

  io_setup();
  struct timespec sleeplen = {0};
  print_game(game);
  while (1) {
    sleeplen.tv_nsec = 400000000;
    while (nanosleep(&sleeplen, &sleeplen))
      ;
    while (inp = get_input()) {
      make_turn(game, inp);
    }
    len = update(game);
    if (len < 0) {
      printf("\n\nYOU LOSE!\nScore: %d\n", -len);
      break;
    }
    print_game(game);
  }
}
