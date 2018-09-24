#include "game.h"
#include "simple_io.h"
#include <stdio.h>
#include <time.h>

int main(int argc, char **argv) {
  size_t height = 8, width = 8;
  if (argc > 1) {
    if (argc != 3) {
      printf("Usage: %s height width\n", argv[0]);
      return -1;
    }

    height = strtol(argv[1], NULL, 10);
    width = strtol(argv[2], NULL, 10);
  }

  struct game *game = game_new(height, width);
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
    while ((inp = get_input())) {
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
