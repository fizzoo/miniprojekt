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
  while (1) {
    print_game(game);
    inp = get_input();
    len = update(game, inp);
    if (len < 0) {
      printf("\n\nYOU LOSE!\nScore: %d\n", -len);
      break;
    }
  }
}
