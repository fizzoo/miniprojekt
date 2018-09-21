#include "simple_io.h"

/* https://stackoverflow.com/a/448982 */
/* Need to change the mode to get char-by-char input, and reset to fix
   terminal later. */
struct termios orig_termios;
void reset_terminal_mode() {
  tcsetattr(0, TCSANOW, &orig_termios);
}
void set_rawer_terminal_mode() {
  struct termios new_termios;

  /* take two copies - one for now, one for later */
  tcgetattr(0, &orig_termios);
  new_termios = orig_termios;

  /* register cleanup handler, and set the new terminal mode */
  atexit(reset_terminal_mode);
  cfmakeraw(&new_termios);

  /* Fix so that newlines also do carriage return and shit, don't want to edit output. */
  new_termios.c_oflag = orig_termios.c_oflag;
  /* Let me exit with ctrl-c, etc. */
  new_termios.c_lflag |= ISIG;

  tcsetattr(0, TCSANOW, &new_termios);
}
int kbhit() {
  struct timeval tv = {0L, 0L};

  /* Make a set of file descriptors with only stdin. */
  fd_set fds;
  FD_ZERO(&fds);
  FD_SET(0, &fds);

  /* Check if fds is ready for reading currently. */
  return select(1, &fds, NULL, NULL, &tv);
}
int getch() {
  int r;
  unsigned char c;

  r = read(0, &c, sizeof(c));

  if (r < 0) {
    return r;
  } else {
    return c;
  }
}

void io_setup() { set_rawer_terminal_mode(); }

int get_input() {
  if (1){
    int c = getch();
    switch (c){
    case 'h':
      return TURN_LEFT;
    case 'j':
      return TURN_DOWN;
    case 'k':
      return TURN_UP;
    case 'l':
      return TURN_RIGHT;
    default:
      return 0;
    }
  } else {
    return 0;
  }
}

void print_game(struct game *game) {
  char *line = malloc(sizeof(char) * game->width + 3);
  line[game->width + 2] = 0;

  for (size_t x = 1; x < game->width + 1; ++x) {
    line[x] = '-';
  }
  line[0] = '/';
  line[game->width + 1] = '\\';
  puts(line);

  line[0] = '|';
  line[game->width + 1] = '|';
  for (size_t y = 0; y < game->height; ++y) {
    for (size_t x = 0; x < game->width; ++x) {
      char v = *game_at(game, &(struct pos){x, y});
      char d = ' ';
      switch (v) {
      case BOARD_SNAKE:
        d = 'o';
        break;
      case BOARD_FOOD | BOARD_SNAKE:
        d = 'O';
        break;
      case BOARD_FOOD:
        d = 'x';
        break;
      }

      line[x + 1] = d;
    }
    puts(line);
  }

  for (size_t x = 1; x < game->width + 1; ++x) {
    line[x] = '-';
  }
  line[0] = '\\';
  line[game->width + 1] = '/';
  puts(line);

  puts("\n");
  free(line);
}
