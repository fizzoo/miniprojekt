/**
 * Tiny program for outputting some status information.
 */

#include <ncurses.h>

#define INPUTS 8
#define BUFFERSIZE 2048

char buffer[BUFFERSIZE];
char *inp[INPUTS];
bool running = 1;

/**
 * Distance to first '\n' or '\0'.
 */
int linelen(char *p) {
  int res = 0;
  while (*p != '\n' && *p != '\0') {
    ++p;
    ++res;
  }
  return res;
}

/**
 * Pointer to first null character in array.
 */
char *firstnull(char *p) {
  while (*p != '\0') {
    ++p;
  }
  return p;
}

void get_status(char *buf) {
  FILE *fp;
  int sizeleft = BUFFERSIZE - 1;
  char a;

  for (int i = 0; i < INPUTS; ++i) {
    if (!inp[i]) {
      // No command in this slot
      continue;
    }

    fp = popen(inp[i], "r");
    if (!fp) {
      // Failed to find/open command
      continue;
    }

    while (sizeleft > 0 && (a = getc_unlocked(fp)) != EOF) {
      --sizeleft;
      *buf++ = a;
    }
    --sizeleft;
    *buf++ = 7;
    fclose(fp);
  }

  *buf = '\0';
}

void write_status(char *buf, int maxx, int maxy) {
  int y = maxy / 2 - INPUTS;
  int startx = maxx / 2 - linelen(buf) / 2;
  int x;

  while (*buf != '\0') {
    if (*buf == '\n') {
      ++buf;
      ++y;
      continue;
    }
    if (*buf == 7) {
      // End of one command, recalculate width.
      ++buf;
      ++y;
      startx = maxx / 2 - linelen(buf) / 2;
    }
    x = startx;

    while (*buf != '\0' && *buf != '\n') {
      mvaddch(y, x++, *buf++);
    }
  }
}

void doer() {
  int c = getch();
  switch (c) {
  case ERR:
    return;
  case 'q':
    running = false;
    break;
  case KEY_UP:
    mvaddch(10, 10, 'W');
    break;
  case KEY_DOWN:
    mvaddch(10, 11, 'S');
    break;
  case KEY_LEFT:
    mvaddch(10, 12, 'A');
    break;
  case KEY_RIGHT:
    mvaddch(10, 13, 'D');
    break;
  }
  refresh();
}

int main(void) {
  int maxx = 80, maxy = 80, tmp_maxx, tmp_maxy;

  inp[0] = "date";
  inp[1] = "acpi";
  inp[2] = "nmcli d | ag wifi";

  initscr();
  raw();
  keypad(stdscr, TRUE);
  curs_set(0);
  noecho();
  timeout(250);

  while (running) {
    getmaxyx(stdscr, tmp_maxy, tmp_maxx);
    if (tmp_maxx != maxx || tmp_maxy != maxy) {
      maxx = tmp_maxx;
      maxy = tmp_maxy;
      erase();
    }

    get_status(buffer);
    write_status(buffer, maxx, maxy);

    refresh();

    doer();
  }

  noraw();
  echo();
  endwin();
}
