/**
 * Tiny program for outputting some status information.
 */

#include <ncurses.h>
#include <locale.h>

#define INPUTS 8
#define BUFFERSIZE 2048

char buffer[BUFFERSIZE];
char *inp[INPUTS];

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
      if (*buf == 230) {
        ++buf;
      }
      mvaddch(y, x++, *buf++);
    }
  }
}

int main(void) {
  int maxx, maxy;
  inp[0] = "date";
  inp[1] = "acpi";
  inp[2] = "nmcli d | ag wifi";

  setlocale(LC_ALL, "");
  initscr();
  cbreak();
  keypad(stdscr, TRUE);
  curs_set(0);

  // inp[1] = acpi;

  for (;;) {
    getmaxyx(stdscr, maxy, maxx);

    erase();
    get_status(buffer);
    write_status(buffer, maxx, maxy);

    refresh();

    timeout(250);
  }

cleanup:
  nocbreak();
  endwin();
}
