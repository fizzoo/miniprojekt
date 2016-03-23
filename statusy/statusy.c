/**
 * Tiny program for outputting some status information.
 */

#include <ncurses.h>

#define INPUTS 8

char buf[1024];
char *inp[INPUTS];
char *date = "date";
char *acpi = "acpi";

/**
 * Distance to first '\n' or '\0'.
 */
int _strlen(char *p) {
  int res = 0;
  while (*p != '\n' && *p != '\0') {
    ++p;
    ++res;
  }
  return res;
}

void write_status(void) {
  FILE *fp;
  char *s;
  int wx, wy, strwidth;

  getmaxyx(stdscr, wy, wx);

  for (int i = 0; i < INPUTS; ++i) {
    s = inp[i];
    if (!s) {
      continue;
    }

    fp = popen(s, "r");
    if (!fp) {
      continue;
    }

    fgets(buf, sizeof(buf) - 1, fp); // hope it fits
    fclose(fp);

    strwidth = _strlen(buf);
    mvaddstr(i + wy / 2 - INPUTS, wx / 2 - strwidth / 2, buf);
  }
  refresh();
}

int main(void) {
  initscr();
  cbreak();
  keypad(stdscr, TRUE);
  curs_set(0);

  inp[0] = date;
  // inp[1] = acpi;

  for (;;) {
    write_status();

    timeout(250);
  }

cleanup:
  nocbreak();
  endwin();
}
