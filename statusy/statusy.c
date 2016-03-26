/**
 * Tiny program for outputting some status information.
 */

#include <ncurses.h>
#include <fcntl.h>
#include <unistd.h>

#define NRINPUTS 8
#define BUFFERSIZE 4096

char buffer[BUFFERSIZE];
char *commands[NRINPUTS];
bool running = 1;

/**
 * Redirects stderr to the file specified by filename.
 */
void redirect_err_to(char *filename) {
  int fd;

  fd = open(filename, O_WRONLY);
  dup2(fd, 2);
  close(fd);
}

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

/**
 * Executes the command and places the resulting string in buf, followed by
 * exiter, and returns a pointer just beyond that.
 */
char *read_command(char *command, char *buf, char exiter) {
  FILE *fp;
  char a, *start = buf;

  fp = popen(command, "r");
  if (!fp) {
    return buf;
  }

  while ((a = getc_unlocked(fp)) != EOF) {
    *buf++ = a;
  }
  if (start != buf) {
    // Wrote something
    *buf++ = exiter;
  }
  pclose(fp);

  return buf;
}

/**
 * Iterates through the available commands, executes the commands and gathers
 * their output to buf.
 */
void get_status(char *buf) {
  for (int i = 0; i < NRINPUTS; ++i) {
    if (!commands[i]) {
      // No command in this slot
      continue;
    }

    buf = read_command(commands[i], buf, 7);
  }

  *buf = '\0';
}

/**
 * Writes the status information in buf, formatted for the screen.
 */
void write_status(char *buf, int maxx, int maxy) {
  int y = maxy / 2 - NRINPUTS;
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
      continue;
    }
    x = startx;

    while (*buf != '\0' && *buf != '\n' && *buf != 7) {
      mvaddch(y, x++, *buf++);
    }
  }
}

/**
 * Handles user input.
 */
void doer() {
  int c = getch();
  switch (c) {
  case ERR:
    return;
  case 'q':
    running = false;
    break;
  case 'w':
    if (!commands[3]) {
      commands[3] = "nmcli d wifi";
    } else {
      commands[3] = NULL;
    }
  }
}

int main(void) {
  int maxx, maxy;

  redirect_err_to("/dev/null");

  commands[0] = "date";
  commands[1] = "acpi";

  initscr();
  raw();
  keypad(stdscr, TRUE);
  curs_set(0);
  noecho();
  timeout(100);

  while (running) {
    getmaxyx(stdscr, maxy, maxx);

    get_status(buffer);
    write_status(buffer, maxx, maxy);

    refresh();

    doer();
    erase();
  }

  noraw();
  echo();
  endwin();
}
