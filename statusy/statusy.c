/**
 * Tiny program for outputting some status information.
 */

#include <ncurses.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>

#define NRINPUTS 8
#define BUFFERSIZE 4096

char buffer[BUFFERSIZE];
char *commands[NRINPUTS];
bool running = 1;

static char COMMAND_END = 7;

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
 * Average line length
 */
int avg_linelen(char *p){
  int sum = 0;
  int lines = 0;
  while (*p != '\0' && *p != COMMAND_END){
    if (*p == '\n'){
      ++lines;
    } else {
      ++sum;
    }
    ++p;
  }
  if (*(p-2) == '\n') --lines;  /* \n right before \0 */
  if (lines < 1) lines = 1;
  mvprintw(0, 0, "%d,%d,%d\n", sum, lines, sum/lines);
  return sum/lines;
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

    buf = read_command(commands[i], buf, COMMAND_END);
  }

  *buf = '\0';
}

/**
 * Writes the status information in buf, formatted for the screen.
 */
void write_status(char *buf, int maxx, int maxy) {
  int y = maxy / 2 - NRINPUTS;
  int startx = maxx / 2 - avg_linelen(buf) / 2;
  int x;

  while (*buf != '\0') {
    if (*buf == '\n') {
      ++buf;
      ++y;
      continue;
    }
    if (*buf == COMMAND_END) {
      // End of one command, recalculate width.
      ++buf;
      ++y;
      startx = maxx / 2 - avg_linelen(buf) / 2;
      continue;
    }
    x = startx;

    while (*buf != '\0' && *buf != '\n' && *buf != COMMAND_END) {
      if (y >= 0 && y < maxy && x >= 0 && x < maxx) {
        mvaddch(y, x, *buf);
      }
      ++x;
      ++buf;
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
      commands[3] = "wpa_cli -i wlp4s0 status";
    } else {
      commands[3] = NULL;
    }
    break;
  case 'i':
    if (!commands[4]) {
      commands[4] = "ip a";
    } else {
      commands[4] = NULL;
    }
    break;
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

  // Solves occasional timing problem where getmaxyx keeps invalid values by
  // saying something changed.
  raise(SIGWINCH);

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
