/**
 * Tiny program for outputting some status information.
 */

#include <ncurses.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>

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

struct buf_statistics {
  int lines[NRINPUTS];
  int letters[NRINPUTS];
  int avglen[NRINPUTS];
  int found_cmds;
};

/**
 * Create a buf_statistics with the number of lines and letters of each of the commands of the input buffer.
 */
struct buf_statistics get_statistics(char *buf){
  struct buf_statistics stats;
  memset(&stats, 0, sizeof(stats));
  int cur_cmd = 0;
  char c;

  while ((c = *buf++) != '\0'){
    if (c == COMMAND_END){
      ++cur_cmd;
      stats.found_cmds = cur_cmd;
    } else if (c == '\n'){
      ++stats.lines[cur_cmd];
    } else {
      ++stats.letters[cur_cmd];
    }
  }

  for (int i = 0; i < NRINPUTS; ++i){
    if (stats.lines[i] < 1){
      stats.lines[i] = 1;
    }
    stats.avglen[i] = stats.letters[i] / stats.lines[i];
  }

  return stats;
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
  struct buf_statistics stats = get_statistics(buf);

  int total_lines = 0;
  for (int i = 0; i < stats.found_cmds; ++i){
    total_lines += stats.lines[i] + 1;
  }

  int cur_cmd = 0;
  int x;
  int y = maxy / 2 - total_lines / 2;
  int startx = maxx / 2 - stats.avglen[cur_cmd] / 2;

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
      ++cur_cmd;
      if (cur_cmd >= stats.found_cmds){
        break;
      }
      startx = maxx / 2 - stats.avglen[cur_cmd] / 2;
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
