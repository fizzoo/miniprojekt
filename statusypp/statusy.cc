#include <algorithm>
#include <iostream>
#include <memory>
#include <ncurses.h>
#include <optional>
#include <unistd.h>
#include <variant>
#include <vector>

// Not a header file.... Mostly just lazy. The ncurses stuff is not
// namespaced either, so it is not very clean anyway.
using namespace std;

/* todo/thoughts for improvement:
Make a nice clock somehow. maybe just figlet.
Otherwise re-implement statusy in nicer code.
Have different styles for printing, e.g. left-adjusted, center-each-line, color?
prettifying box? (maybe skip) let one specify the commands and the style in a
lisp/json file
 */

// RAII driven NCurses init and exit
class CursesWrap {
private:
  int maxx, maxy;

public:
  CursesWrap() {
    initscr();
    raw();
    keypad(stdscr, TRUE);
    curs_set(0);
    noecho();
    timeout(100);
    Update();
  }
  ~CursesWrap() {
    noraw();
    echo();
    endwin();
  }
  void Update() { getmaxyx(stdscr, maxy, maxx); }
  pair<int, int> GetBounds() { return {maxx, maxy}; }
};

enum class Style { CenterEachLine, LeftAdjustCenterOfMass };

// https://stackoverflow.com/a/478960
string Exec(const char *cmd) {
  array<char, 128> buffer;
  string result;
  shared_ptr<FILE> pipe(popen(cmd, "r"), pclose);
  if (!pipe)
    throw std::runtime_error("popen() failed!");
  while (!feof(pipe.get())) {
    if (fgets(buffer.data(), 128, pipe.get()) != nullptr)
      result += buffer.data();
  }
  return result;
}

// Prints the entire str (which may be multiline), such that each new
// line still starts on column startx. Returns the number of the next
// empty line. The meaning of x depends on the style.
int PrintBuf(int starty, int x, const string &str, Style stl) {
  auto *start = str.c_str(), *end = str.c_str() + str.size(),
       *line_start = start, *line_end = start;

  while (line_start != end) {
    line_end = find(line_start, end, '\n');

    int startx;
    switch (stl) {
    case Style::CenterEachLine:
      startx = x - (line_end - line_start) / 2;
      break;
    case Style::LeftAdjustCenterOfMass:
      startx = x;
      break;
    }
    if (startx < 0) {
      startx = 0;
    }

    mvaddnstr(starty, startx, line_start, line_end - line_start);
    ++starty;
    line_start = line_end + 1;
  }

  return starty;
}

int main() {
  CursesWrap curses;
  auto [maxy, maxx] = curses.GetBounds();
  array<string, 4> commands = {"date '+%T'", "date '+%F'", "ip a",
                               "wpa_cli -i wlp4s0 status"};
  vector<string> output(commands.size());

  while (1) {
    transform(commands.begin(), commands.end(), output.begin(),
              [](const string &x) { return Exec(x.c_str()); });

    int x = 10, y = 5;
    for (auto &c : output) {
      y = PrintBuf(y, x, c, Style::CenterEachLine);
    }
    refresh();
    sleep(1);
  }
}
