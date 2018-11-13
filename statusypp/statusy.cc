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
string Exec(char const *cmd) {
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

bool EndsWith(std::string const &str, std::string const &ending) {
  if (ending.size() > str.size()) {
    return false;
  }
  return equal(ending.rbegin(), ending.rend(), str.rbegin());
}

class Command {
public:
  bool active = true;
  char toggle_key = 0;
  string command_string;
  Style style;
  optional<string> MaybeExec() {
    if (!active) {
      return {};
    }
    auto combined_string = command_string + " 2>&1";

    auto output = Exec(combined_string.c_str());
    if (EndsWith(output, "command not found\n")) {
      return {};
    }
    return output;
  }
  Command(string command_string_, Style style_, char toggle_key_ = 0)
      : command_string(command_string_), style(style_) {
    if (toggle_key_ != 0) {
      active = false;
      toggle_key = toggle_key_;
    }
  }
};

class State {
public:
  bool should_exit = false;
  vector<Command> commands;
};

// Prints the entire str (which may be multiline), such that each new
// line still starts on column startx. Returns the number of the next
// empty line. The meaning of x depends on the style.
int PrintBuf(int starty, int x, string const &str, Style stl) {
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

int GetKeyOrWait() {
  int c = getch();
  switch (c) {
  case ERR:
    return 0;
  default:
    return c;
  }
}

constexpr char Ctrl(char c) { return c & 31; }

void ActOnKey(State *state, int key) {
  switch (key) {
  case 'q':
  case Ctrl('c'):
    state->should_exit = true;
    return;
  }
  for (auto &c : state->commands) {
    if (c.toggle_key && c.toggle_key == key) {
      c.active = !c.active;
      return;
    }
  }
}

int main() {
  CursesWrap curses;
  State state;
  auto [maxy, maxx] = curses.GetBounds();
  state.commands.emplace_back("date '+%T%n%F'", Style::CenterEachLine);
  state.commands.emplace_back("acpi", Style::CenterEachLine);
  state.commands.emplace_back("ip a", Style::LeftAdjustCenterOfMass, 'i');
  state.commands.emplace_back("wpa_cli -i wlp4s0 status",
                              Style::LeftAdjustCenterOfMass, 'w');

  while (!state.should_exit) {
    vector<string> output;

    int x = 30, y = 5;
    erase();
    for (auto &c : state.commands) {
      auto out = c.MaybeExec();
      if (out) {
        y = PrintBuf(y, x, *out, c.style) + 1;
      }
    }
    refresh();
    int key = GetKeyOrWait();
    if (key) {
      ActOnKey(&state, key);
    }
  }
}
