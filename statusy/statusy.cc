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
prettifying box?
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
    clear();
  }
  ~CursesWrap() {
    noraw();
    echo();
    endwin();
  }
  void Update() { getmaxyx(stdscr, maxy, maxx); }
  pair<int, int> GetBounds() { return {maxx, maxy}; }
};

enum class Style { CenterEachLine, LeftAdjust };

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

size_t LongestLine(string const &str) {
  size_t res = 0, cur = 0;
  auto it = str.cbegin(), end = str.cend();
  while (it != end) {
    if (*it == '\n') {
      res = cur > res ? cur : res;
      cur = 0;
    } else {
      ++cur;
    }
    ++it;
  }
  return res;
}

// Prints the entire str (which may be multiline), such that each new
// line still starts on column startx. Returns the number of the next
// empty line. The meaning of x depends on the style.
int PrintBuf(int starty, int midx, string const &str, Style stl) {
  auto *start = str.c_str(), *end = str.c_str() + str.size(),
       *line_start = start, *line_end = start;

  size_t longest_line;
  if (stl == Style::LeftAdjust) {
    longest_line = LongestLine(str);
  }

  while (line_start != end) {
    line_end = find(line_start, end, '\n');

    int startx;
    switch (stl) {
    case Style::CenterEachLine:
      startx = midx - (line_end - line_start) / 2;
      break;
    case Style::LeftAdjust:
      startx = midx - longest_line / 2;
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
  case 'x':
    clear();
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
  state.commands.emplace_back("toilet `date '+%T'`", Style::LeftAdjust, 'c');
  state.commands.emplace_back("date '+%T%n%F%n%A'", Style::CenterEachLine);
  state.commands.emplace_back("acpi", Style::CenterEachLine);
  state.commands.emplace_back("ip a", Style::LeftAdjust, 'i');
  state.commands.emplace_back("wpa_cli -i wlp4s0 status",
                              Style::LeftAdjust, 'w');

  while (!state.should_exit) {
    curses.Update();
    auto [maxx, maxy] = curses.GetBounds();

    vector<optional<string>> output;

    erase();
    for (auto &c : state.commands) {
      auto out = c.MaybeExec();
      output.push_back(out);
    }

    size_t num_lines = 0;
    for (auto const &c : output) {
      if (c) {
        num_lines += count(c->cbegin(), c->cend(), '\n') + 2;
      }
    }
    int y = maxy / 2 - num_lines / 2;
    for (int i = 0; i < state.commands.size(); ++i) {
      if (output[i]) {
        y = PrintBuf(y, maxx / 2, *output[i], state.commands[i].style) + 1;
      }
    }
    refresh();
    int key = GetKeyOrWait();
    if (key) {
      ActOnKey(&state, key);
    }
  }
}
