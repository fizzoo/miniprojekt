#include <ncurses.h>
#include <optional>
#include <variant>

//Not a header file.... Mostly just lazy. The ncurses stuff is not
//namespaced either, so it is not very clean anyway.
using namespace std;

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
  void Update(){
    getmaxyx(stdscr, maxy, maxx);
  }
  pair<int, int> GetBounds(){
    return {maxx, maxy};
  }
};

int main() {
  CursesWrap curses;
  auto [y, x] = curses.GetBounds();
  printf("hello.\n %d %d", y, x);
}
