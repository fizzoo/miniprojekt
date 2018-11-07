#include <ncurses.h>
#include <optional>
#include <variant>

//Not a header file.... Mostly just lazy. The ncurses stuff is not
//namespaced either, so it is not very clean anyway.
using namespace std;

/* todo/thoughts for improvement:
Make a nice clock somehow. maybe just figlet.
Otherwise re-implement statusy in nicer code.
Have different styles for printing, e.g. left-adjusted, center-each-line, color? prettifying box?
(maybe skip) let one specify the commands and the style in a lisp/json file
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
