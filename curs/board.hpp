#include <vector>
#include <cstdlib>
#include <time.h>

enum {
    M0 = 0,
    M1 = 1,
    M2 = 2,
    M3 = 3,
    M4 = 4,
    M5 = 5,
    M6 = 6,
    M7 = 7,
    M8 = 8,
    FLAG,
    MINE,
    HIDDEN};


class board;
typedef int (board::*boardfunc)(int, int);


class board {
    private:
        std::vector<std::vector<bool> > mine;
        std::vector<std::vector<bool> > opened;
        std::vector<std::vector<bool> > flagged;
        bool inited;
        int ismineat(int, int);
        int minesaround(int, int);
        int isflagat(int, int);
        int flagsaround(int, int);
    public:
        board(int, int, int);
        void initmines(int);
        void revealall();
        int flaggedtot;
        int sizey;
        int sizex;
        int mines;
        bool inbounds(int, int);
        int open(int, int);
        int flag(int, int);
        int midmouse(int, int);
        int applyaround(boardfunc, int, int);
        char imgat(int, int);
};

