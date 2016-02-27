#include <ncurses.h>
#include <string>
#include <cstdlib>
#include "board.hpp"



int curx, cury, oldx, oldy, maxx, maxy;

int drawspot(board bor, int x, int y){
    int ch = bor.imgat(x, y);
    switch (ch) {
        case HIDDEN:
            ch = '_';
            mvaddch(y + 1, x, ch);
            break;
        case FLAG:
            ch = 'P';
            mvaddch(y + 1, x, ch | COLOR_PAIR(3));
            break;
        case MINE:
            ch = '*';
            mvaddch(y + 1, x, ch | COLOR_PAIR(1));
            break;
        case M0:
            ch = ' ';
            mvaddch(y + 1, x, ch | COLOR_PAIR(2));
            break;
        default:
            ch += '0';
            mvaddch(y + 1, x, ch | COLOR_PAIR(2));
    }
    return ch;
}

void drawspotcolor(board bor, int x, int y){
    int ch = drawspot(bor, x, y);
    mvaddch(y + 1, x, ch | A_BOLD | COLOR_PAIR(1));
}

void drawall(board bor){
    for (int y = 0; y < bor.sizey; y++){
        for (int x = 0; x < bor.sizex; x++){
            drawspot(bor, x, y);
        }
    }
    refresh();
}

struct dim {int x; int y; int mines;};
dim modechoice(){
    while(1){
        clear();
        printw("(b)eginner, (i)ntermediate, (e)xpert or (c)ustom.. (q)uit");
        int mode = getch();

        switch(mode) {
            case 'b':
                return {8, 8, 10};
            case 'i':
                return {16, 16, 40};
            case 'e':
                return {30, 16, 99};
            case 'c':
                clear();
                printw("x, y, mines w/ newline\n");
                {
                    echo();
                    char x[10]; getstr(x); int ix = atoi(x); if (ix <= 0) {continue;};
                    char y[10]; getstr(y); int iy = atoi(y); if (iy <= 0) {continue;};
                    char m[10]; getstr(m); int im = atoi(m); if (im <= 0) {continue;};
                    noecho();
                    return {ix, iy, im};
                }
            case 'q':
                return {-1, -1, -1};
        }
    }
}

void moveto(int x, int y){
    if (x < 0 || y < 0 || x >= maxx || y >= maxy) {
        return;
    }

    curx = x;
    cury = y;
}

int input(){
    int ch = getch();
    switch (ch) {
        case KEY_LEFT:
            moveto(curx-1,  cury);
            return 1;
        case KEY_RIGHT:
            moveto(curx+1, cury);
            return 1;
        case KEY_DOWN:
            moveto(curx, cury+1);
            return 1;
        case KEY_UP:
            moveto(curx, cury-1);
            return 1;
        case 'c':
            return 2;
        case 'z':
            return 3;
        case 'x':
            return 4;
        case ' ':
            return 32;
        case 'q':
            return 31;
    }
    return -1;
}

int main()
{
    initscr();
    start_color();
    init_pair(1, COLOR_RED, COLOR_BLACK);
    init_pair(2, COLOR_WHITE, COLOR_GREEN);
    init_pair(3, COLOR_WHITE, COLOR_MAGENTA);
    raw();
    keypad(stdscr, TRUE);
    noecho();


    bool quit = false;
    while(!quit){
        clear();
        dim curdim = modechoice();
        if (curdim.x == -1){
            quit = true;
            continue;
        }

        board bor = board(curdim.x, curdim.y, curdim.mines);
        curx = 0; cury = 0; oldx = 0, oldy = 0;
        maxx = curdim.x; maxy = curdim.y;
        bool loss = false;

        clear();
        drawall(bor);
        drawspotcolor(bor, 0, 0);

        while(!loss){
            int in = input();

            switch (in) {
                case 1:
                    drawspot(bor, oldx, oldy);
                    drawspotcolor(bor, curx, cury);
                    oldx = curx; oldy = cury;
                    break;
                case 2:
                    {
                        int res = bor.open(curx, cury);
                        if (res < 0) {
                            loss = true;
                        } else {
                            if (res == 8) {
                                drawall(bor);
                            }
                            drawspotcolor(bor, curx, cury);
                            refresh();
                        }
                    }
                    break;
                case 3:
                    bor.flag(curx, cury);
                    drawspotcolor(bor, curx, cury);
                    break;
                case 4:
                    {
                        int res = bor.midmouse(curx, cury);
                        if (res > 0) { //Got something
                            drawall(bor);
                            drawspotcolor(bor, curx, cury);
                            refresh();
                        } else if (res < 0) { //Loss
                            loss = true;
                        }
                    }
                    break;
                case 32:
                    //flag if hidden
                    bor.flag(curx, cury);

                    //midmouse on all
                    {
                        int diff = 1;
                        while (diff > 0){ //Keeps going until no difference is made on a pass
                            diff = 0;
                            for (int i = 0; i < maxy; i++){
                                for (int j = 0; j < maxx; j++){
                                    diff += bor.midmouse(j, i);
                                    if (diff < 0) {
                                        loss = true;
                                    }
                                }
                            }
                        }
                    }
                    drawall(bor);
                    drawspotcolor(bor, curx, cury);
                    refresh();
                    break;
                case 31:
                    loss = true;
                    quit = true;
                    break;
            }
            mvprintw(0, 0, "mines: ");
            mvprintw(0, 7, std::to_string(curdim.mines - bor.flaggedtot).c_str());
            refresh();
        }
        bor.revealall();

        drawall(bor);

        getch();
    }

    echo();
    endwin();
    return 0;
}
