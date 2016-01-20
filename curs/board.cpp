#include "board.hpp"

board::board(int x, int y, int minen) : inited(false), flaggedtot(0), sizey(y), sizex(x), mines(minen) {
    mine = std::vector<std::vector<bool> >();
    mine.resize(y);
    opened = std::vector<std::vector<bool> >();
    opened.resize(y);
    flagged = std::vector<std::vector<bool> >();
    flagged.resize(y);
    for(int i = 0; i < y; i++){
        mine[i].resize(x);
        opened[i].resize(x);
        flagged[i].resize(x);
    };
}


int board::flag(int x, int y) {
    if (inbounds(x, y) && !opened[y][x]){
        if (flagged[y][x]){
            flagged[y][x] = false;
            flaggedtot--;
        } else {
            flagged[y][x] = true;
            flaggedtot++;
        }
        return 1;
    } else {
        return 0;
    }
}

int board::open(int x, int y) {
    if(inbounds(x, y) && !opened[y][x] && !flagged[y][x]){
        opened[y][x] = true;
        if (mine[y][x]){
            return -10000;
        } else if (!inited){
            initmines(mines);
            inited = true;
        }
        if (minesaround(x, y) == 0) {
            applyaround(&board::open, x, y);
            return 8;
        }
        return 1;
    }
    return 0;
}

int board::midmouse(int x, int y) {
    if (!inbounds(x, y) || !opened[y][x] || flagged[y][x]){
        return 0;
    } else if (minesaround(x, y) == flagsaround(x, y)) {
        return applyaround(&board::open, x, y);
    }
    return 0;
}

int board::applyaround(boardfunc f, int x, int y){
    int sum = 0;
    sum += (this->*f)(x-1, y-1);
    sum += (this->*f)(x-1, y);
    sum += (this->*f)(x-1, y+1);
    sum += (this->*f)(x, y+1);
    sum += (this->*f)(x+1, y+1);
    sum += (this->*f)(x+1, y);
    sum += (this->*f)(x+1, y-1);
    sum += (this->*f)(x, y-1);
    return sum;
}

int board::minesaround(int x, int y){
    return applyaround(&board::ismineat, x, y);
}

int board::flagsaround(int x, int y){
    return applyaround(&board::isflagat, x, y);
}

bool board::inbounds(int x, int y){
    return x >= 0 && y >= 0 && y < sizey && x < sizex;
}

int board::ismineat(int x, int y){
    return inbounds(x, y) && mine[y][x] ? 1 : 0;
}

int board::isflagat(int x, int y){
    return inbounds(x, y) && flagged[y][x] ? 1 : 0;
}

char board::imgat(int x, int y){
    if (!inbounds(x,y)) {
        return 0;
    }
    if (!opened[y][x]) {
        if (flagged[y][x]) {
            return FLAG;
        }
        return HIDDEN;
    } else if (mine[y][x]) {
        return MINE;
    } else {
        return minesaround(x, y);
    }
}

void board::initmines(int mines){
    srand(time(NULL));
    int placed = 0;
    int x;
    int y;
    while (placed < mines){
        x = rand() % sizex;
        y = rand() % sizey;
        if (!mine[y][x] && !opened[y][x]) {
            mine[y][x] = true;
            placed++;
        }
    }
}

void board::revealall(){
    for (int y = 0; y < sizey; y++){
        for (int x = 0; x < sizex; x++){
            opened[y][x] = true;
        }
    }
}





