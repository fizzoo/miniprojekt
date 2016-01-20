#include "fastio.h"

int main(int argc, const char* argv[]) {
    int x;
    scanint(x);
    putint(x);

    ps("HEEJ");

    for (int a = gc(); a != EOF; a = gc()) {
        pc(a);
    }
}
