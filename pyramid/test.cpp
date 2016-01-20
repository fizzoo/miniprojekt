#include "pyramid.h"
#include <iostream>
#include <cassert>

int main(int argc, const char* argv[]) {
    {
        Pyramid<int> a = {0, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4};
        assert(a[0][0] == 0);
        assert(a[1][0] == 1);
        assert(a[2][0] == 2);
        assert(a[3][0] == 3);
        assert(a[4][0] == 4);
        assert(a[3][0] == 3);
        assert(a[3][1] == 3);
        assert(a[3][2] == 3);
        assert(a[3][3] == 3);
    }
}
