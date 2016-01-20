#include <cstdio>

#ifndef SNABB_CPP
#define SNABB_CPP

const auto& pc = putchar_unlocked;
const auto& gc = getchar_unlocked;

void scanint(int &x){
    register int c = gc();
    x = 0;
    for(; (c<48 || c>57); c = gc());
    for(; c>47 && c<58; c = gc()) {
        x = x*10 + c - 48;
    }
}
void putint(int n){
    register int N = n, rev = n, count = 0;
    if (N == 0) { pc('0'); pc('\n'); return ;}
    while ((rev % 10) == 0) {
        count++;
        rev /= 10;
    }
    rev = 0;
    while (N != 0) {
        rev = rev*10 + N % 10;
        N /= 10;
    }
    while (rev != 0) {
        pc(rev % 10 + '0');
        rev /= 10;
    }
    while (count--) pc('0');
}

void ps(const char *s){
    while(*s != '\0'){
        pc(*s);
        ++s;
    }
}

#endif /* end of include guard: SNABB_CPP */
