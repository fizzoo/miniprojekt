#include <vector>
#include <cstdio>

template <typename T>
class Pyramid {
    public:
        Pyramid(std::initializer_list<T> inp) : v(inp){}
        T* operator[](size_t i){
            return v.data() + sumto(i);
        }
        std::vector<T> v;
        size_t sumto(size_t i){
            size_t res = 0;
            for (; i; --i) {
                res += i;
            }
            return res;
        }
};
