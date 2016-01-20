#include <iostream>
#include <iomanip>
#include <fstream>
#include <chrono>
#include <thread>
#include <atomic>

std::atomic<bool> active(true);

void acceptInput(){
    char c;
    std::cin.get(c);
    active = false;
}

int main(int argc, const char* argv[]){
    std::thread input_thread(acceptInput);

    auto time_begin = std::chrono::high_resolution_clock::now();
    while (active) {
        auto time_difference = std::chrono::high_resolution_clock::now() - time_begin;
        long long int seconds = std::chrono::duration_cast<std::chrono::seconds>(time_difference).count();
        std::cout << std::setfill('0')
            << seconds/(60*60) << ':'
            << std::setw(2) << (seconds/60)%60 << ':'
            << std::setw(2) << seconds%60 << std::endl;

        std::this_thread::sleep_for(std::chrono::seconds(1));
    }

    input_thread.join();
    return 0;
}
