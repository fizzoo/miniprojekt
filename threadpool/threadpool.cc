#include "threadpool.h"

/**
 * For each thread; fetches and runs a task, or waits.
 */
void ThreadPool::thread_entry() {
  std::function<void()> job;
  while (true) {
    {
      std::unique_lock<std::mutex> l(lock);

      while (tasks.empty()) {
        if (shutdown) {
          return;
        } else {
          --active_threads;
          longwait.wait(l);
          ++active_threads;
        }
      }

      job = std::move(tasks.front());
      tasks.pop();
    }

    job();
  }
}

/**
 * CTOR creates threads, defaults to 0 -> detection of cpus.
 */
ThreadPool::ThreadPool(unsigned number_of_threads)
    : shutdown(false) {
  if (number_of_threads == 0) {
    number_of_threads = std::thread::hardware_concurrency();

    // Failure of hardware_concurrency()
    if (number_of_threads == 0) {
      number_of_threads = 4;
    }
  }
  active_threads = number_of_threads;

  for (unsigned i = 0; i < number_of_threads; ++i) {
    threads.emplace_back(std::bind(&ThreadPool::thread_entry, this));
  }
}

/**
 * DTOR waits for completion of all tasks.
 */
ThreadPool::~ThreadPool() {
  {
    std::unique_lock<std::mutex> l(lock);
    shutdown = true;
  }
  longwait.notify_all();

  // threads will not join while !tasks.empty().
  for (auto &thread : threads) {
    thread.join();
  }
}

/**
 * Enqueues a task.
 */
void ThreadPool::operator()(std::function<void()> func) {
  {
    std::unique_lock<std::mutex> l(lock);
    tasks.push(std::move(func));
  }
  longwait.notify_one();
}

void ThreadPool::wait_until_done() {
  while (!done()) {
    std::this_thread::yield();
  }
}

bool ThreadPool::done() {
  std::unique_lock<std::mutex> l(lock);
  return (tasks.empty() && !active_threads);
}
