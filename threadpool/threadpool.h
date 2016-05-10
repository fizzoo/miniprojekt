#ifndef THREADPOOL_H
#define THREADPOOL_H

#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>
#include <functional>
#include <atomic>

class ThreadPool {
private:
  std::mutex lock;
  std::condition_variable longwait;
  bool shutdown;
  std::queue<std::function<void()>> tasks;
  std::vector<std::thread> threads;
  void thread_entry();
  unsigned active_threads;

  ThreadPool(ThreadPool const &rhs) = delete;
  ThreadPool(ThreadPool &&rhs) = delete;
  ThreadPool &operator=(ThreadPool const &rhs) = delete;
  ThreadPool &operator=(ThreadPool &&rhs) = delete;

public:
  ThreadPool(unsigned = 0);
  ~ThreadPool();
  void wait_until_done();
  bool done();
  void operator()(std::function<void()>);
};

#endif /* end of include guard: THREADPOOL_H */
