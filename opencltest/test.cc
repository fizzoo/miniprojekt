/**
 * Just prints information that openCL can see.
 */

#include <utility>
#include <CL/cl.hpp>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <iterator>

/**
 * Checks that err is 0, otherwise exits with debug information. Since this is
 * the lazy solution where we don't check which error we got, CL_SUCCESS
 * wouldn't give any extra information. Should be 0 anyway, and this is usable
 * without a definition of that aswell. If you ever see this output, you should
 * probably put in actual checks in the relevant location.
 */
#define CHECK(err)                                                             \
  if (err) {                                                                   \
    std::cerr << "UNEXPECTED ERROR " << err << " at " << __FILE__ << ":"       \
              << __LINE__ << " in function " << __func__ << std::endl;         \
    exit(-1);                                                                  \
  }

int main() {
  cl_int err;

  {
    // Get platforms available
    std::vector<cl::Platform> platformList;
    err = cl::Platform::get(&platformList);
    CHECK(err);
    if (platformList.size() == 0) {
      std::cerr << "Found no platforms" << std::endl;
      exit(-1);
    }

    // Print info about all available platforms
    for (cl::Platform &p : platformList) {
      std::cerr << "Platform: " << p.getInfo<CL_PLATFORM_VENDOR>() << "\n";
      std::cerr << "Profile: " << p.getInfo<CL_PLATFORM_PROFILE>() << "\n";
      std::cerr << "Version: " << p.getInfo<CL_PLATFORM_VERSION>() << "\n";
      std::cerr << "Name: " << p.getInfo<CL_PLATFORM_NAME>() << "\n";
      std::cerr << "Extensions: " << p.getInfo<CL_PLATFORM_EXTENSIONS>()
                << "\n";
      std::cerr << std::endl;
    }
  }

  // Create the context. Iterates through the platforms and picks the first
  // one with a GPU, then creates a context from that.
  cl::Context context(CL_DEVICE_TYPE_GPU, NULL, NULL, NULL, &err);
  if (err) {
    // Fall back to CPU
    std::cerr << "Falling back to CPU." << std::endl;
    context = cl::Context(CL_DEVICE_TYPE_CPU, NULL, NULL, NULL, &err);
  }
  if (err) {
    std::cerr << "No platforms available. Error: '" << err << "'" << std::endl;
  }

  // Get device
  std::vector<cl::Device> devices;
  devices = context.getInfo<CL_CONTEXT_DEVICES>();
  if (devices.size() == 0) {
    std::cerr << "Found no devices" << std::endl;
  }

  // Print info about all available devices
  for (cl::Device &dev : devices) {
    std::cerr << "Name: " << dev.getInfo<CL_DEVICE_NAME>() << "\n";
    std::cerr << "Clock freq: " << dev.getInfo<CL_DEVICE_MAX_CLOCK_FREQUENCY>()
              << "\n";
    std::cerr << "global mem: " << dev.getInfo<CL_DEVICE_GLOBAL_MEM_SIZE>()
              << "\n";
    std::cerr << "global mem cache: "
              << dev.getInfo<CL_DEVICE_GLOBAL_MEM_CACHE_SIZE>() << "\n";
    std::cerr << "local mem: " << dev.getInfo<CL_DEVICE_LOCAL_MEM_SIZE>()
              << "\n";
    std::cerr << "max workgroup size: "
              << dev.getInfo<CL_DEVICE_MAX_WORK_GROUP_SIZE>() << "\n";
    std::cerr << std::endl;
  }
}
