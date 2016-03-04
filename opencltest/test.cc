/**
 * Following the amd Intro OpenCL Tutorial.
 * Some things are deprecated and needed change.
 * Comments by me.
 */

#include <utility>
#include <CL/cl.hpp>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <iterator>
#include "imloader.h"

namespace {
const size_t RESULTLENGTH = 21;
}

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

/**
 * If build fails, print the compilation output and exit.
 */
void checkBuildErr(cl_int err, cl::Device *d, cl::Program *p) {
  if (err != CL_SUCCESS) {
    std::cerr << "ERROR: Building OpenCL program failed!\n";
    std::string log;
    p->getBuildInfo(*d, (cl_program_build_info)CL_PROGRAM_BUILD_LOG, &log);
    std::cerr << log << std::endl;
    exit(-2);
  }
}

int main() {
  cl_int err;

  // Since we can't specify which platform we actually want (in this program,
  // no interaction), this won't help with the helloworld. However, print the
  // information.
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
    //Fall back to CPU
    context = cl::Context(CL_DEVICE_TYPE_CPU, NULL, NULL, NULL, &err);
  }
  CHECK(err);

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
    std::cerr << std::endl;
  }

  // Create a buffer for storing the result
  char *outH = new char[RESULTLENGTH]();
  cl::Buffer outCL(context, CL_MEM_WRITE_ONLY | CL_MEM_USE_HOST_PTR,
                   RESULTLENGTH, outH, &err);
  CHECK(err);

  iml::Image img("foo.png");
  cl::Image2D(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
              {CL_RGB, CL_UNSIGNED_INT8}, img.width(), img.height(), 0,
              img.data());

  // Load kernel source
  std::ifstream file("kernel.cl");
  if (!file) {
    std::cerr << "Kernel source file not opened correctly" << std::endl;
    exit(-1);
  }
  std::string prog{std::istreambuf_iterator<char>(file),
                   std::istreambuf_iterator<char>()};

  // Create program
  cl::Program::Sources source(1,
                              std::make_pair(prog.c_str(), prog.length() + 1));
  cl::Program program(context, source);
  err = program.build(devices, "");
  checkBuildErr(err, &devices[0], &program);

  // Create kernel
  cl::Kernel kernel(program, "hello", &err);
  CHECK(err);
  err = kernel.setArg(0, outCL);
  CHECK(err);

  // Queue kernel
  cl::CommandQueue queue(context, devices[0], 0, &err);
  CHECK(err);
  cl::Event event;
  err = queue.enqueueNDRangeKernel(kernel, cl::NullRange,
                                   cl::NDRange(RESULTLENGTH), cl::NDRange(1, 1),
                                   NULL, &event);
  CHECK(err);

  // Queue reading result
  event.wait();
  err = queue.enqueueReadBuffer(outCL, CL_TRUE, 0, RESULTLENGTH, outH);
  CHECK(err);
  std::cout << outH;
  return EXIT_SUCCESS;

  // Cleanup maybe
  delete outH;
}