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
    // Fall back to CPU
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

  iml::Image img("foo.png");
  if (!img) {
    std::cerr << "Image loaded incorrectly" << std::endl;
    exit(-1);
  }
  cl::Image2D img_in(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                     {CL_RGBA, CL_UNSIGNED_INT8}, img.width(), img.height(), 0,
                     img.data, &err);
  if (err) {
    switch (err) {
    case CL_INVALID_CONTEXT:
      std::cerr << "CL_INVALID_CONTEXT" << std::endl;
      break;
    case CL_INVALID_VALUE:
      std::cerr << "CL_INVALID_VALUE" << std::endl;
      break;
    case CL_INVALID_IMAGE_SIZE:
      std::cerr << "CL_INVALID_IMAGE_SIZE" << std::endl;
      break;
    case CL_INVALID_HOST_PTR:
      std::cerr << "CL_INVALID_HOST_PTR" << std::endl;
      break;
    case CL_IMAGE_FORMAT_NOT_SUPPORTED:
      std::cerr << "CL_IMAGE_FORMAT_NOT_SUPPORTED" << std::endl;
      break;
    case CL_MEM_OBJECT_ALLOCATION_FAILURE:
      std::cerr << "CL_MEM_OBJECT_ALLOCATION_FAILURE" << std::endl;
      break;
    case CL_INVALID_OPERATION:
      std::cerr << "CL_INVALID_OPERATION: no device supporting image"
                << std::endl;
      break;
    }
    CHECK(err);
  }
  cl::Image2D img_out(context, CL_MEM_WRITE_ONLY, {CL_RGBA, CL_UNSIGNED_INT8},
                      img.width(), img.height(), 0, 0, &err);
  CHECK(err);

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
  cl::Kernel kernel(program, "invert", &err);
  CHECK(err);
  err = kernel.setArg(0, img_in);
  err = kernel.setArg(1, img_out);
  if (err == CL_INVALID_MEM_OBJECT) {
    std::cerr << "kernel argument setting failed with CL_INVALID_MEM_OBJECT"
              << std::endl;
  }
  CHECK(err);

  // Queue kernel
  cl::CommandQueue queue(context, devices[0], 0, &err);
  CHECK(err);
  cl::Event event;
  err = queue.enqueueNDRangeKernel(kernel, cl::NullRange,
                                   cl::NDRange(img.width(), img.height()),
                                   cl::NDRange(1, 1), NULL, &event);
  CHECK(err);

  // Queue reading result
  event.wait();
  cl::size_t<3> origin;
  origin[0] = 0;
  origin[1] = 0;
  origin[2] = 0;
  cl::size_t<3> region;
  region[0] = img.width();
  region[1] = img.height();
  region[2] = 1;
  err = queue.enqueueReadImage(img_out, CL_TRUE, origin, region, 0, 0,
                               (void *)img.data);
  CHECK(err);

  return EXIT_SUCCESS;
}
