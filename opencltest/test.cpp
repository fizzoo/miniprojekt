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
#include <sstream>
#include <iterator>


const std::string hw("Hello World\n");

/**
 * Exit if it is an error. Note down name of erring function.
 */
inline void checkErr(cl_int err, const char * name) {
    if (err != CL_SUCCESS) {
        std::cerr << "ERROR: " << name  << " (" << err << ")" << std::endl;
        exit(EXIT_FAILURE);
    }
}

void checkBuildErr(cl_int err, cl::Device *d, cl::Program *p){
    if (err != CL_SUCCESS) {
        std::cerr << "ERROR: Building OpenCL program failed!\n";
        std::string log;
        p->getBuildInfo(*d, (cl_program_build_info)CL_PROGRAM_BUILD_LOG, &log);
        std::cerr << log << std::endl;
        exit(EXIT_FAILURE);
    }
}

int main(){
    cl_int err;

    //Get platforms available
    std::vector<cl::Platform> platformList;
    err = cl::Platform::get(&platformList);
    checkErr(err, "cl::Platform::get");
    checkErr(platformList.size()!=0 ? CL_SUCCESS : -1, "found no platforms");
    std::cerr << "Number of platforms are: " << platformList.size() << "\n\n";

    //Print info about all available platforms
    for (cl::Platform &platform : platformList) {
        std::string a,b,c,d,e;
        platform.getInfo((cl_platform_info)CL_PLATFORM_VENDOR,      &a);
        platform.getInfo((cl_platform_info)CL_PLATFORM_PROFILE,     &b);
        platform.getInfo((cl_platform_info)CL_PLATFORM_VERSION,     &c);
        platform.getInfo((cl_platform_info)CL_PLATFORM_NAME,        &d);
        platform.getInfo((cl_platform_info)CL_PLATFORM_EXTENSIONS,  &e);
        std::cerr << "Platform: " << a << "\n";
        std::cerr << "Profile: " << b << "\n";
        std::cerr << "Version: " << c << "\n";
        std::cerr << "Name: " << d << "\n";
        std::cerr << "Extensions: " << e << "\n\n";
    }
    cl::Platform& nvidiacl = platformList[0]; //TODO: Somehow select correctly if multiple available.

    //Create the context
    cl_context_properties cprops[3] = {CL_CONTEXT_PLATFORM, (cl_context_properties)(nvidiacl)(), 0};
    cl::Context context(CL_DEVICE_TYPE_GPU, cprops, NULL, NULL, &err);
    checkErr(err, "Context::Context()");

    //Create a buffer for storing the result
    char * outH = new char[hw.length()+1];
    cl::Buffer outCL(context, CL_MEM_WRITE_ONLY | CL_MEM_USE_HOST_PTR, hw.length()+1, outH, &err);
    checkErr(err, "Buffer::Buffer()");

    //Get device
    std::vector<cl::Device> devices;
    devices = context.getInfo<CL_CONTEXT_DEVICES>();
    checkErr(devices.size() > 0 ? CL_SUCCESS : -1, "devices.size() > 0");

    //Load kernel source
    std::ifstream file("kernel.cl");
    checkErr(file.is_open() ? CL_SUCCESS:-1, "kernel.cl");
    std::ostringstream prog;
    prog << file.rdbuf();

    //Create program
    cl::Program::Sources source(1, std::make_pair(prog.str().c_str(), prog.str().length()+1));
    cl::Program program(context, source);
    err = program.build(devices,""); //build on all devices?
    checkBuildErr(err, &devices[0], &program);

    //Create kernel
    cl::Kernel kernel(program, "hello", &err);
    checkErr(err, "Kernel::Kernel()");
    err = kernel.setArg(0, outCL);
    checkErr(err, "Kernel::setArg()");

    //Queue kernel
    cl::CommandQueue queue(context, devices[0], 0, &err);
    checkErr(err, "CommandQueue::CommandQueue()");
    cl::Event event;
    err = queue.enqueueNDRangeKernel(kernel, cl::NullRange, cl::NDRange(hw.length()+1), cl::NDRange(1, 1), NULL, &event);
    checkErr(err, "ComamndQueue::enqueueNDRangeKernel()");

    //Queue reading result
    event.wait();
    err = queue.enqueueReadBuffer(outCL, CL_TRUE, 0, hw.length()+1, outH);
    checkErr(err, "ComamndQueue::enqueueReadBuffer()");
    std::cout << outH;
    return EXIT_SUCCESS;

    //Cleanup maybe
    delete outH;
}
