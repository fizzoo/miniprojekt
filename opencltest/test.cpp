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

namespace {
    const size_t RESULTLENGTH = 20;
}

/**
 * Exit if it is an error. Note down name of erring function.
 */
inline void checkErr(cl_int err, const char * name) {
    if (err != CL_SUCCESS) {
        std::cerr << "ERROR: " << name  << " (" << err << ")" << std::endl;
        exit(EXIT_FAILURE);
    }
}

/**
 * If build fails, print the compilation output and exit.
 */
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

    //Since we can't specify which platform we actually want (in this program,
    //no interaction), this won't help with the helloworld. However, print the
    //information.
    {
        //Get platforms available
        std::vector<cl::Platform> platformList;
        err = cl::Platform::get(&platformList);
        checkErr(err, "cl::Platform::get");
        checkErr(platformList.size()!=0 ? CL_SUCCESS : -1, "found no platforms");

        //Print info about all available platforms
        for (cl::Platform &platform : platformList) {
            std::string res;
            platform.getInfo((cl_platform_info)CL_PLATFORM_VENDOR,      &res);
            std::cerr << "Platform: " << res << "\n";
            platform.getInfo((cl_platform_info)CL_PLATFORM_PROFILE,     &res);
            std::cerr << "Profile: " << res << "\n";
            platform.getInfo((cl_platform_info)CL_PLATFORM_VERSION,     &res);
            std::cerr << "Version: " << res << "\n";
            platform.getInfo((cl_platform_info)CL_PLATFORM_NAME,        &res);
            std::cerr << "Name: " << res << "\n";
            platform.getInfo((cl_platform_info)CL_PLATFORM_EXTENSIONS,  &res);
            std::cerr << "Extensions: " << res << "\n";
            std::cerr << std::endl;
        }
    }

    //Create the context. Grabs the first and best available GPU, from
    //whatever platform, and creates a context from that.
    cl::Context context(CL_DEVICE_TYPE_GPU, NULL, NULL, NULL, &err);
    checkErr(err, "Context::Context()");

    //Create a buffer for storing the result
    char * outH = new char[RESULTLENGTH];
    cl::Buffer outCL(context, CL_MEM_WRITE_ONLY | CL_MEM_USE_HOST_PTR,
            RESULTLENGTH, outH, &err);
    checkErr(err, "Buffer::Buffer()");

    //Get device
    std::vector<cl::Device> devices;
    devices = context.getInfo<CL_CONTEXT_DEVICES>();
    checkErr(devices.size() > 0 ? CL_SUCCESS : -1, "devices.size() > 0");


    //Print info about all available devices
    for (cl::Device &dev : devices) {
        std::string s;
        cl_uint i;
        cl_ulong l;
        dev.getInfo((cl_platform_info)CL_DEVICE_NAME, &s);
        std::cerr << "Name: " << s << "\n";
        dev.getInfo((cl_platform_info)CL_DEVICE_MAX_CLOCK_FREQUENCY, &i);
        std::cerr << "Clock freq: " << i << "\n";
        dev.getInfo((cl_platform_info)CL_DEVICE_GLOBAL_MEM_SIZE, &l);
        std::cerr << "global mem: " << l << "\n";
        dev.getInfo((cl_platform_info)CL_DEVICE_GLOBAL_MEM_CACHE_SIZE, &l);
        std::cerr << "global mem cache: " << l << "\n";
        dev.getInfo((cl_platform_info)CL_DEVICE_LOCAL_MEM_SIZE, &l);
        std::cerr << "local mem: " << l << "\n";
        std::cerr << std::endl;
    }

    //Load kernel source
    std::ifstream file("kernel.cl");
    checkErr(file.is_open() ? CL_SUCCESS:-1, "kernel.cl");
    std::string prog {
        std::istreambuf_iterator<char>(file),
        std::istreambuf_iterator<char>()};

    //Create program
    cl::Program::Sources source(1, std::make_pair(prog.c_str(), prog.length()+1));
    cl::Program program(context, source);
    err = program.build(devices,"");
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
    err = queue.enqueueNDRangeKernel(kernel, cl::NullRange, cl::NDRange(RESULTLENGTH), cl::NDRange(1, 1), NULL, &event);
    checkErr(err, "ComamndQueue::enqueueNDRangeKernel()");

    //Queue reading result
    event.wait();
    err = queue.enqueueReadBuffer(outCL, CL_TRUE, 0, RESULTLENGTH, outH);
    checkErr(err, "ComamndQueue::enqueueReadBuffer()");
    std::cout << outH;
    return EXIT_SUCCESS;

    //Cleanup maybe
    delete outH;
}
