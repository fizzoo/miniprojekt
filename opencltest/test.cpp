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

int main(){
    cl_int err;

    //Get platforms available
    std::vector< cl::Platform > platformList;
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
    cl::Platform& gpu = platformList[0]; //TODO: Somehow select the gpu if multiple available.

    cl_context_properties cprops[3] = {CL_CONTEXT_PLATFORM, (cl_context_properties)(gpu)(), 0};
    cl::Context context(CL_DEVICE_TYPE_GPU, cprops, NULL, NULL, &err);
    checkErr(err, "Context::Context()");
}
