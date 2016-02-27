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
    std::vector< cl::Platform > platformList;
    cl::Platform::get(&platformList);
    checkErr(platformList.size()!=0 ? CL_SUCCESS : -1, "cl::Platform::get");
    std::cerr << "Platform number is: " << platformList.size() << std::endl;std::string platformVendor;
    platformList[0].getInfo((cl_platform_info)CL_PLATFORM_VENDOR, &platformVendor);
    std::cerr << "Platform is by: " << platformVendor << "\n";

    cl_context_properties cprops[3] = {CL_CONTEXT_PLATFORM, (cl_context_properties)(platformList[0])(), 0};
    cl::Context context(CL_DEVICE_TYPE_GPU, cprops, NULL, NULL, &err);
    checkErr(err, "Context::Context()"); 
}
