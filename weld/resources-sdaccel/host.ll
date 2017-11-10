#include "CL/cl.h"
#include "xcl.h"
#include "oclHelper.h"

#include <algorithm>
#include <cstdio>
#include <string>
#include <vector>
using std::vector;

// Copied from SDAccel examples
//Allocator template to align buffer to Page boundary for better data transfer
template <typename T>
struct aligned_allocator
{
  using value_type = T;
  T* allocate(std::size_t num)
  {
    void* ptr = nullptr;
    if (posix_memalign(&ptr,4096,num*sizeof(T)))
      throw std::bad_alloc();
    return reinterpret_cast<T*>(ptr);
  }
  void deallocate(T* p, std::size_t num)
  {
    free(p);
  }
};

void event_cb(cl_event event, cl_int cmd_status, void *data) {
       cl_command_type command;
       clGetEventInfo(event, CL_EVENT_COMMAND_TYPE, sizeof(cl_command_type),
                              &command, nullptr);
       cl_int status;
       clGetEventInfo(event, CL_EVENT_COMMAND_EXECUTION_STATUS, sizeof(cl_int),
                              &status, nullptr);
       const char *command_str;
       const char *status_str;
       switch (command) {
           case CL_COMMAND_READ_BUFFER:
             command_str = "buffer read";
             break;
           case CL_COMMAND_WRITE_BUFFER:
             command_str = "buffer write";
             break;
           case CL_COMMAND_NDRANGE_KERNEL:
             command_str = "kernel";
             break;
           
    }
       switch (status) {
           case CL_QUEUED:
             status_str = "Queued";
             break;
           case CL_SUBMITTED:
             status_str = "Submitted";
             break;
           case CL_RUNNING:
             status_str = "Executing";
             break;
           case CL_COMPLETE:
             status_str = "Completed";
             break;
           
    }
       printf("%s %s %s\n", status_str, reinterpret_cast<char *>(data), command_str);
       fflush(stdout);
     
}

// Wrap any OpenCL API calls that return error code(cl_int) with the below macro
// to quickly check for an error
#define OCL_CHECK(call)                                                        \
  do {                                                                         \
    cl_int err = call;                                                         \
    if (err != CL_SUCCESS) {                                                   \
      printf(__FILE__ ":%d: [ERROR] " #call " returned %s\n", __LINE__,        \
             oclErrorCode(err));                                               \
      exit(EXIT_FAILURE);                                                      \
    }                                                                          \
  } while (0);

void set_callback(cl_event event, const char *queue_name) {
  OCL_CHECK(
      clSetEventCallback(event, CL_COMPLETE, event_cb, (void *)queue_name));
}
int routine(float* e0, unsigned int e0_size, float* e1, unsigned int e1_size, float* e2, unsigned int e2_size, float* e3, unsigned int e3_size, float* e4, unsigned int e4_size, float* e5, unsigned int e5_size, float* result0, unsigned int result0_size);

int main () {

  float e0[] = { 1, 2, 3, 4, 5 };
  float e1[] = { 1, 2, 3, 4, 5 };
  float e2[] = { 1, 2, 3, 4, 5 };
  float e3[] = { 1, 2, 3, 4, 5 };
  float e4[] = { 1, 2, 3, 4, 5 };
  float e5[] = { 1, 2, 3, 4, 5 };

  int e0_size = 5;
  int e1_size = 5;
  int e2_size = 5;
  int e3_size = 5;
  int e4_size = 5;
  int e5_size = 5;

  float result0[5];

  routine (
  e0,
  e0_size,
  e1,
  e1_size,
  e2,
  e2_size,
  e3,
  e3_size,
  e4,
  e4_size,
  e5,
  e5_size,
  result0,
  5
  );
}

int routine($INPUTS) {

