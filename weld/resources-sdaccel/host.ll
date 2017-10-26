#include "CL/cl.h"
#include "xcl.h"


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

void check(cl_int err) {
    if (err) {
        printf("ERROR: Operation Failed: %d\n", err);
        exit(EXIT_FAILURE);
    }
}

int routine($INPUTS) {

    xcl_world world = xcl_world_single();

    cl_mem during_allocation = clCreateBuffer(world.context, CL_MEM_COPY_HOST_PTR, size_in_bytes,
                     host_memory.data(), &err);
}
