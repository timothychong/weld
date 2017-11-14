#include <stdint.h>
#include <string.h>
#include <inttypes.h>
#include <iostream>

// From prelude.ll:82, standard struct to represent a vector for Weld
struct weld_vector_t {
	int32_t *data;
	int64_t length;
};

// Arguments for this particular function
struct args_t {
	struct weld_vector_t vector;
	int32_t a;
};

// Standard wrapper for input arguments, data is a pointer to an args_t
struct input_arg_t {
	int64_t data;
	int32_t n_workers;
	int64_t mem_limit;
};

// Output result type for this particular function
struct results_t {
	struct weld_vector_t vector;
};

// Standard wrapper for output arguments, data is a pointer to a results_t
struct output_result_t {
	int64_t data;
	int64_t run_id;
	int64_t error_no;
};

extern "C" int64_t test_run_method_name(int64_t in)
{
	// Parse input arguments
	struct args_t *args = (struct args_t *)(((input_arg_t *)in)->data);
	int64_t vec_len = args->vector.length;
	int32_t *in_vec = args->vector.data;
	int32_t offset = args->a;

	// Compute output vector
	int32_t *out_vec = (int32_t *)malloc(sizeof(int32_t)*vec_len);
	struct weld_vector_t v;
	v.data = out_vec;
	v.length = vec_len;
	for (int64_t i = 0; i < vec_len; i++) {
		out_vec[i] = in_vec[i] + offset;
	}

	// Stuff output results
	struct results_t *result = (struct results_t *)malloc(sizeof(struct results_t));
	result->vector = v;

	// Set up standard output structure
	struct output_result_t *out = (struct output_result_t *)malloc(sizeof(struct output_result_t));
	out->data = (int64_t)result;
	out->run_id = 0;
	out->error_no = 0;

	return (int64_t)out;
}