#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/time.h>

#include <string.h>
#include <inttypes.h>

// Include the Weld API.
#include "../../../c/weld.h"

struct weld_vector {
    float *data;
    int64_t length;
};

struct args {
    struct weld_vector e0;
    struct weld_vector e1;
    struct weld_vector e2;
    struct weld_vector e3;
    struct weld_vector e4;
    struct weld_vector e5;
};

const char *program = "|e0:vec[f32], e1:vec[f32], e2:vec[f32], e3:vec[f32], e4:vec[f32], e5:vec[f32]| map(zip(map(zip(map(zip(e1, map(map(zip(e2,e5), |z: {f32,f32}| z.$0 * z.$1), |z : f32| exp(z))), |z: {f32,f32}| z.$0 * z.$1),map(map(zip(map(zip(map(zip(map(map(zip(e0,e1), |z: {f32,f32}| z.$0 / z.$1), |z : f32| log(z)),map(zip(map(zip(e3,map(map(zip(e4,e4), |z: {f32,f32}| z.$0 * z.$1), |z: f32| z * 0.5f)), |z: {f32,f32}| z.$0 + z.$1),e2), |z: {f32,f32}| z.$0 * z.$1)), |z: {f32,f32}| z.$0 + z.$1),map(zip(e4,map(e2, |z : f32| sqrt(z))), |z: {f32,f32}| z.$0 * z.$1)), |z: {f32,f32}| z.$0 / z.$1),map(zip(e4,map(e2, |z : f32| sqrt(z))), |z: {f32,f32}| z.$0 * z.$1)), |z: {f32,f32}| z.$0 - z.$1), |z: f32| z - 0.0f)), |z: {f32,f32}| z.$0 * z.$1),map(zip(e0,map(map(zip(map(zip(map(map(zip(e0,e1), |z: {f32,f32}| z.$0 / z.$1), |z : f32| log(z)),map(zip(map(zip(e3,map(map(zip(e4,e4), |z: {f32,f32}| z.$0 * z.$1), |z: f32| z * 0.5f)), |z: {f32,f32}| z.$0 + z.$1),e2), |z: {f32,f32}| z.$0 * z.$1)), |z: {f32,f32}| z.$0 + z.$1),map(zip(e4,map(e2, |z : f32| sqrt(z))), |z: {f32,f32}| z.$0 * z.$1)), |z: {f32,f32}| z.$0 / z.$1), |z: f32| z - 0.0f)), |z: {f32,f32}| z.$0 * z.$1)), |z: {f32,f32}| z.$0 - z.$1)";

struct timeval run_and_time_function(weld_module_t m, uint64_t length, weld_error_t e) {
    weld_vector v0;
    weld_vector v1;
    weld_vector v2;
    weld_vector v3;
    weld_vector v4;
    weld_vector v5;

    float *data0 = (float *)malloc(sizeof(float)*length);
    float *data1 = (float *)malloc(sizeof(float)*length);
    float *data2 = (float *)malloc(sizeof(float)*length);
    float *data3 = (float *)malloc(sizeof(float)*length);
    float *data4 = (float *)malloc(sizeof(float)*length);
    float *data5 = (float *)malloc(sizeof(float)*length);

    // TODO: Dennis, what are sensible inputs for blackscholes? Some of these 
    // give nan or inf...
    int r0 = rand()%5;
    int r1 = rand()%5;
    int r2 = rand()%5;
    int r3 = rand()%5;
    int r4 = rand()%5;
    int r5 = rand()%5;
    for (int32_t i = 0; i < length; i++) {
        data0[i] = r0;
        data1[i] = r1;
        data2[i] = r2;
        data3[i] = r3;
        data4[i] = r4;
        data5[i] = r5;
    }

    v0.data = data0;
    v0.length = length;
    v1.data = data1;
    v1.length = length;
    v2.data = data2;
    v2.length = length;
    v3.data = data3;
    v3.length = length;
    v4.data = data4;
    v4.length = length;
    v5.data = data5;
    v5.length = length;

    struct args a;
    a.e0 = v0;
    a.e1 = v1;
    a.e2 = v2;
    a.e3 = v3;
    a.e4 = v4;
    a.e5 = v5;

    struct timeval start, end, diff;
    gettimeofday(&start, 0); // Start timing
    weld_value_t arg = weld_value_new(&a);

    // Run the module and get the result.
    weld_conf_t conf = weld_conf_new();
    weld_value_t result = weld_module_run(m, conf, arg, e);
    if (weld_error_code(e)) {
        const char *err = weld_error_message(e);
        printf("Error message: %s\n", err);
        exit(1);
    }

    struct weld_vector *result_data = 
        (struct weld_vector *)weld_value_data(result);
    float final_result = result_data->data[0];

    // Free the values.
    free(data0);
    free(data1);
    free(data2);
    free(data3);
    free(data4);
    free(data5);
    weld_value_free(result);
    weld_value_free(arg);
    weld_conf_free(conf);

    gettimeofday(&end, 0); // End timing
    timersub(&end, &start, &diff);
    printf("Weld: %ld.%06lds (result=%f)\n",
            (long) diff.tv_sec, (long) diff.tv_usec, final_result);
    return diff;
}

int main() {
    // Length of vector inputs
    const uint64_t length = 1 << 22;
    // Number of timing iterations (averaged)
    const int num_iter = 25;

    printf("Testing with length %"PRId64" and %d iterations...\n", 
        length, num_iter);

    srand(1);

    // Compile Weld module.
    weld_error_t e = weld_error_new();
    weld_conf_t conf = weld_conf_new();
    weld_module_t m = weld_module_compile(program, conf, e);
    if (weld_error_code(e)) {
        const char *err = weld_error_message(e);
        printf("Error message: %s\n", err);
        exit(1);
    }

    // Set up timers
    struct timeval total, next, sum;
    timerclear(&total);

    // Iterate through timing runs
    for (int i = 0; i < num_iter; i++) {
        next = run_and_time_function(m, length, e);
        timeradd(&total, &next, &sum);
        total = sum;
    }

    // Print aggregate timing result
    printf("Weld total: %ld.%06lds \n",
        (long) total.tv_sec, (long) total.tv_usec);

    int64_t total_time = ((long)total.tv_sec)*1000000 + ((long)total.tv_usec);
    int64_t avg_time = total_time/num_iter;
    printf("Weld average: %ld.%06lds \n",
        (long) avg_time/1000000, (long) avg_time%1000000);

    // Cleanup
    weld_conf_free(conf);
    weld_error_free(e);
    weld_module_free(m);
    return 0;
}
