#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include <string.h>

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

int main() {
    // Compile Weld module.
    weld_error_t e = weld_error_new();
    weld_conf_t conf = weld_conf_new();
    weld_module_t m = weld_module_compile(program, conf, e);
    weld_conf_free(conf);

    if (weld_error_code(e)) {
        const char *err = weld_error_message(e);
        printf("Error message: %s\n", err);
        exit(1);
    }

    weld_vector v0;
    weld_vector v1;
    weld_vector v2;
    weld_vector v3;
    weld_vector v4;
    weld_vector v5;
    const uint64_t length = 256;

    float *data0 = (float *)malloc(sizeof(float)*length);
    float *data1 = (float *)malloc(sizeof(float)*length);
    float *data2 = (float *)malloc(sizeof(float)*length);
    float *data3 = (float *)malloc(sizeof(float)*length);
    float *data4 = (float *)malloc(sizeof(float)*length);
    float *data5 = (float *)malloc(sizeof(float)*length);

    for (int32_t i = 0; i < length; i++) {
        data0[i] = 1;
        data1[i] = 2;
        data2[i] = 3;
        data3[i] = 4;
        data4[i] = 5;
        data5[i] = 6;
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

    weld_value_t arg = weld_value_new(&a);

    // Run the module and get the result.
    conf = weld_conf_new();
    weld_value_t result = weld_module_run(m, conf, arg, e);
    if (weld_error_code(e)) {
        const char *err = weld_error_message(e);
        printf("Error message: %s\n", err);
        exit(1);
    }
    struct weld_vector *result_data = (struct weld_vector *)weld_value_data(result);

    for (int32_t i = 0; i < length; i++) {
        printf("Answer element %d: %f\n", i, result_data->data[i]);  
    }


    printf("Answer Length: %d\n", (int32_t)result_data->length);
    printf("Expected Length: %llu\n", length);

    // Free the values.
    weld_value_free(result);
    weld_value_free(arg);
    weld_conf_free(conf);

    weld_error_free(e);
    weld_module_free(m);
    return 0;
}
