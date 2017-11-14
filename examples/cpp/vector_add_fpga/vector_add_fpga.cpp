#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include <string.h>
#include <inttypes.h>

// Include the Weld API.
#include "../../../c/weld.h"

struct weld_vector {
    int32_t *data;
    int64_t length;
};

struct args {
    struct weld_vector vector;
    int32_t a;
};

const char *program = "|x:vec[i32], a:i32| map(x, |y| y+a)";

int main() {
    // Compile Weld module.
    weld_error_t e = weld_error_new();
    weld_conf_t conf = weld_conf_new();

    weld_conf_set(conf, "weld.compile.multithread_support", "false");

    weld_module_t m = weld_module_compile(program, conf, e);
    weld_conf_free(conf);

    if (weld_error_code(e)) {
        const char *err = weld_error_message(e);
        printf("Error message: %s\n", err);
        exit(1);
    }

    weld_vector v;
    const uint64_t length = 256;
    int32_t *data = (int32_t *)malloc(sizeof(uint32_t)*length);
    for (int i = 0; i < length; i++) {
        data[i] = 1;
    }

    v.data = data;
    v.length = length;

    struct args a;
    a.vector = v;
    a.a = 10;

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

    // Print output
    printf("Output length: %"PRId64"\n", result_data->length);
    for (int i = 0; i < length; i++) {
        printf("Output %d: %d\n", i, result_data->data[i]);
    }

    // Free the values.
    weld_value_free(result);
    weld_value_free(arg);
    weld_conf_free(conf);

    weld_error_free(e);
    weld_module_free(m);
    printf("Freeing data and quiting!\n");
    return 0;
}
