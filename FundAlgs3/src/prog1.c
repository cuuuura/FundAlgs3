#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

typedef enum {
    SUCCESS = 0,
    ERR_INVALID_ARGUMENTS,
    ERR_INVALID_R,
    ERR_MEMORY_ALLOCATION
} status_t;

static const char *status_msg(status_t s) {
    switch (s) {
    case SUCCESS: return "success";
    case ERR_INVALID_ARGUMENTS: return "invalid arguments";
    case ERR_INVALID_R: return "invalid r (must be 1..5)";
    case ERR_MEMORY_ALLOCATION: return "memory allocation failed";
    default: return "unknown error";
    }
}


static unsigned int bitwise_add_one(unsigned int x) {
    unsigned int carry = 1;
    while (carry != 0) {
        unsigned int next_carry = x & carry;
        x = x ^ carry;
        carry = next_carry << 1;
    }
    return x;
}

static uintptr_t bitwise_add_one_ptr(uintptr_t x) {
    uintptr_t carry = 1;
    while (carry != 0) {
        uintptr_t next_carry = x & carry;
        x = x ^ carry;
        carry = next_carry << 1;
    }
    return x;
}

static uintptr_t bitwise_sub_one_ptr(uintptr_t x) {
    uintptr_t carry = 1;
    while (carry != 0) {
        uintptr_t next_carry = (~x) & carry;
        x = x ^ carry;
        carry = next_carry << 1;
    }
    return x;
}

status_t convert_to_base_2r(int num, int r, const char *digits, char **out_str) {
    if (out_str == NULL || digits == NULL) return ERR_INVALID_ARGUMENTS;
    if (r < 1 || r > 5) return ERR_INVALID_R;

    char *buf = (char *)malloc(35);
    if (buf == NULL) return ERR_MEMORY_ALLOCATION;

    if (num == 0) {
        buf[0] = '0';
        buf[1] = '\0';
        *out_str = buf;
        return SUCCESS;
    }

    bool is_negative = (num < 0);
    unsigned int u_num = (unsigned int)num;
    
    if (is_negative) {
        u_num = ~u_num;
        u_num = bitwise_add_one(u_num);
    }

    unsigned int mask = ~(~0U << r); 
    
    char *ptr = buf;

    while (u_num != 0) {
        unsigned int digit = u_num & mask;
        *ptr = digits[digit];
        ptr = (char *)bitwise_add_one_ptr((uintptr_t)ptr);
        u_num = u_num >> r;
    }

    if (is_negative) {
        *ptr = '-';
        ptr = (char *)bitwise_add_one_ptr((uintptr_t)ptr);
    }
    *ptr = '\0';

    char *start = buf;
    char *end = ptr;
    end = (char *)bitwise_sub_one_ptr((uintptr_t)end);

    while (start < end) {
        char tmp = *start;
        *start = *end;
        *end = tmp;
        start = (char *)bitwise_add_one_ptr((uintptr_t)start);
        end = (char *)bitwise_sub_one_ptr((uintptr_t)end);
    }

    *out_str = buf;
    return SUCCESS;
}

int main(void) {
    const char digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUV";

    int nums[] = {0, 10, -10, 255, -255};
    int r_values[] = {1, 3, 4, 5};
    
    for (size_t i = 0; i < sizeof(nums) / sizeof(nums[0]); i++) {
        for (size_t j = 0; j < sizeof(r_values) / sizeof(r_values[0]); j++) {
            int num = nums[i];
            int r = r_values[j];
            char *res = NULL;
            
            status_t st = convert_to_base_2r(num, r, digits, &res);
            
            if (st == SUCCESS) {
                printf("Number %d in base 2^%d: %s\n", num, r, res);
                free(res);
            } else {
                printf("Error: %s\n", status_msg(st));
            }
        }
    }
    return 0;
}