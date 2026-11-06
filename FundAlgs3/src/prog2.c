#include <stdio.h>
#include <stdlib.h>

typedef enum {
    SUCCESS = 0,
    ERR_INVALID_ARGUMENTS,
    ERR_MEMORY_ALLOCATION,
    ERR_INVALID_FORMAT
} status_t;

typedef struct {
    unsigned int id;
    int start;
    int finish;
} Report;

static const char *status_msg(status_t s) {
    switch (s) {
    case SUCCESS: return "success";
    case ERR_INVALID_ARGUMENTS: return "invalid arguments";
    case ERR_MEMORY_ALLOCATION: return "memory allocation failed";
    case ERR_INVALID_FORMAT: return "invalid input format";
    default: return "unknown error";
    }
}

static int compare_reports(const void *a, const void *b) {
    const Report *r1 = (const Report *)a;
    const Report *r2 = (const Report *)b;
    
    if (r1->start != r2->start) {
        return (r1->start < r2->start) ? -1 : 1;
    }
    if (r1->finish != r2->finish) {
        return (r1->finish < r2->finish) ? -1 : 1;
    }
    return 0;
}

static int compare_ids(const void *a, const void *b) {
    unsigned int id1 = *(const unsigned int *)a;
    unsigned int id2 = *(const unsigned int *)b;
    if (id1 < id2) return -1;
    if (id1 > id2) return 1;
    return 0;
}

status_t find_max_reports(const Report *reports, int n, int k, int *out_count) {
    if (reports == NULL || out_count == NULL) return ERR_INVALID_ARGUMENTS;
    if (n <= 0 || k <= 0) return ERR_INVALID_ARGUMENTS;

    Report *sorted_reports = (Report *)malloc(n * sizeof(Report));
    if (sorted_reports == NULL) return ERR_MEMORY_ALLOCATION;

    for (int i = 0; i < n; i++) {
        sorted_reports[i] = reports[i];
    }

    qsort(sorted_reports, n, sizeof(Report), compare_reports);

    int *rooms = (int *)malloc(k * sizeof(int));
    if (rooms == NULL) {
        free(sorted_reports);
        return ERR_MEMORY_ALLOCATION;
    }
    
    for (int i = 0; i < k; i++) {
        rooms[i] = -1;
    }

    int count = 0;

    for (int i = 0; i < n; i++) {
        int free_room_idx = -1;

        for (int j = 0; j < k; j++) {
            if (rooms[j] <= sorted_reports[i].start) {
                free_room_idx = j;
                break;
            }
        }

        if (free_room_idx != -1) {
            rooms[free_room_idx] = sorted_reports[i].finish;
            count++;
        } else {
            int latest_room_idx = 0;
            for (int j = 1; j < k; j++) {
                if (rooms[j] > rooms[latest_room_idx]) {
                    latest_room_idx = j;
                }
            }

            if (sorted_reports[i].finish < rooms[latest_room_idx]) {
                rooms[latest_room_idx] = sorted_reports[i].finish;
            }
        }
    }

    free(rooms);
    free(sorted_reports);
    
    *out_count = count;
    return SUCCESS;
}

int main(void) {
    int n, k;
    
    if (scanf("%d %d", &n, &k) != 2 || n <= 0 || k <= 0) {
        fprintf(stderr, "Error: invalid n or k\n");
        return 1;
    }

    Report *reports = (Report *)malloc(n * sizeof(Report));
    unsigned int *ids = (unsigned int *)malloc(n * sizeof(unsigned int));
    if (reports == NULL || ids == NULL) {
        free(reports); free(ids);
        fprintf(stderr, "Error: memory allocation failed\n");
        return 1;
    }

    status_t st = SUCCESS;

    for (int i = 0; i < n; i++) {
        int h1, m1, h2, m2;
        unsigned int id;
        
        if (scanf("%u %d:%d %d:%d", &id, &h1, &m1, &h2, &m2) != 5) {
            st = ERR_INVALID_FORMAT;
            break;
        }
        
        if (id == 0) {
            st = ERR_INVALID_FORMAT;
            break;
        }

        if (h1 < 0 || h1 > 23 || m1 < 0 || m1 > 59 || h2 < 0 || h2 > 23 || m2 < 0 || m2 > 59) {
            st = ERR_INVALID_FORMAT;
            break;
        }

        int start = h1 * 60 + m1;
        int finish = h2 * 60 + m2;

        if (start >= finish) {
            st = ERR_INVALID_FORMAT;
            break;
        }

        reports[i].id = id;
        reports[i].start = start;
        reports[i].finish = finish;
        ids[i] = id;
    }

    if (st != SUCCESS) {
        fprintf(stderr, "Error: %s\n", status_msg(st));
        free(reports); free(ids);
        return 1;
    }

    qsort(ids, n, sizeof(unsigned int), compare_ids);
    for (int i = 1; i < n; i++) {
        if (ids[i] == ids[i - 1]) {
            fprintf(stderr, "Error: duplicate ID found\n");
            free(reports); free(ids);
            return 1;
        }
    }
    free(ids);

    int max_count = 0;
    st = find_max_reports(reports, n, k, &max_count);

    if (st == SUCCESS) {
        printf("%d\n", max_count);
    } else {
        fprintf(stderr, "Error: %s\n", status_msg(st));
    }

    free(reports);
    return (st == SUCCESS) ? 0 : 1;
}