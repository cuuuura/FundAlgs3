#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <math.h>

// 6

typedef enum {
    SUCCESS = 0,
    ERR_NULL,
    ERR_MEMORY,
    ERR_VALUE,
    ERR_NOT_FOUND,
    ERR_EOF,
    ERR_FILE,
    ERR_ROUTE
} status_t;

enum {
    MIN_YEAR = 1900
};

typedef enum {
    MARKER_START = 0,
    MARKER_MIDDLE,
    MARKER_END
} marker_t;

typedef enum {
    METRIC_ROUTES = 0,
    METRIC_TOTAL_DISTANCE,
    METRIC_LONGEST_ROUTE,
    METRIC_SHORTEST_ROUTE,
    METRIC_LONGEST_STOP,
    METRIC_SHORTEST_STOP,
    METRIC_TOTAL_IDLE
} metric_t;

typedef struct {
    char *data;
    int length;
} String;

typedef struct {
    double x;
    double y;
} Point;

typedef struct {
    unsigned int day;
    unsigned int month;
    unsigned int year;
    unsigned int hour;
    unsigned int minute;
    unsigned int second;
    long long seconds;
} DateTime;

typedef struct {
    DateTime arrival;
    DateTime departure;
    marker_t marker;
    Point point;
} Stop;

typedef struct {
    String vehicle;
    Stop stop;
} Record;

typedef struct {
    Record *items;
    size_t count;
    size_t capacity;
} RecordArray;

typedef struct StopNode {
    Stop stop;
    struct StopNode *next;
} StopNode;

typedef struct RouteNode {
    String vehicle;
    StopNode *head;
    StopNode *tail;
    size_t stops_count;
    struct RouteNode *next;
} RouteNode;

typedef struct {
    RouteNode *head;
    RouteNode *tail;
    size_t count;
} Storage;

typedef struct {
    const String *vehicle;
    size_t routes;
    double total_distance;
    double longest_route;
    double shortest_route;
    long long longest_stop;
    long long shortest_stop;
    long long total_idle;
} VehicleStat;

typedef struct {
    metric_t metric;
    int want_max;
    const char *title;
} Query;

typedef struct {
    const char *path;
    size_t line;
    String vehicle;
} LoadInfo;

// String

static status_t string_clear(String *str) {
    if (str == NULL) return ERR_NULL;

    free(str->data);
    str->data = NULL;
    str->length = 0;

    return SUCCESS;
}

static status_t string_from_range(const char *src, size_t len, String *out) {
    if (src == NULL || out == NULL) return ERR_NULL;
    if (out->data != NULL) return ERR_VALUE;
    if (len > (size_t)INT_MAX) return ERR_VALUE;

    char *buf = malloc(len + 1);
    if (buf == NULL) return ERR_MEMORY;

    memcpy(buf, src, len);
    buf[len] = '\0';

    out->data = buf;
    out->length = (int)len;
    return SUCCESS;
}

// вспомогательные

static const char *status_text(status_t st) {
    switch (st) {
        case SUCCESS: return "успешно";
        case ERR_NULL: return "пустой указатель";
        case ERR_MEMORY: return "не хватает памяти";
        case ERR_VALUE: return "некорректное значение";
        case ERR_NOT_FOUND: return "не найдено";
        case ERR_EOF: return "конец ввода";
        case ERR_FILE: return "ошибка работы с файлом";
        case ERR_ROUTE: return "маршруты не складываются";
        default: return "неизвестная ошибка";
    }
}

static status_t parse_uint(const char *s, unsigned int *out) {
    if (s == NULL || out == NULL) return ERR_NULL;
    if (*s == '\0') return ERR_VALUE;

    unsigned int v = 0;
    for (int i = 0; s[i] != '\0'; i++) {
        if (s[i] < '0' || s[i] > '9') return ERR_VALUE;
        unsigned int digit = (unsigned int)(s[i] - '0');

        if (v > (UINT_MAX - digit) / 10u) return ERR_VALUE;
        v = v * 10u + digit;
    }
    *out = v;
    return SUCCESS;
}

static status_t parse_coordinate(const char *s, double *out) {
    if (s == NULL || out == NULL) return ERR_NULL;

    const char *p = s;
    if (*p == '-' || *p == '+') p++;

    int digits = 0, dots = 0;
    for (; *p != '\0'; p++) {
        if (*p == '.') {
            dots++;
            if (dots > 1) return ERR_VALUE;
        } else if (*p >= '0' && *p <= '9') {
            digits++;
        } else {
            return ERR_VALUE;
        }
    }
    if (digits == 0) return ERR_VALUE;

    char *end = NULL;
    double v = strtod(s, &end);
    if (end == s || *end != '\0' || !isfinite(v)) return ERR_VALUE;

    *out = v;
    return SUCCESS;
}

static status_t doubles_compare(double a, double b, double eps, int *result) {
    if (result == NULL) return ERR_NULL;
    if (!isfinite(a) || !isfinite(b)) return ERR_VALUE;
    if (!isfinite(eps) || eps <= 0.0) return ERR_VALUE;

    *result = (a - b > eps) ? 1 : (b - a > eps) ? -1 : 0;
    return SUCCESS;
}

static status_t next_token(const char **cursor, String *out) {
    if (cursor == NULL || *cursor == NULL || out == NULL) return ERR_NULL;
    if (out->data != NULL) return ERR_VALUE;

    const char *p = *cursor;
    while (*p == ' ' || *p == '\t') p++;
    if (*p == '\0') return ERR_VALUE;

    const char *start = p;
    while (*p != '\0' && *p != ' ' && *p != '\t') p++;

    status_t st = string_from_range(start, (size_t)(p - start), out);
    if (st != SUCCESS) return st;

    *cursor = p;
    return SUCCESS;
}

static int has_more_tokens(const char *cursor) {
    if (cursor == NULL) return 0;

    while (*cursor == ' ' || *cursor == '\t') cursor++;
    return *cursor != '\0';
}

// дата и время

static int is_leap(unsigned int y) {
    return (y % 4 == 0 && y % 100 != 0) || y % 400 == 0;
}

static unsigned int days_in_month(unsigned int m, unsigned int y) {
    const unsigned int d[] = {31,28,31,30,31,30,31,31,30,31,30,31};
    if (m == 2 && is_leap(y)) return 29;
    return d[m - 1];
}

static status_t read_digits(const char *s, int count, unsigned int *out) {
    unsigned int v = 0;
    for (int i = 0; i < count; i++) {
        if (s[i] < '0' || s[i] > '9') return ERR_VALUE;
        v = v * 10u + (unsigned int)(s[i] - '0');
    }
    *out = v;
    return SUCCESS;
}

static long long datetime_to_seconds(const DateTime *d) {
    long long days = 0;
    for (unsigned int y = MIN_YEAR; y < d->year; y++) days += is_leap(y) ? 366 : 365;
    for (unsigned int m = 1; m < d->month; m++) days += days_in_month(m, d->year);
    days += d->day - 1;

    return days * 86400LL + d->hour * 3600LL + d->minute * 60LL + d->second;
}

static status_t parse_datetime(const char *date, const char *clock, DateTime *out) {
    if (date == NULL || clock == NULL || out == NULL) return ERR_NULL;
    if (strlen(date) != 10 || date[2] != '.' || date[5] != '.') return ERR_VALUE;
    if (strlen(clock) != 8 || clock[2] != ':' || clock[5] != ':') return ERR_VALUE;

    DateTime d = {0};
    status_t st = read_digits(date, 2, &d.day);
    if (st == SUCCESS) st = read_digits(date + 3, 2, &d.month);
    if (st == SUCCESS) st = read_digits(date + 6, 4, &d.year);
    if (st == SUCCESS) st = read_digits(clock, 2, &d.hour);
    if (st == SUCCESS) st = read_digits(clock + 3, 2, &d.minute);
    if (st == SUCCESS) st = read_digits(clock + 6, 2, &d.second);
    if (st != SUCCESS) return st;

    if (d.year < MIN_YEAR) return ERR_VALUE;
    if (d.month < 1 || d.month > 12) return ERR_VALUE;
    if (d.day < 1 || d.day > days_in_month(d.month, d.year)) return ERR_VALUE;
    if (d.hour > 23 || d.minute > 59 || d.second > 59) return ERR_VALUE;

    d.seconds = datetime_to_seconds(&d);
    *out = d;
    return SUCCESS;
}

// чтение записей

static status_t parse_marker(const char *s, marker_t *out) {
    if (s == NULL || out == NULL) return ERR_NULL;

    if (strcmp(s, "S") == 0 || strcmp(s, "start") == 0) *out = MARKER_START;
    else if (strcmp(s, "M") == 0 || strcmp(s, "middle") == 0) *out = MARKER_MIDDLE;
    else if (strcmp(s, "E") == 0 || strcmp(s, "end") == 0) *out = MARKER_END;
    else return ERR_VALUE;

    return SUCCESS;
}

static status_t read_datetime(const char **cursor, DateTime *out) {
    String date = {0};
    String clock = {0};

    status_t st = next_token(cursor, &date);
    if (st == SUCCESS) st = next_token(cursor, &clock);
    if (st == SUCCESS) st = parse_datetime(date.data, clock.data, out);

    string_clear(&date);
    string_clear(&clock);
    return st;
}

static status_t parse_point(const char *line, Point *out) {
    if (line == NULL || out == NULL) return ERR_NULL;

    String tok = {0};
    const char *cur = line;
    Point p = {0.0, 0.0};

    status_t st = next_token(&cur, &tok);
    if (st == SUCCESS) st = parse_coordinate(tok.data, &p.x);
    string_clear(&tok);

    if (st == SUCCESS) st = next_token(&cur, &tok);
    if (st == SUCCESS) st = parse_coordinate(tok.data, &p.y);
    string_clear(&tok);

    if (st == SUCCESS && has_more_tokens(cur)) st = ERR_VALUE;
    if (st == SUCCESS) *out = p;
    return st;
}

static status_t parse_record(const char *line, const Point *point, Record *out) {
    if (line == NULL || point == NULL || out == NULL) return ERR_NULL;

    Record r = {0};
    String tok = {0};
    const char *cur = line;

    status_t st = next_token(&cur, &r.vehicle);
    if (st == SUCCESS) st = read_datetime(&cur, &r.stop.arrival);
    if (st == SUCCESS) st = read_datetime(&cur, &r.stop.departure);
    if (st == SUCCESS && r.stop.departure.seconds < r.stop.arrival.seconds) st = ERR_VALUE;

    if (st == SUCCESS) st = next_token(&cur, &tok);
    if (st == SUCCESS) st = parse_marker(tok.data, &r.stop.marker);
    string_clear(&tok);

    if (st == SUCCESS && has_more_tokens(cur)) st = ERR_VALUE;

    if (st != SUCCESS) {
        string_clear(&r.vehicle);
        return st;
    }

    r.stop.point = *point;
    *out = r;
    return SUCCESS;
}

static void records_clear(RecordArray *arr) {
    if (arr == NULL) return;

    for (size_t i = 0; i < arr->count; i++) string_clear(&arr->items[i].vehicle);
    free(arr->items);

    arr->items = NULL;
    arr->count = 0;
    arr->capacity = 0;
}

static status_t records_add(RecordArray *arr, Record *record) {
    if (arr == NULL || record == NULL) return ERR_NULL;

    if (arr->count == arr->capacity) {
        if (arr->capacity > SIZE_MAX / sizeof(Record) / 2) return ERR_MEMORY;

        size_t new_cap = (arr->capacity == 0) ? 16 : arr->capacity * 2;
        Record *tmp = realloc(arr->items, new_cap * sizeof *tmp);
        if (tmp == NULL) return ERR_MEMORY;
        arr->items = tmp;
        arr->capacity = new_cap;
    }

    arr->items[arr->count] = *record;
    *record = (Record){0};
    arr->count++;
    return SUCCESS;
}

static status_t read_line(FILE *in, String *out) {
    if (in == NULL || out == NULL) return ERR_NULL;
    if (out->data != NULL) return ERR_VALUE;

    size_t cap = 32, len = 0;
    char *buf = malloc(cap);
    if (buf == NULL) return ERR_MEMORY;

    int c;
    while ((c = fgetc(in)) != EOF && c != '\n') {
        if (len + 1 >= cap) {
            if (cap > ((size_t)INT_MAX + 1) / 2) { free(buf); return ERR_VALUE; }

            size_t new_cap = cap * 2;
            char *nb = realloc(buf, new_cap);
            if (nb == NULL) { free(buf); return ERR_MEMORY; }
            buf = nb;
            cap = new_cap;
        }
        buf[len++] = (char)c;
    }

    if (ferror(in)) { free(buf); return ERR_FILE; }
    if (c == EOF && len == 0) { free(buf); return ERR_EOF; }
    while (len > 0 && (buf[len-1]==' ' || buf[len-1]=='\t' || buf[len-1]=='\r')) len--;
    buf[len] = '\0';

    out->data = buf;
    out->length = (int)len;
    return SUCCESS;
}

static status_t records_load_file(const char *path, RecordArray *arr, size_t *error_line) {
    if (path == NULL || arr == NULL || error_line == NULL) return ERR_NULL;

    FILE *f = fopen(path, "r");
    if (f == NULL) return ERR_FILE;

    status_t st = SUCCESS;
    size_t line_no = 0;
    int reading = 1;
    int have_point = 0;
    Point point = {0.0, 0.0};

    while (reading) {
        String line = {0};
        st = read_line(f, &line);
        line_no++;

        if (st == ERR_EOF) {
            reading = 0;
            st = SUCCESS;
            if (!have_point) {
                st = ERR_VALUE;
                *error_line = line_no;
            }
        } else if (st != SUCCESS) {
            *error_line = line_no;
            reading = 0;
        } else {
            if (line.length > 0) {
                if (!have_point) {
                    st = parse_point(line.data, &point);
                    have_point = (st == SUCCESS);
                } else {
                    Record r = {0};
                    st = parse_record(line.data, &point, &r);
                    if (st == SUCCESS) st = records_add(arr, &r);
                    string_clear(&r.vehicle);
                }

                if (st != SUCCESS) {
                    *error_line = line_no;
                    reading = 0;
                }
            }
            string_clear(&line);
        }
    }

    if (fclose(f) != 0 && st == SUCCESS) st = ERR_FILE;
    return st;
}

// хранилище

static void storage_clear(Storage *storage) {
    if (storage == NULL) return;

    RouteNode *route = storage->head;
    while (route != NULL) {
        StopNode *stop = route->head;
        while (stop != NULL) {
            StopNode *next_stop = stop->next;
            free(stop);
            stop = next_stop;
        }

        RouteNode *next_route = route->next;
        string_clear(&route->vehicle);
        free(route);
        route = next_route;
    }

    storage->head = NULL;
    storage->tail = NULL;
    storage->count = 0;
}

static status_t storage_start_route(Storage *storage, String *vehicle, RouteNode **out) {
    if (storage == NULL || vehicle == NULL || out == NULL) return ERR_NULL;

    RouteNode *node = malloc(sizeof *node);
    if (node == NULL) return ERR_MEMORY;

    node->vehicle = *vehicle;
    *vehicle = (String){0};
    node->head = NULL;
    node->tail = NULL;
    node->stops_count = 0;
    node->next = NULL;

    if (storage->tail == NULL) storage->head = node;
    else storage->tail->next = node;
    storage->tail = node;
    storage->count++;

    *out = node;
    return SUCCESS;
}

static status_t route_append_stop(RouteNode *route, const Stop *stop) {
    if (route == NULL || stop == NULL) return ERR_NULL;

    StopNode *node = malloc(sizeof *node);
    if (node == NULL) return ERR_MEMORY;

    node->stop = *stop;
    node->next = NULL;

    if (route->tail == NULL) route->head = node;
    else route->tail->next = node;
    route->tail = node;
    route->stops_count++;
    return SUCCESS;
}

static int marker_rank(marker_t m) {
    if (m == MARKER_END) return 0;
    if (m == MARKER_MIDDLE) return 1;
    return 2;
}

static int compare_records(const void *a, const void *b) {
    const Record *x = a;
    const Record *y = b;

    int r = strcmp(x->vehicle.data, y->vehicle.data);
    if (r != 0) return r;

    if (x->stop.arrival.seconds != y->stop.arrival.seconds)
        return (x->stop.arrival.seconds > y->stop.arrival.seconds) ? 1 : -1;

    r = marker_rank(x->stop.marker) - marker_rank(y->stop.marker);
    if (r != 0) return r;

    if (x->stop.departure.seconds != y->stop.departure.seconds)
        return (x->stop.departure.seconds > y->stop.departure.seconds) ? 1 : -1;
    return 0;
}

static status_t report_bad_route(const String *vehicle, LoadInfo *info) {
    if (vehicle != NULL && info != NULL && info->vehicle.data == NULL)
        string_from_range(vehicle->data, (size_t)vehicle->length, &info->vehicle);
    return ERR_ROUTE;
}

static status_t storage_build(RecordArray *records, Storage *storage, LoadInfo *info) {
    if (records == NULL || storage == NULL || info == NULL) return ERR_NULL;

    if (records->count > 1) qsort(records->items, records->count, sizeof(Record), compare_records);

    RouteNode *route = NULL;
    status_t st = SUCCESS;

    for (size_t i = 0; i < records->count && st == SUCCESS; i++) {
        Record *r = &records->items[i];

        if (route != NULL && strcmp(route->vehicle.data, r->vehicle.data) != 0) {
            st = report_bad_route(&route->vehicle, info);
        } else if (r->stop.marker == MARKER_START) {
            if (route != NULL) st = report_bad_route(&r->vehicle, info);
            else st = storage_start_route(storage, &r->vehicle, &route);
            if (st == SUCCESS) st = route_append_stop(route, &r->stop);
        } else {
            if (route == NULL) st = report_bad_route(&r->vehicle, info);
            else st = route_append_stop(route, &r->stop);
            if (st == SUCCESS && r->stop.marker == MARKER_END) route = NULL;
        }
    }

    if (st == SUCCESS && route != NULL) st = report_bad_route(&route->vehicle, info);
    return st;
}

static status_t storage_load_files(size_t count, char *const paths[], Storage *storage,
                                   size_t *total_records, LoadInfo *info) {
    if (paths == NULL || storage == NULL || total_records == NULL || info == NULL) return ERR_NULL;

    RecordArray records = {0};
    status_t st = SUCCESS;

    for (size_t i = 0; i < count && st == SUCCESS; i++) {
        info->path = paths[i];
        st = records_load_file(paths[i], &records, &info->line);
    }

    if (st == SUCCESS) {
        *total_records = records.count;
        st = storage_build(&records, storage, info);
    }

    records_clear(&records);
    return st;
}

// статистика

static status_t route_metrics(const RouteNode *route, double *dist,
                              long long *stop_min, long long *stop_max, long long *idle) {
    if (route == NULL || dist == NULL || stop_min == NULL || stop_max == NULL || idle == NULL) return ERR_NULL;

    double sum = 0.0;
    long long mn = LLONG_MAX, mx = 0, id = 0;
    const StopNode *prev = NULL;

    for (const StopNode *cur = route->head; cur != NULL; cur = cur->next) {
        long long d = cur->stop.departure.seconds - cur->stop.arrival.seconds;
        if (d < mn) mn = d;
        if (d > mx) mx = d;
        id += d;
        if (prev != NULL)
            sum += hypot(cur->stop.point.x - prev->stop.point.x,
                         cur->stop.point.y - prev->stop.point.y);
        prev = cur;
    }
    if (!isfinite(sum)) return ERR_VALUE;

    *dist = sum;
    *stop_min = mn;
    *stop_max = mx;
    *idle = id;
    return SUCCESS;
}

static status_t storage_build_stats(const Storage *storage, double eps,
                                    VehicleStat **out_stats, size_t *out_count) {
    if (storage == NULL || out_stats == NULL || out_count == NULL) return ERR_NULL;

    *out_stats = NULL;
    *out_count = 0;
    if (storage->count == 0) return SUCCESS;

    VehicleStat *stats = malloc(storage->count * sizeof *stats);
    if (stats == NULL) return ERR_MEMORY;

    size_t n = 0;
    status_t st = SUCCESS;

    for (const RouteNode *route = storage->head; route != NULL && st == SUCCESS; route = route->next) {
        double dist = 0.0;
        long long smin = 0, smax = 0, idle = 0;
        st = route_metrics(route, &dist, &smin, &smax, &idle);
        if (st != SUCCESS) break;

        VehicleStat *s;
        if (n > 0 && strcmp(stats[n-1].vehicle->data, route->vehicle.data) == 0) {
            s = &stats[n-1];
            int cmp = 0;
            st = doubles_compare(dist, s->longest_route, eps, &cmp);
            if (st == SUCCESS && cmp > 0) s->longest_route = dist;
            if (st == SUCCESS) st = doubles_compare(s->shortest_route, dist, eps, &cmp);
            if (st == SUCCESS && cmp > 0) s->shortest_route = dist;
            if (smax > s->longest_stop) s->longest_stop = smax;
            if (smin < s->shortest_stop) s->shortest_stop = smin;
        } else {
            s = &stats[n++];
            *s = (VehicleStat){0};
            s->vehicle = &route->vehicle;
            s->longest_route = dist;
            s->shortest_route = dist;
            s->longest_stop = smax;
            s->shortest_stop = smin;
        }
        if (st != SUCCESS) break;

        s->routes++;
        s->total_distance += dist;
        s->total_idle += idle;
    }

    if (st != SUCCESS) {
        free(stats);
        return st;
    }

    *out_stats = stats;
    *out_count = n;
    return SUCCESS;
}

static status_t stat_metric(const VehicleStat *s, metric_t metric, double *out) {
    if (s == NULL || out == NULL) return ERR_NULL;

    switch (metric) {
        case METRIC_ROUTES: *out = (double)s->routes; break;
        case METRIC_TOTAL_DISTANCE: *out = s->total_distance; break;
        case METRIC_LONGEST_ROUTE: *out = s->longest_route; break;
        case METRIC_SHORTEST_ROUTE: *out = s->shortest_route; break;
        case METRIC_LONGEST_STOP: *out = (double)s->longest_stop; break;
        case METRIC_SHORTEST_STOP: *out = (double)s->shortest_stop; break;
        case METRIC_TOTAL_IDLE: *out = (double)s->total_idle; break;
        default: return ERR_VALUE;
    }
    return SUCCESS;
}

static status_t stats_find_extreme(const VehicleStat *stats, size_t count, metric_t metric, int want_max, double eps, const VehicleStat **found, size_t *found_count, double *best) {
    if (stats == NULL || found == NULL || found_count == NULL || best == NULL) return ERR_NULL;
    if (count == 0) return ERR_NOT_FOUND;

    double best_value = 0.0;
    status_t st = stat_metric(&stats[0], metric, &best_value);
    if (st != SUCCESS) return st;

    for (size_t i = 1; i < count; i++) {
        double v = 0.0;
        int cmp = 0;
        st = stat_metric(&stats[i], metric, &v);
        if (st != SUCCESS) return st;
        st = doubles_compare(v, best_value, eps, &cmp);
        if (st != SUCCESS) return st;
        if ((want_max && cmp > 0) || (!want_max && cmp < 0)) best_value = v;
    }

    size_t k = 0;
    for (size_t i = 0; i < count; i++) {
        double v = 0.0;
        int cmp = 0;
        st = stat_metric(&stats[i], metric, &v);
        if (st == SUCCESS) st = doubles_compare(v, best_value, eps, &cmp);
        if (st != SUCCESS) return st;
        if (cmp == 0) found[k++] = &stats[i];
    }

    *found_count = k;
    *best = best_value;
    return SUCCESS;
}

// вывод

static void print_datetime(const DateTime *d) {
    printf("%02u.%02u.%04u %02u:%02u:%02u", d->day, d->month, d->year, d->hour, d->minute, d->second);
}

static void print_duration(long long seconds) {
    printf("%lld ч %02lld мин %02lld с", seconds / 3600, (seconds % 3600) / 60, seconds % 60);
}

static void print_route(size_t number, const RouteNode *route) {
    printf("Маршрут %zu: транспорт %s, остановок: %zu\n", number, route->vehicle.data, route->stops_count);

    for (const StopNode *cur = route->head; cur != NULL; cur = cur->next) {
        const char *kind = "промежуточная";
        if (cur->stop.marker == MARKER_START) kind = "начальная";
        else if (cur->stop.marker == MARKER_END) kind = "конечная";

        printf("  (%.4f; %.4f) %s: прибытие ", cur->stop.point.x, cur->stop.point.y, kind);
        print_datetime(&cur->stop.arrival);
        printf(", отправление ");
        print_datetime(&cur->stop.departure);
        printf("\n");
    }
}

static void print_vehicle_stat(const VehicleStat *s) {
    printf("  %s: маршрутов %zu, путь %.3f, маршруты от %.3f до %.3f, остановка от ", s->vehicle->data, s->routes, s->total_distance, s->shortest_route, s->longest_route);
    print_duration(s->shortest_stop);
    printf(" до ");
    print_duration(s->longest_stop);
    printf(", простой ");
    print_duration(s->total_idle);
    printf("\n");
}

static void print_metric_value(metric_t metric, double value) {
    switch (metric) {
        case METRIC_ROUTES:
            printf("%.0f", value);
            break;
        case METRIC_TOTAL_DISTANCE:
        case METRIC_LONGEST_ROUTE:
        case METRIC_SHORTEST_ROUTE:
            printf("%.3f", value);
            break;
        default:
            print_duration((long long)value);
            break;
    }
}

static void print_menu(void) {
    printf("\nМеню транспорта\n"
           "1. Показать все маршруты\n"
           "2. Транспорт, проехавший больше всех маршрутов\n"
           "3. Транспорт, проехавший меньше всех маршрутов\n"
           "4. Транспорт, проехавший самый длинный путь\n"
           "5. Транспорт, проехавший самый короткий путь\n"
           "6. Транспорт с самым длинным маршрутом\n"
           "7. Транспорт с самым коротким маршрутом\n"
           "8. Транспорт с самой долгой остановкой\n"
           "9. Транспорт с самой короткой остановкой\n"
           "10. Транспорт с самым большим временем простоя\n"
           "0. Выход\n"
           "Выберите действие: ");
    fflush(stdout);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Использование: %s <файл_остановки_1> [<файл_остановки_2> ...]\n", argv[0]);
        return 1;
    }

    const double eps = 1e-6;

    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '\0') {
            fprintf(stderr, "Ошибка: путь к файлу не должен быть пустым\n");
            return 1;
        }
    }

    Storage storage = {0};
    LoadInfo info = {0};
    size_t total_records = 0;

    status_t st = storage_load_files((size_t)(argc - 1), argv + 1, &storage, &total_records, &info);
    if (st != SUCCESS) {
        switch (st) {
            case ERR_FILE:
                fprintf(stderr, "Ошибка: не удалось прочитать файл '%s'\n", info.path);
                break;
            case ERR_VALUE:
                fprintf(stderr, "Ошибка: неверный формат данных в файле '%s', строка %zu\n", info.path, info.line);
                break;
            case ERR_ROUTE:
                fprintf(stderr, "Ошибка: у транспорта '%s' нарушена цепочка остановок (начальная, промежуточные, конечная)\n",
                        info.vehicle.data != NULL ? info.vehicle.data : "?");
                break;
            default:
                fprintf(stderr, "Ошибка загрузки (%s)\n", status_text(st));
                break;
        }
        string_clear(&info.vehicle);
        storage_clear(&storage);
        return 1;
    }
    string_clear(&info.vehicle);

    VehicleStat *stats = NULL;
    size_t stats_count = 0;
    st = storage_build_stats(&storage, eps, &stats, &stats_count);
    if (st != SUCCESS) {
        fprintf(stderr, "Ошибка подсчёта статистики (%s)\n", status_text(st));
        storage_clear(&storage);
        return 1;
    }

    const VehicleStat **found = malloc((stats_count ? stats_count : 1) * sizeof *found);
    if (found == NULL) {
        fprintf(stderr, "Ошибка: %s\n", status_text(ERR_MEMORY));
        free(stats);
        storage_clear(&storage);
        return 1;
    }

    printf("Файлов: %d, записей: %zu, маршрутов: %zu, транспортных средств: %zu\n", argc - 1, total_records, storage.count, stats_count);

    const Query queries[] = {
        {METRIC_ROUTES, 1, "Проехало больше всех маршрутов"},
        {METRIC_ROUTES, 0, "Проехало меньше всех маршрутов"},
        {METRIC_TOTAL_DISTANCE, 1, "Самый длинный пройденный путь"},
        {METRIC_TOTAL_DISTANCE, 0, "Самый короткий пройденный путь"},
        {METRIC_LONGEST_ROUTE, 1, "Самый длинный маршрут"},
        {METRIC_SHORTEST_ROUTE, 0, "Самый короткий маршрут"},
        {METRIC_LONGEST_STOP, 1, "Самая долгая остановка"},
        {METRIC_SHORTEST_STOP, 0, "Самая короткая остановка"},
        {METRIC_TOTAL_IDLE, 1, "Самое большое время простоя"}
    };

    int running = 1;
    while (running) {
        print_menu();
        String line = {0};
        st = read_line(stdin, &line);
        if (st != SUCCESS) {
            string_clear(&line);
            if (st != ERR_EOF) fprintf(stderr, "Ошибка ввода (%s)\n", status_text(st));
            break;
        }

        unsigned int choice = 0;
        status_t parse_st = parse_uint(line.data, &choice);
        string_clear(&line);
        if (parse_st != SUCCESS || choice > 10) {
            printf("Неверный пункт меню\n");
            continue;
        }

        if (choice != 0 && storage.count == 0) {
            printf("Нет данных о маршрутах\n");
            continue;
        }

        switch (choice) {
            case 0:
                running = 0;
                break;
            case 1: {
                size_t number = 1;
                for (const RouteNode *route = storage.head; route != NULL; route = route->next)
                    print_route(number++, route);
                break;
            }
            default: {
                Query q = queries[choice - 2];
                size_t n = 0;
                double best = 0.0;

                st = stats_find_extreme(stats, stats_count, q.metric, q.want_max, eps, found, &n, &best);
                switch (st) {
                    case SUCCESS:
                        printf("%s: ", q.title);
                        print_metric_value(q.metric, best);
                        printf(" (транспортных средств: %zu)\n", n);
                        for (size_t i = 0; i < n; i++) print_vehicle_stat(found[i]);
                        break;
                    case ERR_NOT_FOUND:
                        printf("Нет данных о маршрутах\n");
                        break;
                    default:
                        printf("Ошибка (%s)\n", status_text(st));
                        if (st == ERR_MEMORY) running = 0;
                        break;
                }
                break;
            }
        }
    }

    free(found);
    free(stats);
    storage_clear(&storage);
    printf("Программа завершена.\n");
    return 0;
}