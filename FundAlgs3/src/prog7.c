#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <math.h>
#include <time.h>
#include <sys/stat.h>

// 7

typedef enum {
    SUCCESS = 0,
    ERR_NULL,
    ERR_MEMORY,
    ERR_VALUE,
    ERR_NOT_FOUND,
    ERR_EOF,
    ERR_FILE,
    ERR_SAME_FILE
} status_t;

enum {
    MIN_YEAR = 1900
};

typedef enum {
    OP_ADD = 0,
    OP_DELETE,
    OP_EDIT
} op_type_t;

typedef enum {
    FIELD_SURNAME = 0,
    FIELD_NAME,
    FIELD_PATRONYMIC,
    FIELD_BIRTH,
    FIELD_GENDER,
    FIELD_INCOME,
    FIELD_COUNT
} field_t;

typedef struct {
    char *data;
    int length;
} String;

typedef struct {
    unsigned int day;
    unsigned int month;
    unsigned int year;
} Date;

typedef struct {
    String surname;
    String name;
    String patronymic;
    Date birth;
    char gender;
    double income;
} Liver;

typedef struct Node {
    Liver liver;
    struct Node *next;
} Node;

typedef struct {
    Node *head;
    size_t count;
} LiverList;

typedef struct {
    op_type_t type;
    Node *node;
    Liver old_liver;
} HistoryEntry;

typedef struct {
    HistoryEntry *items;
    size_t count;
    size_t capacity;
} History;

typedef struct {
    int use[FIELD_COUNT];
    Liver values;
} Criteria;

// String

static status_t string_equal(const String *s1, const String *s2, int *result) {
    if (s1 == NULL || s2 == NULL || result == NULL) return ERR_NULL;
    if (s1->data == NULL || s2->data == NULL) return ERR_VALUE;

    *result = (s1->length == s2->length && strcmp(s1->data, s2->data) == 0);
    return SUCCESS;
}

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

static status_t string_duplicate(const String *src, String *dest) {
    if (src == NULL || dest == NULL) return ERR_NULL;
    if (src->data == NULL) return ERR_VALUE;

    return string_from_range(src->data, (size_t)src->length, dest);
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
        case ERR_SAME_FILE: return "файлы совпадают";
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

static status_t parse_income(const char *s, double *out) {
    if (s == NULL || out == NULL) return ERR_NULL;
    if (*s == '\0') return ERR_VALUE;

    int dots = 0;
    for (int i = 0; s[i] != '\0'; i++) {
        if (s[i] == '.') {
            dots++;
            if (dots > 1 || i == 0 || s[i + 1] == '\0') return ERR_VALUE;
        } else if (s[i] < '0' || s[i] > '9') {
            return ERR_VALUE;
        }
    }

    char *end = NULL;
    double v = strtod(s, &end);
    if (end == s || *end != '\0') return ERR_VALUE;
    if (!isfinite(v) || v < 0.0) return ERR_VALUE;

    *out = v;
    return SUCCESS;
}

static status_t parse_gender(const char *s, char *out) {
    if (s == NULL || out == NULL) return ERR_NULL;
    if ((s[0] != 'M' && s[0] != 'W') || s[1] != '\0') return ERR_VALUE;

    *out = s[0];
    return SUCCESS;
}

static status_t validate_name(const String *s, int allow_empty) {
    if (s == NULL || s->data == NULL) return ERR_NULL;
    if (s->length == 0) return allow_empty ? SUCCESS : ERR_VALUE;

    for (int i = 0; i < s->length; i++) {
        char c = s->data[i];
        if (!((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))) return ERR_VALUE;
    }
    return SUCCESS;
}

static status_t doubles_equal(double a, double b, double eps, int *result) {
    if (result == NULL) return ERR_NULL;
    if (!isfinite(a) || !isfinite(b)) return ERR_VALUE;
    if (!isfinite(eps) || eps <= 0.0) return ERR_VALUE;

    *result = (fabs(a - b) <= eps);
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

static size_t count_tokens(const char *s) {
    if (s == NULL) return 0;

    size_t n = 0;
    while (*s != '\0') {
        while (*s == ' ' || *s == '\t') s++;
        if (*s == '\0') break;

        n++;
        while (*s != '\0' && *s != ' ' && *s != '\t') s++;
    }
    return n;
}

// даты

static unsigned int days_in_month(unsigned int m, unsigned int y) {
    const unsigned int d[] = {31,28,31,30,31,30,31,31,30,31,30,31};
    if (m == 2 && ((y % 4 == 0 && y % 100 != 0) || y % 400 == 0)) return 29;
    return d[m - 1];
}

static int date_compare(const Date *a, const Date *b) {
    if (a->year != b->year) return (a->year > b->year) ? 1 : -1;
    if (a->month != b->month) return (a->month > b->month) ? 1 : -1;
    if (a->day != b->day) return (a->day > b->day) ? 1 : -1;
    return 0;
}

static status_t parse_date(const char *s, Date *out) {
    if (s == NULL || out == NULL) return ERR_NULL;

    unsigned int parts[3] = {0, 0, 0};
    int digits[3] = {0, 0, 0};
    int idx = 0;

    for (int i = 0; s[i] != '\0'; i++) {
        if (s[i] == '.') {
            idx++;
            if (idx > 2) return ERR_VALUE;
            continue;
        }
        if (s[i] < '0' || s[i] > '9') return ERR_VALUE;
        if (digits[idx] >= 4) return ERR_VALUE;

        parts[idx] = parts[idx] * 10u + (unsigned int)(s[i] - '0');
        digits[idx]++;
    }

    if (idx != 2) return ERR_VALUE;
    if (digits[0] < 1 || digits[0] > 2 || digits[1] < 1 || digits[1] > 2 || digits[2] != 4) return ERR_VALUE;

    unsigned int d = parts[0], m = parts[1], y = parts[2];
    if (m < 1 || m > 12) return ERR_VALUE;
    if (d < 1 || d > days_in_month(m, y)) return ERR_VALUE;

    out->day = d;
    out->month = m;
    out->year = y;
    return SUCCESS;
}

static status_t validate_birth_date(const Date *d) {
    if (d == NULL) return ERR_NULL;
    if (d->year < MIN_YEAR) return ERR_VALUE;

    time_t now = time(NULL);
    if (now == (time_t)-1) return ERR_VALUE;

    struct tm *lt = localtime(&now);
    if (lt == NULL) return ERR_VALUE;

    Date today;
    today.day = (unsigned int)lt->tm_mday;
    today.month = (unsigned int)lt->tm_mon + 1;
    today.year = (unsigned int)lt->tm_year + 1900;

    if (date_compare(d, &today) > 0) return ERR_VALUE;
    return SUCCESS;
}

// Liver

static void liver_clear(Liver *l) {
    if (l == NULL) return;

    string_clear(&l->surname);
    string_clear(&l->name);
    string_clear(&l->patronymic);

    l->birth = (Date){0, 0, 0};
    l->gender = 'M';
    l->income = 0.0;
}

static status_t liver_copy(const Liver *src, Liver *dest) {
    if (src == NULL || dest == NULL) return ERR_NULL;

    Liver tmp = {0};
    status_t st = string_duplicate(&src->surname, &tmp.surname);
    if (st == SUCCESS) st = string_duplicate(&src->name, &tmp.name);
    if (st == SUCCESS) st = string_duplicate(&src->patronymic, &tmp.patronymic);
    if (st != SUCCESS) {
        liver_clear(&tmp);
        return st;
    }

    tmp.birth = src->birth;
    tmp.gender = src->gender;
    tmp.income = src->income;

    *dest = tmp;
    return SUCCESS;
}

static status_t parse_liver(const char *line, Liver *out) {
    if (line == NULL || out == NULL) return ERR_NULL;

    size_t n = count_tokens(line);
    if (n != 5 && n != 6) return ERR_VALUE;

    Liver l = {0};
    String tok = {0};
    const char *cur = line;

    status_t st = next_token(&cur, &l.surname);
    if (st == SUCCESS) st = validate_name(&l.surname, 0);

    if (st == SUCCESS) st = next_token(&cur, &l.name);
    if (st == SUCCESS) st = validate_name(&l.name, 0);

    if (st == SUCCESS) {
        if (n == 6) {
            st = next_token(&cur, &l.patronymic);
            if (st == SUCCESS) st = validate_name(&l.patronymic, 0);
        } else {
            st = string_from_range("", 0, &l.patronymic);
        }
    }

    if (st == SUCCESS) st = next_token(&cur, &tok);
    if (st == SUCCESS) st = parse_date(tok.data, &l.birth);
    if (st == SUCCESS) st = validate_birth_date(&l.birth);
    string_clear(&tok);

    if (st == SUCCESS) st = next_token(&cur, &tok);
    if (st == SUCCESS) st = parse_gender(tok.data, &l.gender);
    string_clear(&tok);

    if (st == SUCCESS) st = next_token(&cur, &tok);
    if (st == SUCCESS) st = parse_income(tok.data, &l.income);
    string_clear(&tok);

    if (st != SUCCESS) {
        liver_clear(&l);
        return st;
    }

    *out = l;
    return SUCCESS;
}

// список

static void node_destroy(Node *node) {
    if (node == NULL) return;

    liver_clear(&node->liver);
    free(node);
}

static void list_clear(LiverList *list) {
    if (list == NULL) return;

    Node *cur = list->head;
    while (cur != NULL) {
        Node *next = cur->next;
        node_destroy(cur);
        cur = next;
    }
    list->head = NULL;
    list->count = 0;
}

static status_t list_insert_node(LiverList *list, Node *node) {
    if (list == NULL || node == NULL) return ERR_NULL;

    Node *prev = NULL;
    Node *cur = list->head;
    while (cur != NULL && date_compare(&cur->liver.birth, &node->liver.birth) >= 0) {
        prev = cur;
        cur = cur->next;
    }

    node->next = cur;
    if (prev == NULL) list->head = node;
    else prev->next = node;

    list->count++;
    return SUCCESS;
}

static status_t list_unlink_node(LiverList *list, Node *node) {
    if (list == NULL || node == NULL) return ERR_NULL;

    Node *prev = NULL;
    Node *cur = list->head;
    while (cur != NULL && cur != node) {
        prev = cur;
        cur = cur->next;
    }
    if (cur == NULL) return ERR_NOT_FOUND;

    if (prev == NULL) list->head = cur->next;
    else prev->next = cur->next;

    node->next = NULL;
    list->count--;
    return SUCCESS;
}

static status_t list_add_liver(LiverList *list, Liver *liver, Node **out_node) {
    if (list == NULL || liver == NULL) return ERR_NULL;

    Node *node = malloc(sizeof *node);
    if (node == NULL) return ERR_MEMORY;

    node->liver = *liver;
    node->next = NULL;

    status_t st = list_insert_node(list, node);
    if (st != SUCCESS) {
        free(node);
        return st;
    }

    *liver = (Liver){0};
    if (out_node != NULL) *out_node = node;
    return SUCCESS;
}

// история

static status_t history_reserve(History *h) {
    if (h == NULL) return ERR_NULL;
    if (h->count < h->capacity) return SUCCESS;

    if (h->capacity > SIZE_MAX / sizeof(HistoryEntry) / 2) return ERR_MEMORY;

    size_t new_cap = (h->capacity == 0) ? 8 : h->capacity * 2;
    HistoryEntry *tmp = realloc(h->items, new_cap * sizeof *tmp);
    if (tmp == NULL) return ERR_MEMORY;

    h->items = tmp;
    h->capacity = new_cap;
    return SUCCESS;
}

static status_t history_push(History *h, op_type_t type, Node *node, Liver *old_liver) {
    if (h == NULL || node == NULL) return ERR_NULL;
    if (h->count >= h->capacity) return ERR_MEMORY;

    HistoryEntry *e = &h->items[h->count];
    e->type = type;
    e->node = node;
    e->old_liver = (Liver){0};
    if (old_liver != NULL) {
        e->old_liver = *old_liver;
        *old_liver = (Liver){0};
    }

    h->count++;
    return SUCCESS;
}

static void history_commit(History *h) {
    if (h == NULL) return;

    for (size_t i = 0; i < h->count; i++) {
        HistoryEntry *e = &h->items[i];
        if (e->type == OP_DELETE) node_destroy(e->node);
        else if (e->type == OP_EDIT) liver_clear(&e->old_liver);
        e->node = NULL;
    }
    h->count = 0;
}

static void history_destroy(History *h) {
    if (h == NULL) return;

    history_commit(h);
    free(h->items);
    h->items = NULL;
    h->capacity = 0;
}

// изменения

static status_t registry_add(LiverList *list, History *history, Liver *liver) {
    if (list == NULL || history == NULL || liver == NULL) return ERR_NULL;

    status_t st = history_reserve(history);
    if (st != SUCCESS) return st;

    Node *node = NULL;
    st = list_add_liver(list, liver, &node);
    if (st != SUCCESS) return st;

    return history_push(history, OP_ADD, node, NULL);
}

static status_t registry_delete(LiverList *list, History *history, Node *node) {
    if (list == NULL || history == NULL || node == NULL) return ERR_NULL;

    status_t st = history_reserve(history);
    if (st != SUCCESS) return st;

    st = list_unlink_node(list, node);
    if (st != SUCCESS) return st;

    return history_push(history, OP_DELETE, node, NULL);
}

static status_t registry_edit(LiverList *list, History *history, Node *node, Liver *updated) {
    if (list == NULL || history == NULL || node == NULL || updated == NULL) return ERR_NULL;

    status_t st = history_reserve(history);
    if (st != SUCCESS) return st;

    st = list_unlink_node(list, node);
    if (st != SUCCESS) return st;

    Liver old = node->liver;
    node->liver = *updated;
    *updated = (Liver){0};

    st = list_insert_node(list, node);
    if (st != SUCCESS) return st;

    st = history_push(history, OP_EDIT, node, &old);
    if (st != SUCCESS) liver_clear(&old);
    return st;
}

static status_t undo_entry(LiverList *list, HistoryEntry *e) {
    if (list == NULL || e == NULL) return ERR_NULL;

    status_t st = SUCCESS;
    switch (e->type) {
        case OP_ADD:
            st = list_unlink_node(list, e->node);
            if (st == SUCCESS) {
                node_destroy(e->node);
                e->node = NULL;
            }
            break;
        case OP_DELETE:
            st = list_insert_node(list, e->node);
            if (st == SUCCESS) e->node = NULL;
            break;
        case OP_EDIT:
            st = list_unlink_node(list, e->node);
            if (st == SUCCESS) {
                liver_clear(&e->node->liver);
                e->node->liver = e->old_liver;
                e->old_liver = (Liver){0};
                st = list_insert_node(list, e->node);
                e->node = NULL;
            }
            break;
        default:
            st = ERR_VALUE;
            break;
    }
    return st;
}

static status_t registry_undo(LiverList *list, History *history, size_t *undone) {
    if (list == NULL || history == NULL || undone == NULL) return ERR_NULL;

    *undone = 0;
    size_t k = history->count / 2;
    if (k == 0) return SUCCESS;

    for (size_t i = 0; i < k; i++) {
        HistoryEntry *e = &history->items[history->count - 1];
        status_t st = undo_entry(list, e);
        if (st != SUCCESS) return st;

        history->count--;
        (*undone)++;
    }

    history_commit(history);
    return SUCCESS;
}

// поиск

static status_t liver_matches(const Liver *l, const Criteria *c, double eps, int *take) {
    if (l == NULL || c == NULL || take == NULL) return ERR_NULL;

    int match = 1;
    for (int f = 0; f < FIELD_COUNT && match; f++) {
        if (!c->use[f]) continue;

        int eq = 0;
        status_t st = SUCCESS;
        switch ((field_t)f) {
            case FIELD_SURNAME: st = string_equal(&l->surname, &c->values.surname, &eq); break;
            case FIELD_NAME: st = string_equal(&l->name, &c->values.name, &eq); break;
            case FIELD_PATRONYMIC: st = string_equal(&l->patronymic, &c->values.patronymic, &eq); break;
            case FIELD_BIRTH: eq = (date_compare(&l->birth, &c->values.birth) == 0); break;
            case FIELD_GENDER: eq = (l->gender == c->values.gender); break;
            case FIELD_INCOME: st = doubles_equal(l->income, c->values.income, eps, &eq); break;
            default: st = ERR_VALUE; break;
        }
        if (st != SUCCESS) return st;
        if (!eq) match = 0;
    }

    *take = match;
    return SUCCESS;
}

static status_t list_search(const LiverList *list, const Criteria *c, double eps,
                            Node **found, size_t *out_count) {
    if (list == NULL || c == NULL || found == NULL || out_count == NULL) return ERR_NULL;

    size_t k = 0;
    for (Node *cur = list->head; cur != NULL; cur = cur->next) {
        int take = 0;
        status_t st = liver_matches(&cur->liver, c, eps, &take);
        if (st != SUCCESS) return st;
        if (take) found[k++] = cur;
    }

    *out_count = k;
    return (k == 0) ? ERR_NOT_FOUND : SUCCESS;
}

// ввод вывод

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

static status_t read_field(const char *prompt, String *out) {
    printf("%s", prompt);
    fflush(stdout);
    return read_line(stdin, out);
}

static status_t check_different_files(const char *in_path, const char *out_path) {
    if (in_path == NULL || out_path == NULL) return ERR_NULL;

    struct stat s_in, s_out;
    if (stat(in_path, &s_in) != 0) return ERR_FILE;
    if (!S_ISREG(s_in.st_mode)) return ERR_FILE;

    if (stat(out_path, &s_out) != 0) return SUCCESS;

    if (s_in.st_ino == s_out.st_ino && s_in.st_dev == s_out.st_dev) return ERR_SAME_FILE;
    return SUCCESS;
}

static status_t list_load(const char *path, LiverList *list, size_t *error_line) {
    if (path == NULL || list == NULL || error_line == NULL) return ERR_NULL;

    FILE *f = fopen(path, "r");
    if (f == NULL) return ERR_FILE;

    status_t st = SUCCESS;
    size_t line_no = 0;
    int reading = 1;

    while (reading) {
        String line = {0};
        st = read_line(f, &line);
        line_no++;

        if (st == ERR_EOF) {
            st = SUCCESS;
            reading = 0;
        } else if (st != SUCCESS) {
            *error_line = line_no;
            reading = 0;
        } else {
            if (line.length > 0) {
                Liver l = {0};
                st = parse_liver(line.data, &l);
                if (st == SUCCESS) st = list_add_liver(list, &l, NULL);
                liver_clear(&l);

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

static status_t list_dump(const char *path, const LiverList *list) {
    if (path == NULL || list == NULL) return ERR_NULL;

    FILE *f = fopen(path, "w");
    if (f == NULL) return ERR_FILE;

    status_t st = SUCCESS;
    for (const Node *cur = list->head; cur != NULL && st == SUCCESS; cur = cur->next) {
        const Liver *l = &cur->liver;

        if (fprintf(f, "%s %s ", l->surname.data, l->name.data) < 0) st = ERR_FILE;
        if (st == SUCCESS && l->patronymic.length > 0 && fprintf(f, "%s ", l->patronymic.data) < 0) st = ERR_FILE;
        if (st == SUCCESS &&
            fprintf(f, "%02u.%02u.%04u %c %.2f\n", l->birth.day, l->birth.month, l->birth.year,
                    l->gender, l->income) < 0)
            st = ERR_FILE;
    }

    if (fclose(f) != 0 && st == SUCCESS) st = ERR_FILE;
    return st;
}

static void print_liver(size_t number, const Liver *l) {
    printf("%zu. %s %s", number, l->surname.data, l->name.data);
    if (l->patronymic.length > 0) printf(" %s", l->patronymic.data);
    printf(" | %02u.%02u.%04u | %c | доход: %.2f\n",
           l->birth.day, l->birth.month, l->birth.year, l->gender, l->income);
}

static void print_menu(size_t pending) {
    printf("\nМеню жителей\n"
           "1. Показать всех жителей (от молодых к старшим)\n"
           "2. Найти жителей\n"
           "3. Добавить жителя\n"
           "4. Изменить жителя\n"
           "5. Удалить жителя\n"
           "6. Отменить последние N/2 изменений (сейчас N = %zu, будет отменено %zu)\n"
           "7. Выгрузить список в файл\n"
           "0. Выход\n"
           "Выберите действие: ", pending, pending / 2);
    fflush(stdout);
}

static status_t read_name_field(const char *prompt, String *out, int allow_empty) {
    status_t st = read_field(prompt, out);
    if (st != SUCCESS) return st;

    st = validate_name(out, allow_empty);
    if (st == ERR_VALUE) {
        if (allow_empty) printf("Ошибка: только латинские буквы (можно оставить пустым)\n");
        else printf("Ошибка: только латинские буквы, строка не пустая\n");
    }
    return st;
}

static status_t input_field(Liver *l, field_t field) {
    if (l == NULL) return ERR_NULL;

    String s = {0};
    status_t st;
    switch (field) {
        case FIELD_SURNAME:
            string_clear(&l->surname);
            return read_name_field("Фамилия: ", &l->surname, 0);
        case FIELD_NAME:
            string_clear(&l->name);
            return read_name_field("Имя: ", &l->name, 0);
        case FIELD_PATRONYMIC:
            string_clear(&l->patronymic);
            return read_name_field("Отчество (Enter — нет): ", &l->patronymic, 1);
        case FIELD_BIRTH:
            st = read_field("Дата рождения (д.м.гггг): ", &s);
            if (st == SUCCESS) {
                st = parse_date(s.data, &l->birth);
                if (st == SUCCESS) st = validate_birth_date(&l->birth);
                if (st == ERR_VALUE) printf("Ошибка: дата в формате д.м.гггг, существующая и не в будущем (год от %d)\n", (int)MIN_YEAR);
            }
            string_clear(&s);
            return st;
        case FIELD_GENDER:
            st = read_field("Пол (M/W): ", &s);
            if (st == SUCCESS) {
                st = parse_gender(s.data, &l->gender);
                if (st == ERR_VALUE) printf("Ошибка: пол — символ M или W\n");
            }
            string_clear(&s);
            return st;
        case FIELD_INCOME:
            st = read_field("Средний доход за месяц: ", &s);
            if (st == SUCCESS) {
                st = parse_income(s.data, &l->income);
                if (st == ERR_VALUE) printf("Ошибка: доход — неотрицательное число (например 1500.50)\n");
            }
            string_clear(&s);
            return st;
        default:
            return ERR_VALUE;
    }
}

static status_t input_criteria(Criteria *c) {
    if (c == NULL) return ERR_NULL;

    const char *names[] = {"фамилия", "имя", "отчество", "дата рождения", "пол", "доход"};
    printf("Задайте условия поиска\n");
    int selected = 0;

    for (int f = 0; f < FIELD_COUNT; f++) {
        String answer = {0};
        printf("Искать по полю \"%s\"? (1 — да, Enter — нет): ", names[f]);
        fflush(stdout);

        status_t st = read_line(stdin, &answer);
        if (st == SUCCESS) {
            if (answer.length == 1 && answer.data[0] == '1') {
                c->use[f] = 1;
                selected++;
                st = input_field(&c->values, (field_t)f);
            } else if (answer.length != 0) {
                printf("Ошибка: введите 1 или нажмите Enter\n");
                st = ERR_VALUE;
            }
        }
        string_clear(&answer);
        if (st != SUCCESS) return st;
    }

    if (selected == 0) {
        printf("Ошибка: не выбрано ни одного поля\n");
        return ERR_VALUE;
    }
    return SUCCESS;
}

// действия меню

static status_t search_livers(const LiverList *list, double eps, Node **selected) {
    if (list == NULL) return ERR_NULL;

    Criteria c = {0};
    Node **found = NULL;
    size_t n = 0;

    status_t st = input_criteria(&c);
    if (st == SUCCESS) {
        found = malloc(list->count * sizeof(*found));
        if (found == NULL) st = ERR_MEMORY;
    }
    if (st == SUCCESS) st = list_search(list, &c, eps, found, &n);
    if (st == SUCCESS) {
        printf("Найдено жителей: %zu\n", n);
        for (size_t i = 0; i < n; i++) print_liver(i + 1, &found[i]->liver);
    }

    if (st == SUCCESS && selected != NULL) {
        String s = {0};
        unsigned int number = 0;
        st = read_field("Номер жителя из списка выше: ", &s);
        if (st == SUCCESS) {
            st = parse_uint(s.data, &number);
            if (st == SUCCESS && (number < 1 || number > n)) st = ERR_VALUE;
            if (st == ERR_VALUE) printf("Ошибка: введите число от 1 до %zu\n", n);
        }
        string_clear(&s);

        if (st == SUCCESS) *selected = found[number - 1];
    }

    free(found);
    liver_clear(&c.values);
    return st;
}

static status_t action_add(LiverList *list, History *history) {
    if (list == NULL || history == NULL) return ERR_NULL;

    Liver l = {0};
    status_t st = SUCCESS;
    for (int f = 0; f < FIELD_COUNT && st == SUCCESS; f++)
        st = input_field(&l, (field_t)f);
    if (st == SUCCESS) st = registry_add(list, history, &l);

    liver_clear(&l);
    return st;
}

static status_t action_delete(LiverList *list, History *history, double eps) {
    if (list == NULL || history == NULL) return ERR_NULL;

    Node *node = NULL;
    status_t st = search_livers(list, eps, &node);
    if (st == SUCCESS) st = registry_delete(list, history, node);
    return st;
}

static status_t action_edit(LiverList *list, History *history, double eps) {
    if (list == NULL || history == NULL) return ERR_NULL;

    Node *node = NULL;
    status_t st = search_livers(list, eps, &node);
    if (st != SUCCESS) return st;

    String s = {0};
    unsigned int number = 0;
    st = read_field("Что изменить (1 — фамилия, 2 — имя, 3 — отчество, 4 — дата рождения, 5 — пол, 6 — доход): ", &s);
    if (st == SUCCESS) {
        st = parse_uint(s.data, &number);
        if (st == SUCCESS && (number < 1 || number > (unsigned int)FIELD_COUNT)) st = ERR_VALUE;
        if (st == ERR_VALUE) printf("Ошибка: введите число от 1 до %d\n", (int)FIELD_COUNT);
    }
    string_clear(&s);
    if (st != SUCCESS) return st;

    Liver updated = {0};
    st = liver_copy(&node->liver, &updated);
    if (st == SUCCESS) st = input_field(&updated, (field_t)(number - 1));
    if (st == SUCCESS) st = registry_edit(list, history, node, &updated);

    liver_clear(&updated);
    return st;
}

static status_t action_dump(const LiverList *list, const char *in_path) {
    if (list == NULL || in_path == NULL) return ERR_NULL;

    String path = {0};
    status_t st = read_field("Путь к файлу для выгрузки: ", &path);
    if (st == SUCCESS && path.length == 0) {
        printf("Ошибка: путь не должен быть пустым\n");
        st = ERR_VALUE;
    }
    if (st == SUCCESS) st = check_different_files(in_path, path.data);
    if (st == SUCCESS) st = list_dump(path.data, list);

    string_clear(&path);
    return st;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Использование: %s <входной_файл>\n", argv[0]);
        return 1;
    }

    const char *in_path = argv[1];
    const double eps = 1e-6;

    if (in_path[0] == '\0') {
        fprintf(stderr, "Ошибка: путь к файлу не должен быть пустым\n");
        return 1;
    }

    LiverList list = {0};
    History history = {0};
    size_t error_line = 0;

    status_t st = list_load(in_path, &list, &error_line);
    if (st != SUCCESS) {
        switch (st) {
            case ERR_FILE:
                fprintf(stderr, "Ошибка: не удалось прочитать файл '%s'\n", in_path);
                break;
            case ERR_VALUE:
                fprintf(stderr, "Ошибка: неверный формат данных в строке %zu\n", error_line);
                break;
            default:
                fprintf(stderr, "Ошибка чтения файла (строка %zu): %s\n", error_line, status_text(st));
                break;
        }
        list_clear(&list);
        return 1;
    }

    printf("Считано жителей: %zu\n", list.count);

    int running = 1;
    while (running) {
        print_menu(history.count);
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
        if (parse_st != SUCCESS || choice > 7) {
            printf("Неверный пункт меню\n");
            continue;
        }

        if ((choice == 1 || choice == 2 || choice == 4 || choice == 5) && list.count == 0) {
            printf("Список жителей пуст\n");
            continue;
        }

        const char *ok_message = NULL;
        st = SUCCESS;

        switch (choice) {
            case 0:
                running = 0;
                break;
            case 1: {
                size_t number = 1;
                for (const Node *cur = list.head; cur != NULL; cur = cur->next)
                    print_liver(number++, &cur->liver);
                break;
            }
            case 2:
                st = search_livers(&list, eps, NULL);
                break;
            case 3:
                st = action_add(&list, &history);
                ok_message = "Житель добавлен";
                break;
            case 4:
                st = action_edit(&list, &history, eps);
                ok_message = "Житель изменён";
                break;
            case 5:
                st = action_delete(&list, &history, eps);
                ok_message = "Житель удалён";
                break;
            case 6: {
                size_t total = history.count;
                size_t undone = 0;
                st = registry_undo(&list, &history, &undone);
                if (st == SUCCESS && undone == 0)
                    printf("Нечего отменять: N = %zu, N/2 = 0\n", total);
                else if (st == SUCCESS)
                    printf("Отменено изменений: %zu из %zu\n", undone, total);
                break;
            }
            case 7:
                st = action_dump(&list, in_path);
                ok_message = "Список выгружен в файл";
                break;
            default:
                break;
        }

        switch (st) {
            case SUCCESS:
                if (ok_message != NULL) printf("%s\n", ok_message);
                break;
            case ERR_VALUE:
                break;
            case ERR_NOT_FOUND:
                printf("Ничего не найдено\n");
                break;
            case ERR_EOF:
                running = 0;
                break;
            case ERR_SAME_FILE:
                printf("Ошибка: нельзя выгружать данные во входной файл\n");
                break;
            case ERR_FILE:
                printf("Ошибка: не удалось записать файл\n");
                break;
            case ERR_MEMORY:
                printf("Ошибка (%s)\n", status_text(st));
                running = 0;
                break;
            default:
                printf("Ошибка (%s)\n", status_text(st));
                break;
        }
    }

    history_destroy(&history);
    list_clear(&list);
    printf("Программа завершена.\n");
    return 0;
}