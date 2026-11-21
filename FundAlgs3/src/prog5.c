#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <math.h>
#include <sys/stat.h>

// Задание 5

typedef enum {
    SUCCESS = 0,
    ERR_NULL,
    ERR_MEMORY,
    ERR_VALUE,
    ERR_NOT_FOUND,
    ERR_EOF,
    ERR_FILE,
    ERR_SAME_FILE,
    ERR_DUPLICATE_ID
} status_t;

enum {
    GRADES_COUNT = 5,
    MAX_GRADE = 100
};

typedef enum {
    FIELD_ID = 0,
    FIELD_SURNAME,
    FIELD_NAME,
    FIELD_GROUP
} field_t;

typedef int (*comparator_t)(const void *, const void *);

typedef struct {
    char *data;
    int length;
} String;

typedef struct {
    unsigned int id;
    String name;
    String surname;
    String group;
    unsigned char *grades;
} Student;

typedef struct {
    Student *items;
    size_t count;
    size_t capacity;
} StudentList;

// String (из задания 4)

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
        case ERR_SAME_FILE: return "входной и выходной файлы совпадают";
        case ERR_DUPLICATE_ID: return "id должны быть уникальными";
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

static status_t parse_grade(const char *s, unsigned char *out) {
    if (s == NULL || out == NULL) return ERR_NULL;

    unsigned int v = 0;
    status_t st = parse_uint(s, &v);
    if (st != SUCCESS) return st;
    if (v > MAX_GRADE) return ERR_VALUE;

    *out = (unsigned char)v;
    return SUCCESS;
}

static status_t validate_name(const String *s) {
    if (s == NULL || s->data == NULL) return ERR_NULL;
    if (s->length == 0) return ERR_VALUE;

    for (int i = 0; i < s->length; i++) {
        char c = s->data[i];
        if (!((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))) return ERR_VALUE;
    }
    return SUCCESS;
}

static status_t validate_group(const String *s) {
    if (s == NULL || s->data == NULL) return ERR_NULL;
    if (s->length == 0) return ERR_VALUE;

    for (int i = 0; i < s->length; i++) {
        char c = s->data[i];
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') return ERR_VALUE;
    }
    return SUCCESS;
}

static status_t is_greater(double a, double b, double eps, int *result) {
    if (result == NULL) return ERR_NULL;
    if (!isfinite(a) || !isfinite(b)) return ERR_VALUE;
    if (!isfinite(eps) || eps <= 0.0) return ERR_VALUE;

    *result = (a - b > eps);
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

// работа со студентами

static void student_clear(Student *s) {
    if (s == NULL) return;

    string_clear(&s->name);
    string_clear(&s->surname);
    string_clear(&s->group);
    free(s->grades);

    s->grades = NULL;
    s->id = 0;
}

static void students_clear(StudentList *list) {
    if (list == NULL) return;

    for (size_t i = 0; i < list->count; i++) student_clear(&list->items[i]);
    free(list->items);

    list->items = NULL;
    list->count = 0;
    list->capacity = 0;
}

static status_t student_average(const Student *s, double *out) {
    if (s == NULL || out == NULL) return ERR_NULL;
    if (s->grades == NULL) return ERR_VALUE;

    unsigned int sum = 0;
    for (int i = 0; i < GRADES_COUNT; i++) sum += s->grades[i];

    *out = (double)sum / GRADES_COUNT;
    return SUCCESS;
}

// забирает содержимое student себе
static status_t students_add(StudentList *list, Student *student) {
    if (list == NULL || student == NULL) return ERR_NULL;

    if (list->count == list->capacity) {
        if (list->capacity > SIZE_MAX / sizeof(Student) / 2) return ERR_MEMORY;

        size_t new_cap = (list->capacity == 0) ? 4 : list->capacity * 2;
        Student *tmp = realloc(list->items, new_cap * sizeof *tmp);
        if (tmp == NULL) return ERR_MEMORY;
        list->items = tmp;
        list->capacity = new_cap;
    }

    list->items[list->count] = *student;
    *student = (Student){0};
    list->count++;
    return SUCCESS;
}

static status_t parse_student(const char *line, Student *out) {
    if (line == NULL || out == NULL) return ERR_NULL;

    Student s = {0};
    String tok = {0};
    const char *cur = line;

    status_t st = next_token(&cur, &tok);
    if (st == SUCCESS) st = parse_uint(tok.data, &s.id);
    string_clear(&tok);

    if (st == SUCCESS) st = next_token(&cur, &s.name);
    if (st == SUCCESS) st = validate_name(&s.name);

    if (st == SUCCESS) st = next_token(&cur, &s.surname);
    if (st == SUCCESS) st = validate_name(&s.surname);

    if (st == SUCCESS) st = next_token(&cur, &s.group);
    if (st == SUCCESS) st = validate_group(&s.group);

    if (st == SUCCESS) {
        s.grades = malloc(GRADES_COUNT * sizeof *s.grades);
        if (s.grades == NULL) st = ERR_MEMORY;
    }

    for (int i = 0; i < GRADES_COUNT && st == SUCCESS; i++) {
        st = next_token(&cur, &tok);
        if (st == SUCCESS) st = parse_grade(tok.data, &s.grades[i]);
        string_clear(&tok);
    }

    if (st == SUCCESS && has_more_tokens(cur)) st = ERR_VALUE;

    if (st != SUCCESS) {
        student_clear(&s);
        return st;
    }

    *out = s;
    return SUCCESS;
}

// ввод

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

    // выходного файла может ещё не быть — тогда файлы точно разные
    if (stat(out_path, &s_out) != 0) return SUCCESS;

    if (s_in.st_ino == s_out.st_ino && s_in.st_dev == s_out.st_dev) return ERR_SAME_FILE;
    return SUCCESS;
}

static status_t students_id_taken(const StudentList *list, unsigned int id, int *taken) {
    if (list == NULL || taken == NULL) return ERR_NULL;
    *taken = 0;
    for (size_t i = 0; i < list->count; i++)
        if (list->items[i].id == id) { *taken = 1; break; }
    return SUCCESS;
}

static status_t students_load(const char *path, StudentList *list, size_t *error_line) {
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
                Student s = {0};
                st = parse_student(line.data, &s);
                if (st == SUCCESS) {
                    int taken = 0;
                    st = students_id_taken(list, s.id, &taken);
                    if (st == SUCCESS && taken) st = ERR_DUPLICATE_ID;
                }

                if (st == SUCCESS) {
                    st = students_add(list, &s);
                }
                student_clear(&s);

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

// поиск
static status_t students_find(const StudentList *list, field_t field, const String *key, const Student **found, size_t *out_count) {
    if (list == NULL || key == NULL || key->data == NULL ||
        found == NULL || out_count == NULL) return ERR_NULL;

    unsigned int id = 0;
    if (field == FIELD_ID) {
        status_t st = parse_uint(key->data, &id);
        if (st != SUCCESS) return st;
    } 
    else if (field != FIELD_SURNAME && field != FIELD_NAME && field != FIELD_GROUP) {
        return ERR_VALUE;
    }

    size_t k = 0;
    for (size_t i = 0; i < list->count; i++) {
        const Student *s = &list->items[i];
        int match = 0;

        if (field == FIELD_ID) {
            match = (s->id == id);
        } else {
            const String *value = (field == FIELD_SURNAME) ? &s->surname : (field == FIELD_NAME) ? &s->name : &s->group;
            status_t st = string_equal(value, key, &match);
            if (st != SUCCESS) return st;
        }
        if (match) found[k++] = s;
    }

    *out_count = k;
    return (k == 0) ? ERR_NOT_FOUND : SUCCESS;
}

// компараторы для qsort

static int compare_by_id(const void *a, const void *b) {
    const Student *x = a;
    const Student *y = b;

    if (x->id != y->id) return (x->id > y->id) ? 1 : -1;
    return 0;
}

static int compare_by_surname(const void *a, const void *b) {
    const Student *x = a;
    const Student *y = b;

    int r = strcmp(x->surname.data, y->surname.data);
    if (r != 0) return r;
    return compare_by_id(a, b);
}

static int compare_by_name(const void *a, const void *b) {
    const Student *x = a;
    const Student *y = b;

    int r = strcmp(x->name.data, y->name.data);
    if (r != 0) return r;
    return compare_by_id(a, b);
}

static int compare_by_group(const void *a, const void *b) {
    const Student *x = a;
    const Student *y = b;

    int r = strcmp(x->group.data, y->group.data);
    if (r != 0) return r;
    return compare_by_id(a, b);
}

// компаратор выбирается по полю и передаётся в qsort
static status_t students_sort(StudentList *list, field_t field) {
    if (list == NULL) return ERR_NULL;

    comparator_t cmp;
    switch (field) {
        case FIELD_ID:      cmp = compare_by_id;      break;
        case FIELD_SURNAME: cmp = compare_by_surname; break;
        case FIELD_NAME:    cmp = compare_by_name;    break;
        case FIELD_GROUP:   cmp = compare_by_group;   break;
        default: return ERR_VALUE;
    }

    if (list->count > 1) qsort(list->items, list->count, sizeof(*list->items), cmp);
    return SUCCESS;
}

// студенты со средним баллом выше общего среднего балла по всем студентам
static status_t students_above_average(const StudentList *list, double eps, const Student **found, size_t *out_count, double *out_average) {
    if (list == NULL || found == NULL || out_count == NULL || out_average == NULL) return ERR_NULL;
    if (list->count == 0) return ERR_NOT_FOUND;

    double sum = 0.0;
    for (size_t i = 0; i < list->count; i++) {
        double avg = 0.0;
        status_t st = student_average(&list->items[i], &avg);
        if (st != SUCCESS) return st;
        sum += avg;
    }
    double total_avg = sum / (double)list->count;

    size_t k = 0;

    for (size_t i = 0; i < list->count; i++) {
        double avg = 0.0;
        status_t st = student_average(&list->items[i], &avg);
        if (st != SUCCESS) return st;

        int greater = 0;
        st = is_greater(avg, total_avg, eps, &greater);
        if (st != SUCCESS) return st;
        if (greater) found[k++] = &list->items[i];
    }

    *out_count = k;
    *out_average = total_avg;
    return SUCCESS;
}

// запись в трассировочный файл

static status_t export_student_info(FILE *f, unsigned int id, const Student **list, size_t count) {
    if (f == NULL || list == NULL) return ERR_NULL;

    if (fprintf(f, "Студенты с id %u:\n", id) < 0) return ERR_FILE;

    for (size_t i = 0; i < count; i++) {
        double avg = 0.0;
        status_t st = student_average(list[i], &avg);
        if (st != SUCCESS) return st;
        if (fprintf(f, "%s %s, группа %s, средний балл: %.2f\n",
                    list[i]->surname.data, list[i]->name.data, list[i]->group.data, avg) < 0)
            return ERR_FILE;
    }

    if (fflush(f) != 0) return ERR_FILE;
    return SUCCESS;
}

static status_t export_above_average(FILE *f, const Student **list, size_t count, double average) {
    if (f == NULL || list == NULL) return ERR_NULL;

    if (fprintf(f, "Студенты со средним баллом выше общего среднего (%.2f):\n", average) < 0) return ERR_FILE;
    if (count == 0 && fprintf(f, "(таких студентов нет)\n") < 0) return ERR_FILE;

    for (size_t i = 0; i < count; i++) {
        if (fprintf(f, "%s %s\n", list[i]->surname.data, list[i]->name.data) < 0) return ERR_FILE;
    }

    if (fflush(f) != 0) return ERR_FILE;
    return SUCCESS;
}

// вывод на экран

static void print_student(const Student *s) {
    double avg = 0.0;
    if (student_average(s, &avg) != SUCCESS) avg = 0.0;

    printf("ID: %u | %s %s | группа %s | оценки:", s->id, s->surname.data, s->name.data, s->group.data);
    for (int i = 0; i < GRADES_COUNT; i++) printf(" %u", (unsigned int)s->grades[i]);
    printf(" | средний балл: %.2f\n", avg);
}

static void print_menu(void) {
    printf("\nМеню студентов\n"
           "1. Показать всех студентов\n"
           "2. Найти по id\n"
           "3. Найти по фамилии\n"
           "4. Найти по имени\n"
           "5. Найти по группе\n"
           "6. Сортировать по id\n"
           "7. Сортировать по фамилии\n"
           "8. Сортировать по имени\n"
           "9. Сортировать по группе\n"
           "10. Записать в файл данные студента по id\n"
           "11. Записать в файл студентов с баллом выше среднего\n"
           "0. Выход\n"
           "Выберите действие: ");
    fflush(stdout);
}

// ввод ключа поиска с валидацией
static status_t input_search_key(field_t field, String *key) {
    if (key == NULL) return ERR_NULL;

    const char *prompt;
    switch (field) {
        case FIELD_ID: prompt = "Введите id: ";      break;
        case FIELD_SURNAME: prompt = "Введите фамилию: "; break;
        case FIELD_NAME: prompt = "Введите имя: ";     break;
        case FIELD_GROUP: prompt = "Введите группу: ";  break;
        default: return ERR_VALUE;
    }

    status_t st = read_field(prompt, key);
    if (st != SUCCESS) return st;

    switch (field) {
        case FIELD_ID: {
            unsigned int id = 0;
            st = parse_uint(key->data, &id);
            if (st == ERR_VALUE) printf("Ошибка: id — целое неотрицательное число\n");
            break;
        }
        case FIELD_GROUP:
            st = validate_group(key);
            if (st == ERR_VALUE) printf("Ошибка: группа — непустая строка без пробелов\n");
            break;
        default:
            st = validate_name(key);
            if (st == ERR_VALUE) printf("Ошибка: только латинские буквы, строка не пустая\n");
            break;
    }
    return st;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Использование: %s <входной_файл> <выходной_файл>\n", argv[0]);
        return 1;
    }

    const char *in_path = argv[1];
    const char *out_path = argv[2];
    const double eps = 1e-9;

    if (in_path[0] == '\0' || out_path[0] == '\0') {
        fprintf(stderr, "Ошибка: пути к файлам не должны быть пустыми\n");
        return 1;
    }

    status_t st = check_different_files(in_path, out_path);
    if (st != SUCCESS) {
        if (st == ERR_SAME_FILE)
            fprintf(stderr, "Ошибка: входной и выходной файлы совпадают\n");
        else
            fprintf(stderr, "Ошибка: не удаётся открыть входной файл '%s' (%s)\n", in_path, status_text(st));
        return 1;
    }

    StudentList students = {0};
    size_t error_line = 0;
    st = students_load(in_path, &students, &error_line);
    if (st != SUCCESS) {
        switch (st) {
            case ERR_FILE:
                fprintf(stderr, "Ошибка: не удалось прочитать файл '%s'\n", in_path);
                break;
            case ERR_VALUE:
                fprintf(stderr, "Ошибка: неверный формат данных в строке %zu\n", error_line);
                break;
            default:
                fprintf(stderr, "Ошибка чтения файла (строка %zu): %s\n",
                        error_line, status_text(st));
                break;
        }
        students_clear(&students);
        return 1;
    }

    FILE *trace = fopen(out_path, "w");
    if (trace == NULL) {
        fprintf(stderr, "Ошибка: не удалось создать выходной файл '%s'\n", out_path);
        students_clear(&students);
        return 1;
    }

    const Student **found = malloc((students.count ? students.count : 1) * sizeof *found);
    if (found == NULL) {
        fprintf(stderr, "Ошибка: %s\n", status_text(ERR_MEMORY));
        fclose(trace);
        students_clear(&students);
        return 1;
    }

    printf("Считано студентов: %zu\n", students.count);

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
        if (parse_st != SUCCESS || choice > 11) {
            printf("Неверный пункт меню\n");
            continue;
        }

        if (choice != 0 && students.count == 0) {
            printf("Список студентов пуст\n");
            continue;
        }

        switch (choice) {
            case 0:
                running = 0;
                break;

            case 1:
                for (size_t i = 0; i < students.count; i++) print_student(&students.items[i]);
                break;

            case 2: case 3: case 4: case 5: {
                field_t field = (field_t)(choice - 2);
                String key = {0};
                size_t n = 0;

                st = input_search_key(field, &key);
                if (st == SUCCESS) st = students_find(&students, field, &key, found, &n);
                if (st == SUCCESS) {
                    printf("Найдено студентов: %zu\n", n);
                    for (size_t i = 0; i < n; i++) print_student(found[i]);
                }
                string_clear(&key);

                switch (st) {
                    case SUCCESS: break;
                    case ERR_NOT_FOUND: printf("Студенты не найдены\n"); break;
                    case ERR_VALUE: break;
                    case ERR_EOF: running = 0; break;
                    default:
                        printf("Ошибка (%s)\n", status_text(st));
                        if (st == ERR_MEMORY) running = 0;
                        break;
                }
                break;
            }

            case 6: case 7: case 8: case 9: {
                st = students_sort(&students, (field_t)(choice - 6));
                if (st == SUCCESS) printf("Список отсортирован (посмотреть — пункт 1)\n");
                else printf("Ошибка сортировки (%s)\n", status_text(st));
                break;
            }

            case 10: {
                String key = {0};
                size_t n = 0;
                unsigned int id = 0;

                st = input_search_key(FIELD_ID, &key);
                if (st == SUCCESS) st = students_find(&students, FIELD_ID, &key, found, &n);
                if (st == SUCCESS) {
                    st = parse_uint(key.data, &id);
                    if (st == SUCCESS) st = export_student_info(trace, id, found, n);
                }
                string_clear(&key);

                switch (st) {
                    case SUCCESS:
                        printf("Данные студента записаны в файл\n");
                        break;
                    case ERR_NOT_FOUND: printf("Студент с таким id не найден\n"); break;
                    case ERR_VALUE: break;
                    case ERR_EOF: running = 0; break;
                    default:
                        printf("Ошибка (%s)\n", status_text(st));
                        if (st == ERR_MEMORY) running = 0;
                        break;
                }
                break;
            }

            case 11: {
                size_t n = 0;
                double average = 0.0;

                st = students_above_average(&students, eps, found, &n, &average);
                if (st == SUCCESS) st = export_above_average(trace, found, n, average);

                switch (st) {
                    case SUCCESS:
                        printf("Записано студентов: %zu\n", n);
                        break;
                    case ERR_NOT_FOUND:
                        printf("Список студентов пуст\n");
                        break;
                    default:
                        printf("Ошибка (%s)\n", status_text(st));
                        if (st == ERR_MEMORY) running = 0;
                        break;
                }
                break;
            }

            default:
                break;
        }
    }

    free(found);
    if (fclose(trace) != 0)
        fprintf(stderr, "Предупреждение: ошибка закрытия файла трассировки\n");
    students_clear(&students);
    printf("Программа завершена.\n");
    return 0;
}