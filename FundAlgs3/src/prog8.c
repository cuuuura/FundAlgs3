#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

// 8

typedef enum {
    SUCCESS = 0,
    ERR_NULL,
    ERR_MEMORY,
    ERR_VALUE,
    ERR_NOT_FOUND,
    ERR_FILE,
    ERR_OVERFLOW,
    ERR_DIVIDE
} status_t;

typedef enum {
    OP_ADD = 0,
    OP_SUB,
    OP_MULT,
    OP_DIV,
    OP_MOD,
    OP_EVAL,
    OP_DIFF,
    OP_CMPS
} op_t;

typedef struct {
    char *data;
    int length;
} String;

typedef struct Term {
    long long coeff;
    unsigned int exp;
    struct Term *next;
} Term;

typedef struct {
    Term *head;
} Polynomial;

typedef struct {
    op_t op;
    String first;
    String second;
    int arg_count;
} Instruction;

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

static const char *status_text(status_t st) {
    switch (st) {
        case SUCCESS: return "успешно";
        case ERR_NULL: return "пустой указатель";
        case ERR_MEMORY: return "не хватает памяти";
        case ERR_VALUE: return "некорректное значение";
        case ERR_NOT_FOUND: return "не найдено";
        case ERR_FILE: return "ошибка работы с файлом";
        case ERR_OVERFLOW: return "переполнение";
        case ERR_DIVIDE: return "деление невозможно";
        default: return "неизвестная ошибка";
    }
}

// арифметика

static status_t checked_add(long long a, long long b, long long *out) {
    if (out == NULL) return ERR_NULL;
    if (b > 0 && a > LLONG_MAX - b) return ERR_OVERFLOW;
    if (b < 0 && a < -LLONG_MAX - b) return ERR_OVERFLOW;

    *out = a + b;
    return SUCCESS;
}

static status_t checked_mul(long long a, long long b, long long *out) {
    if (out == NULL) return ERR_NULL;
    if (a == 0 || b == 0) {
        *out = 0;
        return SUCCESS;
    }

    long long abs_a = (a < 0) ? -a : a;
    long long abs_b = (b < 0) ? -b : b;
    if (abs_a > LLONG_MAX / abs_b) return ERR_OVERFLOW;

    *out = a * b;
    return SUCCESS;
}

static status_t power_checked(long long base, unsigned int exp, long long *out) {
    if (out == NULL) return ERR_NULL;

    long long result = 1;
    while (exp > 0) {
        if (exp & 1u) {
            status_t st = checked_mul(result, base, &result);
            if (st != SUCCESS) return st;
        }
        exp >>= 1;
        if (exp > 0) {
            status_t st = checked_mul(base, base, &base);
            if (st != SUCCESS) return st;
        }
    }

    *out = result;
    return SUCCESS;
}

// многочлены

static void poly_clear(Polynomial *p) {
    if (p == NULL) return;

    Term *cur = p->head;
    while (cur != NULL) {
        Term *next = cur->next;
        free(cur);
        cur = next;
    }
    p->head = NULL;
}

static status_t poly_add_term(Polynomial *p, long long coeff, unsigned int exp) {
    if (p == NULL) return ERR_NULL;
    if (coeff == 0) return SUCCESS;

    Term *prev = NULL;
    Term *cur = p->head;
    while (cur != NULL && cur->exp > exp) {
        prev = cur;
        cur = cur->next;
    }

    if (cur != NULL && cur->exp == exp) {
        long long sum = 0;
        status_t st = checked_add(cur->coeff, coeff, &sum);
        if (st != SUCCESS) return st;

        if (sum != 0) {
            cur->coeff = sum;
        } else {
            if (prev == NULL) p->head = cur->next;
            else prev->next = cur->next;
            free(cur);
        }
        return SUCCESS;
    }

    Term *node = malloc(sizeof *node);
    if (node == NULL) return ERR_MEMORY;

    node->coeff = coeff;
    node->exp = exp;
    node->next = cur;
    if (prev == NULL) p->head = node;
    else prev->next = node;
    return SUCCESS;
}

static status_t poly_add_scaled(Polynomial *dst, const Polynomial *src, long long factor, unsigned int shift) {
    if (dst == NULL || src == NULL) return ERR_NULL;

    for (const Term *t = src->head; t != NULL; t = t->next) {
        long long coeff = 0;
        unsigned int exp = 0;

        status_t st = checked_mul(t->coeff, factor, &coeff);
        if (st == SUCCESS) {
            if (t->exp > UINT_MAX - shift) st = ERR_OVERFLOW;
            else exp = t->exp + shift;
        }
        if (st == SUCCESS) st = poly_add_term(dst, coeff, exp);
        if (st != SUCCESS) return st;
    }
    return SUCCESS;
}

static status_t poly_add_sub(const Polynomial *a, const Polynomial *b, long long factor, Polynomial *out) {
    if (a == NULL || b == NULL || out == NULL) return ERR_NULL;
    if (out->head != NULL) return ERR_VALUE;

    Polynomial result = {NULL};
    status_t st = poly_add_scaled(&result, a, 1, 0);
    if (st == SUCCESS) st = poly_add_scaled(&result, b, factor, 0);

    if (st != SUCCESS) {
        poly_clear(&result);
        return st;
    }
    *out = result;
    return SUCCESS;
}

static status_t poly_mult(const Polynomial *a, const Polynomial *b, Polynomial *out) {
    if (a == NULL || b == NULL || out == NULL) return ERR_NULL;
    if (out->head != NULL) return ERR_VALUE;

    Polynomial result = {NULL};
    status_t st = SUCCESS;
    for (const Term *t = a->head; t != NULL && st == SUCCESS; t = t->next)
        st = poly_add_scaled(&result, b, t->coeff, t->exp);

    if (st != SUCCESS) {
        poly_clear(&result);
        return st;
    }
    *out = result;
    return SUCCESS;
}

static status_t poly_divide(const Polynomial *a, const Polynomial *b, Polynomial *quotient, Polynomial *remainder) {
    if (a == NULL || b == NULL || quotient == NULL || remainder == NULL) return ERR_NULL;
    if (quotient->head != NULL || remainder->head != NULL) return ERR_VALUE;
    if (b->head == NULL) return ERR_DIVIDE;

    Polynomial q = {NULL};
    Polynomial r = {NULL};
    status_t st = poly_add_scaled(&r, a, 1, 0);

    while (st == SUCCESS && r.head != NULL && r.head->exp >= b->head->exp) {
        if (r.head->coeff % b->head->coeff != 0) {
            st = ERR_DIVIDE;
        } else {
            long long q_coeff = r.head->coeff / b->head->coeff;
            unsigned int q_exp = r.head->exp - b->head->exp;

            st = poly_add_term(&q, q_coeff, q_exp);
            if (st == SUCCESS) st = poly_add_scaled(&r, b, -q_coeff, q_exp);
        }
    }

    if (st != SUCCESS) {
        poly_clear(&q);
        poly_clear(&r);
        return st;
    }
    *quotient = q;
    *remainder = r;
    return SUCCESS;
}

static status_t poly_derivative(const Polynomial *a, Polynomial *out) {
    if (a == NULL || out == NULL) return ERR_NULL;
    if (out->head != NULL) return ERR_VALUE;

    Polynomial result = {NULL};
    status_t st = SUCCESS;
    for (const Term *t = a->head; t != NULL && st == SUCCESS; t = t->next) {
        if (t->exp > 0) {
            long long coeff = 0;
            st = checked_mul(t->coeff, (long long)t->exp, &coeff);
            if (st == SUCCESS) st = poly_add_term(&result, coeff, t->exp - 1);
        }
    }

    if (st != SUCCESS) {
        poly_clear(&result);
        return st;
    }
    *out = result;
    return SUCCESS;
}

static status_t poly_evaluate(const Polynomial *p, long long x, long long *out) {
    if (p == NULL || out == NULL) return ERR_NULL;

    long long sum = 0;
    for (const Term *t = p->head; t != NULL; t = t->next) {
        long long power = 0;
        long long term = 0;

        status_t st = power_checked(x, t->exp, &power);
        if (st == SUCCESS) st = checked_mul(t->coeff, power, &term);
        if (st == SUCCESS) st = checked_add(sum, term, &sum);
        if (st != SUCCESS) return st;
    }

    *out = sum;
    return SUCCESS;
}

static status_t poly_power(const Polynomial *base, unsigned int exp, Polynomial *out) {
    if (base == NULL || out == NULL) return ERR_NULL;
    if (out->head != NULL) return ERR_VALUE;

    Polynomial result = {NULL};
    Polynomial square = {NULL};
    status_t st = poly_add_term(&result, 1, 0);
    if (st == SUCCESS) st = poly_add_scaled(&square, base, 1, 0);

    while (st == SUCCESS && exp > 0) {
        Polynomial tmp = {NULL};

        if (exp & 1u) {
            st = poly_mult(&result, &square, &tmp);
            if (st == SUCCESS) {
                poly_clear(&result);
                result = tmp;
            }
        }
        exp >>= 1;
        if (st == SUCCESS && exp > 0) {
            Polynomial tmp2 = {NULL};
            st = poly_mult(&square, &square, &tmp2);
            if (st == SUCCESS) {
                poly_clear(&square);
                square = tmp2;
            }
        }
    }

    poly_clear(&square);
    if (st != SUCCESS) {
        poly_clear(&result);
        return st;
    }
    *out = result;
    return SUCCESS;
}

static status_t poly_compose(const Polynomial *f, const Polynomial *g, Polynomial *out) {
    if (f == NULL || g == NULL || out == NULL) return ERR_NULL;
    if (out->head != NULL) return ERR_VALUE;

    Polynomial result = {NULL};
    status_t st = SUCCESS;
    for (const Term *t = f->head; t != NULL && st == SUCCESS; t = t->next) {
        Polynomial power = {NULL};

        st = poly_power(g, t->exp, &power);
        if (st == SUCCESS) st = poly_add_scaled(&result, &power, t->coeff, 0);
        poly_clear(&power);
    }

    if (st != SUCCESS) {
        poly_clear(&result);
        return st;
    }
    *out = result;
    return SUCCESS;
}

// разбор текста

static status_t parse_digits(const char **p, unsigned long long limit, unsigned long long *out) {
    if (p == NULL || *p == NULL || out == NULL) return ERR_NULL;

    const char *s = *p;
    if (*s < '0' || *s > '9') return ERR_NOT_FOUND;

    unsigned long long v = 0;
    while (*s >= '0' && *s <= '9') {
        unsigned long long digit = (unsigned long long)(*s - '0');
        if (v > (limit - digit) / 10ULL) return ERR_VALUE;
        v = v * 10ULL + digit;
        s++;
    }

    *p = s;
    *out = v;
    return SUCCESS;
}

static status_t parse_integer(const char *s, long long *out) {
    if (s == NULL || out == NULL) return ERR_NULL;

    const char *p = s;
    int negative = 0;
    if (*p == '+' || *p == '-') {
        negative = (*p == '-');
        p++;
    }

    unsigned long long v = 0;
    status_t st = parse_digits(&p, (unsigned long long)LLONG_MAX, &v);
    if (st == ERR_NOT_FOUND) st = ERR_VALUE;
    if (st != SUCCESS) return st;
    if (*p != '\0') return ERR_VALUE;

    *out = negative ? -(long long)v : (long long)v;
    return SUCCESS;
}

static status_t poly_parse(const char *s, Polynomial *out) {
    if (s == NULL || out == NULL) return ERR_NULL;
    if (out->head != NULL) return ERR_VALUE;
    if (*s == '\0') return ERR_VALUE;

    Polynomial result = {NULL};
    const char *p = s;
    status_t st = SUCCESS;
    int first_term = 1;

    while (*p != '\0' && st == SUCCESS) {
        int negative = 0;
        if (*p == '+' || *p == '-') {
            negative = (*p == '-');
            p++;
        } else if (!first_term) {
            st = ERR_VALUE;
        }
        first_term = 0;

        unsigned long long coeff = 1;
        unsigned long long exp = 0;
        int has_digits = 0;

        if (st == SUCCESS) {
            st = parse_digits(&p, (unsigned long long)LLONG_MAX, &coeff);
            if (st == SUCCESS) {
                has_digits = 1;
            } else if (st == ERR_NOT_FOUND) {
                coeff = 1;
                st = SUCCESS;
            }
        }

        if (st == SUCCESS && *p == 'x') {
            p++;
            exp = 1;
            if (*p == '^') {
                p++;
                st = parse_digits(&p, (unsigned long long)UINT_MAX, &exp);
                if (st == ERR_NOT_FOUND) st = ERR_VALUE;
            }
        } else if (st == SUCCESS && !has_digits) {
            st = ERR_VALUE;
        }

        if (st == SUCCESS) {
            long long c = (long long)coeff;
            if (negative) c = -c;
            st = poly_add_term(&result, c, (unsigned int)exp);
        }
    }

    if (st != SUCCESS) {
        poly_clear(&result);
        return st;
    }
    *out = result;
    return SUCCESS;
}

static status_t skip_blank(const char **cursor) {
    if (cursor == NULL || *cursor == NULL) return ERR_NULL;

    const char *p = *cursor;
    while (*p != '\0') {
        if (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n') {
            p++;
        } else if (*p == '%') {
            while (*p != '\0' && *p != '\n') p++;
        } else if (*p == '[') {
            const char *start = p;
            p++;
            while (*p != '\0' && *p != ']') {
                if (*p == '[') {
                    *cursor = start;
                    return ERR_VALUE;
                }
                p++;
            }
            if (*p == '\0') {
                *cursor = start;
                return ERR_VALUE;
            }
            p++;
        } else {
            break;
        }
    }

    *cursor = p;
    return SUCCESS;
}

static int name_is(const char *name, size_t len, const char *word) {
    return strlen(word) == len && strncmp(name, word, len) == 0;
}

static status_t find_op(const char *name, size_t len, op_t *op) {
    if (name == NULL || op == NULL) return ERR_NULL;

    if (name_is(name, len, "Add")) *op = OP_ADD;
    else if (name_is(name, len, "Sub")) *op = OP_SUB;
    else if (name_is(name, len, "Mult")) *op = OP_MULT;
    else if (name_is(name, len, "Div")) *op = OP_DIV;
    else if (name_is(name, len, "Mod")) *op = OP_MOD;
    else if (name_is(name, len, "Eval")) *op = OP_EVAL;
    else if (name_is(name, len, "Diff")) *op = OP_DIFF;
    else if (name_is(name, len, "Cmps")) *op = OP_CMPS;
    else return ERR_VALUE;

    return SUCCESS;
}

static status_t make_arg(const char *start, const char *end, String *out) {
    while (start < end && (*start == ' ' || *start == '\t' || *start == '\r' || *start == '\n')) start++;
    while (end > start && (end[-1] == ' ' || end[-1] == '\t' || end[-1] == '\r' || end[-1] == '\n')) end--;
    if (start == end) return ERR_NOT_FOUND;

    return string_from_range(start, (size_t)(end - start), out);
}

static void instruction_clear(Instruction *ins) {
    if (ins == NULL) return;

    string_clear(&ins->first);
    string_clear(&ins->second);
    ins->arg_count = 0;
}

static status_t parse_instruction(const char **cursor, Instruction *ins) {
    if (cursor == NULL || *cursor == NULL || ins == NULL) return ERR_NULL;

    const char *p = *cursor;
    const char *name = p;
    while ((*p >= 'A' && *p <= 'Z') || (*p >= 'a' && *p <= 'z')) p++;

    Instruction tmp = {0};
    status_t st = find_op(name, (size_t)(p - name), &tmp.op);
    if (st != SUCCESS) return st;
    if (*p != '(') return ERR_VALUE;
    p++;

    const char *args_start = p;
    while (*p != '\0' && *p != ')') p++;
    if (*p != ')') return ERR_VALUE;
    const char *args_end = p;
    p++;

    const char *comma = memchr(args_start, ',', (size_t)(args_end - args_start));
    if (comma == NULL) {
        st = make_arg(args_start, args_end, &tmp.first);
        if (st == SUCCESS) {
            tmp.arg_count = 1;
        } else if (st == ERR_NOT_FOUND) {
            tmp.arg_count = 0;
            st = SUCCESS;
        }
    } else if (memchr(comma + 1, ',', (size_t)(args_end - comma - 1)) != NULL) {
        st = ERR_VALUE;
    } else {
        st = make_arg(args_start, comma, &tmp.first);
        if (st == SUCCESS) st = make_arg(comma + 1, args_end, &tmp.second);
        if (st == ERR_NOT_FOUND) st = ERR_VALUE;
        tmp.arg_count = 2;
    }

    if (st == SUCCESS) st = skip_blank(&p);
    if (st == SUCCESS && *p != ';') st = ERR_VALUE;
    if (st != SUCCESS) {
        instruction_clear(&tmp);
        return st;
    }

    *cursor = p + 1;
    *ins = tmp;
    return SUCCESS;
}

// выполнение

static status_t execute_poly(const Instruction *ins, const Polynomial *acc, Polynomial *result) {
    if (ins == NULL || acc == NULL || result == NULL) return ERR_NULL;

    Polynomial first = {NULL};
    Polynomial second = {NULL};
    Polynomial other = {NULL};
    const Polynomial *a = NULL;
    const Polynomial *b = NULL;
    status_t st = SUCCESS;

    if (ins->op == OP_DIFF) {
        if (ins->arg_count == 0) {
            a = acc;
        } else if (ins->arg_count == 1) {
            st = poly_parse(ins->first.data, &first);
            a = &first;
        } else {
            st = ERR_VALUE;
        }
    } else if (ins->arg_count == 1) {
        a = acc;
        st = poly_parse(ins->first.data, &second);
        b = &second;
    } else if (ins->arg_count == 2) {
        st = poly_parse(ins->first.data, &first);
        if (st == SUCCESS) st = poly_parse(ins->second.data, &second);
        a = &first;
        b = &second;
    } else {
        st = ERR_VALUE;
    }

    if (st == SUCCESS) {
        switch (ins->op) {
            case OP_ADD: st = poly_add_sub(a, b, 1, result); break;
            case OP_SUB: st = poly_add_sub(a, b, -1, result); break;
            case OP_MULT: st = poly_mult(a, b, result); break;
            case OP_DIV: st = poly_divide(a, b, result, &other); break;
            case OP_MOD: st = poly_divide(a, b, &other, result); break;
            case OP_DIFF: st = poly_derivative(a, result); break;
            case OP_CMPS: st = poly_compose(a, b, result); break;
            default: st = ERR_VALUE; break;
        }
    }

    poly_clear(&first);
    poly_clear(&second);
    poly_clear(&other);
    return st;
}

static status_t execute_eval(const Instruction *ins, const Polynomial *acc, long long *value) {
    if (ins == NULL || acc == NULL || value == NULL) return ERR_NULL;

    Polynomial first = {NULL};
    const Polynomial *a = NULL;
    long long point = 0;
    status_t st = SUCCESS;

    if (ins->arg_count == 1) {
        a = acc;
        st = parse_integer(ins->first.data, &point);
    } else if (ins->arg_count == 2) {
        st = poly_parse(ins->first.data, &first);
        if (st == SUCCESS) st = parse_integer(ins->second.data, &point);
        a = &first;
    } else {
        st = ERR_VALUE;
    }

    if (st == SUCCESS) st = poly_evaluate(a, point, value);

    poly_clear(&first);
    return st;
}

// ввод вывод

static status_t read_file(const char *path, String *out) {
    if (path == NULL || out == NULL) return ERR_NULL;
    if (out->data != NULL) return ERR_VALUE;

    FILE *f = fopen(path, "r");
    if (f == NULL) return ERR_FILE;

    size_t cap = 256, len = 0;
    char *buf = malloc(cap);
    if (buf == NULL) {
        fclose(f);
        return ERR_MEMORY;
    }

    status_t st = SUCCESS;
    int c;
    while (st == SUCCESS && (c = fgetc(f)) != EOF) {
        if (c == '\0') {
            st = ERR_VALUE;
        } else if (len + 1 >= cap) {
            if (cap > (size_t)INT_MAX / 2) {
                st = ERR_VALUE;
            } else {
                char *nb = realloc(buf, cap * 2);
                if (nb == NULL) {
                    st = ERR_MEMORY;
                } else {
                    buf = nb;
                    cap *= 2;
                }
            }
        }
        if (st == SUCCESS) buf[len++] = (char)c;
    }
    if (st == SUCCESS && ferror(f)) st = ERR_FILE;
    fclose(f);

    if (st != SUCCESS) {
        free(buf);
        return st;
    }

    buf[len] = '\0';
    out->data = buf;
    out->length = (int)len;
    return SUCCESS;
}

static size_t line_number(const char *text, const char *pos) {
    size_t line = 1;
    for (const char *p = text; p < pos; p++)
        if (*p == '\n') line++;
    return line;
}

static const char *op_name(op_t op) {
    switch (op) {
        case OP_ADD: return "Add";
        case OP_SUB: return "Sub";
        case OP_MULT: return "Mult";
        case OP_DIV: return "Div";
        case OP_MOD: return "Mod";
        case OP_EVAL: return "Eval";
        case OP_DIFF: return "Diff";
        case OP_CMPS: return "Cmps";
        default: return "?";
    }
}

static void poly_print(const Polynomial *p) {
    if (p->head == NULL) {
        printf("0");
        return;
    }

    for (const Term *t = p->head; t != NULL; t = t->next) {
        long long c = t->coeff;
        if (c < 0) {
            printf("-");
            c = -c;
        } else if (t != p->head) {
            printf("+");
        }

        if (c != 1 || t->exp == 0) printf("%lld", c);
        if (t->exp > 0) printf("x");
        if (t->exp > 1) printf("^%u", t->exp);
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Использование: %s <файл_с_инструкциями>\n", argv[0]);
        return 1;
    }
    if (argv[1][0] == '\0') {
        fprintf(stderr, "Ошибка: путь к файлу не должен быть пустым\n");
        return 1;
    }

    String text = {0};
    status_t st = read_file(argv[1], &text);
    if (st == ERR_FILE) {
        fprintf(stderr, "Ошибка: не удалось прочитать файл '%s'\n", argv[1]);
        return 1;
    } else if (st != SUCCESS) {
        fprintf(stderr, "Ошибка чтения файла (%s)\n", status_text(st));
        return 1;
    }

    Polynomial acc = {NULL};
    const char *cursor = text.data;
    size_t line = 1;
    int running = 1;

    while (running && st == SUCCESS) {
        st = skip_blank(&cursor);
        line = line_number(text.data, cursor);

        if (st == SUCCESS && *cursor == '\0') {
            running = 0;
        } else if (st == SUCCESS) {
            Instruction ins = {0};
            st = parse_instruction(&cursor, &ins);

            if (st == SUCCESS && ins.op == OP_EVAL) {
                long long value = 0;
                st = execute_eval(&ins, &acc, &value);
                if (st == SUCCESS) printf("Eval: %lld\n", value);
            } else if (st == SUCCESS) {
                Polynomial result = {NULL};
                st = execute_poly(&ins, &acc, &result);
                if (st == SUCCESS) {
                    poly_clear(&acc);
                    acc = result;
                    printf("%s: ", op_name(ins.op));
                    poly_print(&acc);
                    printf("\n");
                }
            }
            instruction_clear(&ins);
        }
    }

    switch (st) {
        case SUCCESS:
            break;
        case ERR_VALUE:
            fprintf(stderr, "Ошибка в строке %zu: неверный синтаксис\n", line);
            break;
        case ERR_OVERFLOW:
            fprintf(stderr, "Ошибка в строке %zu: переполнение коэффициентов\n", line);
            break;
        case ERR_DIVIDE:
            fprintf(stderr, "Ошибка в строке %zu: деление невозможно (делитель равен нулю или коэффициенты не делятся нацело)\n", line);
            break;
        default:
            fprintf(stderr, "Ошибка в строке %zu: %s\n", line, status_text(st));
            break;
    }

    poly_clear(&acc);
    string_clear(&text);
    return (st == SUCCESS) ? 0 : 1;
}