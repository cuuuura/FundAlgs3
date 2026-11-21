#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

// 10

#define MAX_DEPTH 10000

typedef enum {
    SUCCESS = 0,
    ERR_NULL,
    ERR_MEMORY,
    ERR_VALUE,
    ERR_DEPTH,
    ERR_EMPTY,
    ERR_EOF,
    ERR_FILE,
    ERR_ARGS
} status_t;

typedef struct Node {
    char *name;
    struct Node **children;
    size_t count;
    size_t capacity;
} Node;

static const char *status_text(status_t st) {
    switch (st) {
        case SUCCESS:    return "успех";
        case ERR_NULL:   return "передан нулевой указатель";
        case ERR_MEMORY: return "не удалось выделить память";
        case ERR_VALUE:  return "некорректное скобочное выражение";
        case ERR_DEPTH:  return "слишком глубокая вложенность";
        case ERR_EMPTY:  return "пустая строка";
        case ERR_EOF:    return "конец файла";
        case ERR_FILE:   return "ошибка работы с файлом";
        case ERR_ARGS:   return "неверные аргументы командной строки";
        default:         return "неизвестная ошибка";
    }
}

// работа с деревом

static status_t node_create(const char *name, size_t len, Node **out) {
    if (name == NULL || out == NULL) return ERR_NULL;
    if (*out != NULL) return ERR_VALUE;
    if (len == 0) return ERR_VALUE;

    Node *node = malloc(sizeof *node);
    if (node == NULL) return ERR_MEMORY;

    node->name = malloc(len + 1);
    if (node->name == NULL) {
        free(node);
        return ERR_MEMORY;
    }

    memcpy(node->name, name, len);
    node->name[len] = '\0';
    node->children = NULL;
    node->count = 0;
    node->capacity = 0;

    *out = node;
    return SUCCESS;
}

static void tree_destroy(Node *node) {
    if (node == NULL) return;

    for (size_t i = 0; i < node->count; i++)
        tree_destroy(node->children[i]);

    free(node->children);
    free(node->name);
    free(node);
}

static status_t node_add_child(Node *parent, Node *child) {
    if (parent == NULL || child == NULL) return ERR_NULL;

    if (parent->count == parent->capacity) {
        if (parent->capacity > SIZE_MAX / sizeof(Node *) / 2) return ERR_MEMORY;

        size_t new_cap = (parent->capacity == 0) ? 2 : parent->capacity * 2;
        Node **tmp = realloc(parent->children, new_cap * sizeof *tmp);
        if (tmp == NULL) return ERR_MEMORY;
        parent->children = tmp;
        parent->capacity = new_cap;
    }

    parent->children[parent->count] = child;
    parent->count++;
    return SUCCESS;
}

static size_t tree_height(const Node *node) {
    if (node == NULL) return 0;

    size_t max_child = 0;
    for (size_t i = 0; i < node->count; i++) {
        size_t h = tree_height(node->children[i]);
        if (h > max_child) max_child = h;
    }
    return max_child + 1;
}

// парсинг без ввода

static int is_separator(char c) {
    return c == '(' || c == ')' || c == ',' || isspace((unsigned char)c);
}

static void skip_spaces(const char *s, size_t *pos) {
    while (s[*pos] != '\0' && isspace((unsigned char)s[*pos])) (*pos)++;
}

// узел := имя [ '(' узел { ',' узел } ')' ]
static status_t parse_node(const char *s, size_t *pos, size_t depth, Node **out) {
    if (s == NULL || pos == NULL || out == NULL) return ERR_NULL;
    if (depth > MAX_DEPTH) return ERR_DEPTH;

    skip_spaces(s, pos);
    size_t start = *pos;
    while (s[*pos] != '\0' && !is_separator(s[*pos])) (*pos)++;
    if (*pos == start) return ERR_VALUE;    // имя узла пустое

    Node *node = NULL;
    status_t st = node_create(s + start, *pos - start, &node);
    if (st != SUCCESS) return st;

    skip_spaces(s, pos);
    if (s[*pos] == '(') {
        (*pos)++;

        int done = 0;
        while (!done) {
            Node *child = NULL;
            st = parse_node(s, pos, depth + 1, &child);
            if (st != SUCCESS) {
                tree_destroy(node);
                return st;
            }

            st = node_add_child(node, child);
            if (st != SUCCESS) {
                tree_destroy(child);
                tree_destroy(node);
                return st;
            }

            skip_spaces(s, pos);
            if (s[*pos] == ',') {
                (*pos)++;
            } else if (s[*pos] == ')') {
                (*pos)++;
                done = 1;
            } else {
                tree_destroy(node);
                return ERR_VALUE;
            }
        }
    }

    *out = node;
    return SUCCESS;
}

static status_t tree_parse(const char *line, Node **out) {
    if (line == NULL || out == NULL) return ERR_NULL;
    if (*out != NULL) return ERR_VALUE;

    size_t pos = 0;
    skip_spaces(line, &pos);
    if (line[pos] == '\0') return ERR_EMPTY;

    Node *root = NULL;
    status_t st = parse_node(line, &pos, 0, &root);
    if (st != SUCCESS) return st;

    skip_spaces(line, &pos);
    if (line[pos] != '\0') { 
        tree_destroy(root);
        return ERR_VALUE;
    }

    *out = root;
    return SUCCESS;
}

// ввод вывод

static status_t read_line(FILE *in, char **out, size_t *out_len) {
    if (in == NULL || out == NULL || out_len == NULL) return ERR_NULL;
    if (*out != NULL) return ERR_VALUE;

    size_t cap = 32, len = 0;
    char *buf = malloc(cap);
    if (buf == NULL) return ERR_MEMORY;

    int c;
    while ((c = fgetc(in)) != EOF && c != '\n') {
        if (len + 1 >= cap) {
            if (cap > SIZE_MAX / 2) { free(buf); return ERR_MEMORY; }

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

    *out = buf;
    *out_len = len;
    return SUCCESS;
}

static status_t print_children(FILE *out, const Node *node, char *prefix, size_t prefix_len) {
    if (out == NULL || node == NULL || prefix == NULL) return ERR_NULL;

    for (size_t i = 0; i < node->count; i++) {
        const Node *child = node->children[i];
        int last = (i + 1 == node->count);

        if (fprintf(out, "%s%s%s\n", prefix, last ? "`-- " : "+-- ", child->name) < 0) return ERR_FILE;

        memcpy(prefix + prefix_len, last ? "    " : "|   ", 4);
        prefix[prefix_len + 4] = '\0';

        status_t st = print_children(out, child, prefix, prefix_len + 4);
        prefix[prefix_len] = '\0';
        if (st != SUCCESS) return st;
    }
    return SUCCESS;
}

static status_t tree_print(FILE *out, const Node *root) {
    if (out == NULL || root == NULL) return ERR_NULL;

    size_t height = tree_height(root);
    if (height - 1 > (SIZE_MAX - 1) / 4) return ERR_MEMORY;

    char *prefix = malloc((height - 1) * 4 + 1);
    if (prefix == NULL) return ERR_MEMORY;
    prefix[0] = '\0';

    status_t st = SUCCESS;
    if (fprintf(out, "%s\n", root->name) < 0) st = ERR_FILE;
    if (st == SUCCESS) st = print_children(out, root, prefix, 0);

    free(prefix);
    return st;
}

static status_t process_file(FILE *in, FILE *out, size_t *trees_count) {
    if (in == NULL || out == NULL || trees_count == NULL) return ERR_NULL;

    *trees_count = 0;
    size_t line_no = 0;

    while (1) {
        char *line = NULL;
        size_t len = 0;
        status_t st = read_line(in, &line, &len);
        if (st == ERR_EOF) return SUCCESS;
        if (st != SUCCESS) return st;
        line_no++;

        Node *root = NULL;
        st = tree_parse(line, &root);

        switch (st) {
            case SUCCESS:
                if (fprintf(out, "Строка %zu: %s\n", line_no, line) < 0) st = ERR_FILE;
                if (st == SUCCESS) st = tree_print(out, root);
                if (st == SUCCESS && fprintf(out, "\n") < 0) st = ERR_FILE;
                if (st == SUCCESS) (*trees_count)++;
                tree_destroy(root);
                break;
            case ERR_EMPTY:
                st = SUCCESS;     
                break;
            case ERR_VALUE:
            case ERR_DEPTH:
                if (fprintf(out, "Строка %zu: ошибка — %s\n\n", line_no, status_text(st)) < 0) st = ERR_FILE;
                else st = SUCCESS;
                break;
            default:
                break;          
        }

        free(line);
        if (st != SUCCESS) return st;
    }
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Использование: %s <входной файл> <выходной файл>\n", argv[0]);
        return 1;
    }
    if (argv[1][0] == '\0' || argv[2][0] == '\0') {
        fprintf(stderr, "Ошибка: путь к файлу не может быть пустым\n");
        return 1;
    }
    if (strcmp(argv[1], argv[2]) == 0) {
        fprintf(stderr, "Ошибка: входной и выходной файлы должны различаться\n");
        return 1;
    }

    FILE *in = fopen(argv[1], "r");
    if (in == NULL) {
        fprintf(stderr, "Ошибка: не удалось открыть файл '%s'\n", argv[1]);
        return 1;
    }

    FILE *out = fopen(argv[2], "w");
    if (out == NULL) {
        fprintf(stderr, "Ошибка: не удалось создать файл '%s'\n", argv[2]);
        fclose(in);
        return 1;
    }

    size_t trees = 0;
    status_t st = process_file(in, out, &trees);

    fclose(in);
    if (fclose(out) != 0 && st == SUCCESS) st = ERR_FILE;

    if (st != SUCCESS) {
        fprintf(stderr, "Ошибка: %s (код %d)\n", status_text(st), (int)st);
        return 1;
    }

    printf("Построено деревьев: %zu. Результат записан в '%s'\n", trees, argv[2]);
    return 0;
}