#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <sys/stat.h>

// 9

typedef enum {
    SUCCESS = 0,
    ERR_NULL,
    ERR_MEMORY,
    ERR_VALUE,
    ERR_NOT_FOUND,
    ERR_EOF,
    ERR_FILE,
    ERR_SAME_FILE,
    ERR_OVERFLOW
} status_t;

typedef struct {
    char *data;
    int length;
} String;

typedef struct Node {
    String word;
    size_t count;
    struct Node *left;
    struct Node *right;
} Node;

typedef struct {
    Node *root;
    size_t count;
} Tree;

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
        case ERR_SAME_FILE: return "файлы совпадают";
        case ERR_OVERFLOW: return "переполнение счётчика";
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

static size_t utf8_length(const char *s) {
    size_t n = 0;
    for (; *s != '\0'; s++) {
        if (((unsigned char)*s & 0xC0) != 0x80) n++;
    }
    return n;
}

// разделители

static status_t parse_separator(const char *arg, char *out) {
    if (arg == NULL || out == NULL) return ERR_NULL;

    size_t len = strlen(arg);
    if (len == 1) *out = arg[0];
    else if (len == 2 && arg[0] == '\\' && arg[1] == 'n') *out = '\n';
    else if (len == 2 && arg[0] == '\\' && arg[1] == 't') *out = '\t';
    else if (len == 2 && arg[0] == '\\' && arg[1] == 'r') *out = '\r';
    else return ERR_VALUE;

    return SUCCESS;
}

static status_t make_separators(int count, char *const args[], String *out) {
    if (args == NULL || out == NULL) return ERR_NULL;
    if (count < 1 || out->data != NULL) return ERR_VALUE;

    char *buf = malloc((size_t)count + 1);
    if (buf == NULL) return ERR_MEMORY;

    for (int i = 0; i < count; i++) {
        status_t st = parse_separator(args[i], &buf[i]);
        if (st != SUCCESS) {
            free(buf);
            return st;
        }
    }
    buf[count] = '\0';

    out->data = buf;
    out->length = count;
    return SUCCESS;
}

static int is_separator(const String *separators, int c) {
    return memchr(separators->data, c, (size_t)separators->length) != NULL;
}

// дерево поиска

static void node_clear(Node *node) {
    if (node == NULL) return;

    node_clear(node->left);
    node_clear(node->right);
    string_clear(&node->word);
    free(node);
}

static void tree_clear(Tree *tree) {
    if (tree == NULL) return;

    node_clear(tree->root);
    tree->root = NULL;
    tree->count = 0;
}

static status_t tree_add(Tree *tree, const char *word, size_t delta) {
    if (tree == NULL || word == NULL) return ERR_NULL;
    if (word[0] == '\0' || delta == 0) return ERR_VALUE;

    Node *parent = NULL;
    Node *cur = tree->root;
    int cmp = 0;

    while (cur != NULL) {
        cmp = strcmp(word, cur->word.data);
        if (cmp == 0) {
            if (cur->count > SIZE_MAX - delta) return ERR_OVERFLOW;
            cur->count += delta;
            return SUCCESS;
        }
        parent = cur;
        cur = (cmp < 0) ? cur->left : cur->right;
    }

    Node *node = malloc(sizeof *node);
    if (node == NULL) return ERR_MEMORY;

    node->word = (String){0};
    status_t st = string_from_range(word, strlen(word), &node->word);
    if (st != SUCCESS) {
        free(node);
        return st;
    }
    node->count = delta;
    node->left = NULL;
    node->right = NULL;

    if (parent == NULL) tree->root = node;
    else if (cmp < 0) parent->left = node;
    else parent->right = node;
    tree->count++;
    return SUCCESS;
}

static status_t tree_find(const Tree *tree, const char *word, const Node **out) {
    if (tree == NULL || word == NULL || out == NULL) return ERR_NULL;

    const Node *cur = tree->root;
    while (cur != NULL) {
        int cmp = strcmp(word, cur->word.data);
        if (cmp == 0) {
            *out = cur;
            return SUCCESS;
        }
        cur = (cmp < 0) ? cur->left : cur->right;
    }
    return ERR_NOT_FOUND;
}

static status_t tree_build_from_file(const char *path, const String *separators, Tree *tree) {
    if (path == NULL || separators == NULL || tree == NULL) return ERR_NULL;

    FILE *f = fopen(path, "r");
    if (f == NULL) return ERR_FILE;

    size_t cap = 32, len = 0;
    char *buf = malloc(cap);
    if (buf == NULL) {
        fclose(f);
        return ERR_MEMORY;
    }

    status_t st = SUCCESS;
    int reading = 1;
    while (reading && st == SUCCESS) {
        int c = fgetc(f);

        if (c == EOF || is_separator(separators, c)) {
            if (len > 0) {
                buf[len] = '\0';
                st = tree_add(tree, buf, 1);
                len = 0;
            }
            if (c == EOF) reading = 0;
        } else if (c == '\0') {
            st = ERR_VALUE;
        } else {
            if (len + 1 >= cap) {
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
    }
    if (st == SUCCESS && ferror(f)) st = ERR_FILE;

    free(buf);
    fclose(f);
    return st;
}

static size_t node_depth(const Node *node) {
    if (node == NULL) return 0;

    size_t left = node_depth(node->left);
    size_t right = node_depth(node->right);
    return 1 + (left > right ? left : right);
}

static void collect_nodes(const Node *node, const Node **arr, size_t *pos) {
    if (node == NULL) return;

    collect_nodes(node->left, arr, pos);
    arr[(*pos)++] = node;
    collect_nodes(node->right, arr, pos);
}

static int compare_by_count(const void *a, const void *b) {
    const Node *x = *(const Node *const *)a;
    const Node *y = *(const Node *const *)b;

    if (x->count != y->count) return (x->count < y->count) ? 1 : -1;
    return strcmp(x->word.data, y->word.data);
}

static status_t tree_top(const Tree *tree, size_t n, const Node **arr, size_t *found_count) {
    if (tree == NULL || arr == NULL || found_count == NULL) return ERR_NULL;
    if (n == 0) return ERR_VALUE;
    if (tree->count == 0) return ERR_NOT_FOUND;

    size_t pos = 0;
    collect_nodes(tree->root, arr, &pos);
    qsort(arr, tree->count, sizeof *arr, compare_by_count);

    *found_count = (n < tree->count) ? n : tree->count;
    return SUCCESS;
}

static void find_extremes(const Node *node, const Node **longest, const Node **shortest) {
    if (node == NULL) return;

    find_extremes(node->left, longest, shortest);

    size_t len = utf8_length(node->word.data);
    if (*longest == NULL || len > utf8_length((*longest)->word.data)) *longest = node;
    if (*shortest == NULL || len < utf8_length((*shortest)->word.data)) *shortest = node;

    find_extremes(node->right, longest, shortest);
}

static int nodes_equal(const Node *a, const Node *b) {
    if (a == NULL && b == NULL) return 1;
    if (a == NULL || b == NULL) return 0;

    if (strcmp(a->word.data, b->word.data) != 0 || a->count != b->count) return 0;
    return nodes_equal(a->left, b->left) && nodes_equal(a->right, b->right);
}

// сохранение и восстановление

static status_t save_node(FILE *f, const Node *node) {
    if (node == NULL) return SUCCESS;

    size_t len = (size_t)node->word.length;
    if (fprintf(f, "%zu %zu ", node->count, len) < 0) return ERR_FILE;
    if (fwrite(node->word.data, 1, len, f) != len) return ERR_FILE;
    if (fputc('\n', f) == EOF) return ERR_FILE;

    status_t st = save_node(f, node->left);
    if (st == SUCCESS) st = save_node(f, node->right);
    return st;
}

static status_t tree_save(const Tree *tree, const char *path) {
    if (tree == NULL || path == NULL) return ERR_NULL;

    FILE *f = fopen(path, "w");
    if (f == NULL) return ERR_FILE;

    status_t st = SUCCESS;
    if (fprintf(f, "%zu\n", tree->count) < 0) st = ERR_FILE;
    if (st == SUCCESS) st = save_node(f, tree->root);

    if (fclose(f) != 0 && st == SUCCESS) st = ERR_FILE;
    return st;
}

static status_t load_record(FILE *f, size_t *count, String *word) {
    size_t len = 0;
    if (fscanf(f, "%zu %zu", count, &len) != 2) return ERR_VALUE;
    if (*count == 0 || len == 0 || len > (size_t)INT_MAX) return ERR_VALUE;
    if (fgetc(f) != ' ') return ERR_VALUE;

    char *buf = malloc(len + 1);
    if (buf == NULL) return ERR_MEMORY;

    if (fread(buf, 1, len, f) != len || fgetc(f) != '\n' || memchr(buf, '\0', len) != NULL) {
        free(buf);
        return ERR_VALUE;
    }
    buf[len] = '\0';

    word->data = buf;
    word->length = (int)len;
    return SUCCESS;
}

static status_t tree_load(const char *path, Tree *tree) {
    if (path == NULL || tree == NULL) return ERR_NULL;
    if (tree->root != NULL) return ERR_VALUE;

    FILE *f = fopen(path, "r");
    if (f == NULL) return ERR_FILE;

    size_t total = 0;
    status_t st = SUCCESS;
    if (fscanf(f, "%zu", &total) != 1) st = ERR_VALUE;

    for (size_t i = 0; i < total && st == SUCCESS; i++) {
        size_t count = 0;
        String word = {0};

        st = load_record(f, &count, &word);
        if (st == SUCCESS) {
            size_t before = tree->count;
            st = tree_add(tree, word.data, count);
            if (st == SUCCESS && tree->count == before) st = ERR_VALUE;
        }
        string_clear(&word);
    }

    if (st == SUCCESS) {
        int c = fgetc(f);
        while (c == ' ' || c == '\n') c = fgetc(f);
        if (c != EOF) st = ERR_VALUE;
    }
    if (st == SUCCESS && ferror(f)) st = ERR_FILE;

    fclose(f);
    if (st != SUCCESS) tree_clear(tree);
    return st;
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

static status_t validate_word(const String *separators, const String *word) {
    if (separators == NULL || word == NULL || word->data == NULL) return ERR_NULL;

    if (word->length == 0) {
        printf("Ошибка: слово не должно быть пустым\n");
        return ERR_VALUE;
    }
    for (int i = 0; i < word->length; i++) {
        if (is_separator(separators, (unsigned char)word->data[i])) {
            printf("Ошибка: слово не может содержать разделители\n");
            return ERR_VALUE;
        }
    }
    return SUCCESS;
}

static void print_stats(const Node *node) {
    if (node == NULL) return;

    print_stats(node->left);
    printf("\"%s\": %zu\n", node->word.data, node->count);
    print_stats(node->right);
}

static void print_structure(const Node *node, int level, const char *label) {
    if (node == NULL) return;

    printf("%*s%s \"%s\" (%zu)\n", level * 4, "", label, node->word.data, node->count);
    print_structure(node->left, level + 1, "L:");
    print_structure(node->right, level + 1, "R:");
}

static void print_menu(void) {
    printf("\nМеню статистики слов\n"
           "1. Показать все слова и их количество\n"
           "2. Сколько раз встречается слово\n"
           "3. Показать n самых частых слов\n"
           "4. Самое длинное и самое короткое слово\n"
           "5. Глубина дерева\n"
           "6. Показать структуру дерева\n"
           "7. Сохранить дерево в файл\n"
           "8. Восстановить дерево из файла\n"
           "0. Выход\n"
           "Выберите действие: ");
    fflush(stdout);
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        fprintf(stderr, "Использование: %s <файл_с_текстом> <разделитель1> [<разделитель2> ...]\n", argv[0]);
        return 1;
    }

    const char *text_path = argv[1];
    if (text_path[0] == '\0') {
        fprintf(stderr, "Ошибка: путь к файлу не должен быть пустым\n");
        return 1;
    }

    String separators = {0};
    status_t st = make_separators(argc - 2, argv + 2, &separators);
    if (st == ERR_VALUE) {
        fprintf(stderr, "Ошибка: каждый разделитель — один символ (или \\n, \\t, \\r)\n");
        return 1;
    } else if (st != SUCCESS) {
        fprintf(stderr, "Ошибка (%s)\n", status_text(st));
        return 1;
    }

    Tree tree = {0};
    st = tree_build_from_file(text_path, &separators, &tree);
    if (st != SUCCESS) {
        switch (st) {
            case ERR_FILE:
                fprintf(stderr, "Ошибка: не удалось прочитать файл '%s'\n", text_path);
                break;
            case ERR_VALUE:
                fprintf(stderr, "Ошибка: файл '%s' не является текстовым\n", text_path);
                break;
            default:
                fprintf(stderr, "Ошибка при чтении текста (%s)\n", status_text(st));
                break;
        }
        tree_clear(&tree);
        string_clear(&separators);
        return 1;
    }

    printf("Разных слов: %zu\n", tree.count);
    print_stats(tree.root);

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
        if (parse_st != SUCCESS || choice > 8) {
            printf("Неверный пункт меню\n");
            continue;
        }

        if (choice >= 1 && choice <= 6 && tree.count == 0) {
            printf("В тексте нет слов\n");
            continue;
        }

        const char *ok_message = NULL;
        st = SUCCESS;

        switch (choice) {
            case 0:
                running = 0;
                break;
            case 1:
                print_stats(tree.root);
                break;
            case 2: {
                String word = {0};
                st = read_field("Введите слово: ", &word);
                if (st == SUCCESS) st = validate_word(&separators, &word);
                if (st == SUCCESS) {
                    const Node *node = NULL;
                    st = tree_find(&tree, word.data, &node);
                    if (st == SUCCESS) {
                        printf("Количество вхождений слова \"%s\": %zu\n", word.data, node->count);
                    } else if (st == ERR_NOT_FOUND) {
                        printf("Слово \"%s\" не встречается (0 раз)\n", word.data);
                        st = SUCCESS;
                    }
                }
                string_clear(&word);
                break;
            }
            case 3: {
                String s = {0};
                unsigned int n = 0;
                st = read_field("Сколько слов показать (n): ", &s);
                if (st == SUCCESS) {
                    st = parse_uint(s.data, &n);
                    if (st == SUCCESS && n == 0) st = ERR_VALUE;
                    if (st == ERR_VALUE) printf("Ошибка: n — целое число не меньше 1\n");
                }
                string_clear(&s);

                if (st == SUCCESS) {
                    const Node **arr = malloc(tree.count * sizeof(*arr));
                    if (arr == NULL) {
                        st = ERR_MEMORY;
                    } else {
                        size_t shown = 0;
                        st = tree_top(&tree, n, arr, &shown);
                        if (st == SUCCESS) {
                            for (size_t i = 0; i < shown; i++)
                                printf("%zu. \"%s\": %zu\n", i + 1, arr[i]->word.data, arr[i]->count);
                        }
                        free(arr);
                    }
                }
                break;
            }
            case 4: {
                const Node *longest = NULL;
                const Node *shortest = NULL;
                find_extremes(tree.root, &longest, &shortest);
                printf("Самое длинное слово: \"%s\" (%zu симв.)\n", longest->word.data, utf8_length(longest->word.data));
                printf("Самое короткое слово: \"%s\" (%zu симв.)\n", shortest->word.data, utf8_length(shortest->word.data));
                break;
            }
            case 5:
                printf("Глубина дерева: %zu\n", node_depth(tree.root));
                break;
            case 6:
                print_structure(tree.root, 0, "корень:");
                break;
            case 7: {
                String path = {0};
                st = read_field("Путь к файлу для сохранения дерева: ", &path);
                if (st == SUCCESS && path.length == 0) {
                    printf("Ошибка: путь не должен быть пустым\n");
                    st = ERR_VALUE;
                }
                if (st == SUCCESS) st = check_different_files(text_path, path.data);
                if (st == SUCCESS) st = tree_save(&tree, path.data);
                string_clear(&path);
                ok_message = "Дерево сохранено";
                break;
            }
            case 8: {
                String path = {0};
                st = read_field("Путь к файлу с сохранённым деревом: ", &path);
                if (st == SUCCESS && path.length == 0) {
                    printf("Ошибка: путь не должен быть пустым\n");
                    st = ERR_VALUE;
                }

                Tree restored = {0};
                int same = 0;
                if (st == SUCCESS) {
                    st = tree_load(path.data, &restored);
                    if (st == ERR_VALUE) printf("Ошибка: файл не содержит корректно сохранённого дерева\n");
                }
                if (st == SUCCESS) {
                    same = nodes_equal(tree.root, restored.root);
                    printf("Дерево восстановлено: разных слов %zu\n", restored.count);
                    printf("Совпадает с текущим деревом (слова, счётчики и форма): %s\n", same ? "да" : "нет");
                    tree_clear(&tree);
                    tree = restored;
                    restored = (Tree){0};
                }
                tree_clear(&restored);
                string_clear(&path);
                break;
            }
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
                printf("Ошибка: нельзя записывать дерево в файл с исходным текстом\n");
                break;
            case ERR_FILE:
                printf("Ошибка: не удалось прочитать или записать файл\n");
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

    tree_clear(&tree);
    string_clear(&separators);
    printf("Программа завершена.\n");
    return 0;
}