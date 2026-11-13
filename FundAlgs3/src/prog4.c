#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <math.h>

// 4.1

typedef enum {
    SUCCESS = 0,
    ERR_NULL,
    ERR_MEMORY,
    ERR_VALUE,
    ERR_NOT_FOUND,
    ERR_EOF,
    ERR_EXISTS
} status_t;

typedef struct {
    char *data;
    int length;
} String;

status_t string_create(const char *init, String **out_str) {
    if (init == NULL || out_str == NULL) return ERR_NULL;
    if (*out_str != NULL) return ERR_VALUE;

    size_t len = strlen(init);
    if (len > (size_t)INT_MAX) return ERR_VALUE;

    String *str = malloc(sizeof *str);
    if (str == NULL) return ERR_MEMORY;

    str->data = malloc(len + 1);
    if (str->data == NULL) {
        free(str);
        return ERR_MEMORY;
    }

    memcpy(str->data, init, len + 1);
    str->length = (int)len;
    *out_str = str;
    return SUCCESS;
}

status_t string_destroy(String **str) {
    if (str == NULL || *str == NULL) return ERR_NULL;

    free((*str)->data);
    free(*str);
    *str = NULL;

    return SUCCESS;
}

status_t string_compare(const String *s1, const String *s2, int *result) {
    if (s1 == NULL || s2 == NULL || result == NULL) return ERR_NULL;
    if (s1->data == NULL || s2->data == NULL) return ERR_VALUE;

    if (s1->length != s2->length)
        *result = (s1->length > s2->length) ? 1 : -1;
    else
        *result = strcmp(s1->data, s2->data);
    return SUCCESS;
}

status_t string_equal(const String *s1, const String *s2, int *result) {
    if (s1 == NULL || s2 == NULL || result == NULL) return ERR_NULL;
    if (s1->data == NULL || s2->data == NULL) return ERR_VALUE;

    *result = (s1->length == s2->length && strcmp(s1->data, s2->data) == 0);
    return SUCCESS;
}

status_t string_copy(String *dest, const String *src) {
    if (dest == NULL || src == NULL) return ERR_NULL;
    if (dest->data == NULL || src->data == NULL) return ERR_VALUE;
    if (dest == src) return SUCCESS;

    char *new_data = realloc(dest->data, (size_t)src->length + 1);
    if (new_data == NULL) return ERR_MEMORY;

    memcpy(new_data, src->data, (size_t)src->length + 1);
    dest->data = new_data;
    dest->length = src->length;

    return SUCCESS;
}

status_t string_copy_new(const String *src, String **dest) {
    if (src == NULL || dest == NULL) return ERR_NULL;
    if (src->data == NULL) return ERR_VALUE;

    return string_create(src->data, dest);     
}

status_t string_concat(String *dest, const String *src) {
    if (dest == NULL || src == NULL) return ERR_NULL;
    if (dest->data == NULL || src->data == NULL) return ERR_VALUE;
    if ((size_t)dest->length + (size_t)src->length > (size_t)INT_MAX) return ERR_VALUE;

    int old_len = dest->length;
    int new_len = old_len + src->length;

    char *new_data = realloc(dest->data, (size_t)new_len + 1);
    if (new_data == NULL) return ERR_MEMORY;
    dest->data = new_data;

    if (src == dest) {
        memmove(new_data + old_len, new_data, (size_t)old_len + 1);
    } else {
        memcpy(new_data + old_len, src->data, (size_t)src->length + 1);
    }

    dest->length = new_len;
    return SUCCESS;
}

// для 4.2

status_t string_clear(String *str) {
    if (str == NULL) return ERR_NULL;

    free(str->data);
    str->data = NULL;
    str->length = 0;

    return SUCCESS;
}

// 4.2

typedef struct {
    String city;        
    String street;      
    unsigned int house; 
    String building;       
    unsigned int apartment;
    String index;        
} Address;

typedef struct {
    Address address;
    double weight;
    String id;
    String creation_time;
    String delivery_time;
    String actual_delivery_time;
} Mail;

typedef struct {
    Address *post_address;
    Mail *mails;
    size_t count;
    size_t capacity;
} Post;

// парсинг без ввода

static int days_in_month(int m, int y) {
    const int d[] = {31,28,31,30,31,30,31,31,30,31,30,31};
    if (m == 2 && ((y % 4 == 0 && y % 100 != 0) || y % 400 == 0)) return 29;
    return d[m - 1];
}

static status_t parse_time(const char *s, time_t *out) {
    if (s == NULL || out == NULL) return ERR_NULL;
    if (strlen(s) != 19) return ERR_VALUE;

    for (int i = 0; i < 19; i++) {
        if (i == 2 || i == 5 || i == 13 || i == 16) {
            if (s[i] != ':') return ERR_VALUE;
        } else if (i == 10) {
            if (s[i] != ' ') return ERR_VALUE;
        } else if (s[i] < '0' || s[i] > '9') return ERR_VALUE;
    }

    int d  = (s[0]-'0')*10 + (s[1]-'0');
    int mo = (s[3]-'0')*10 + (s[4]-'0');
    int y  = (s[6]-'0')*1000 + (s[7]-'0')*100 + (s[8]-'0')*10 + (s[9]-'0');
    int h  = (s[11]-'0')*10 + (s[12]-'0');
    int mi = (s[14]-'0')*10 + (s[15]-'0');
    int se = (s[17]-'0')*10 + (s[18]-'0');
    if (y < 1900 || mo < 1 || mo > 12 || d < 1 || d > days_in_month(mo, y) || h > 23 || mi > 59 || se > 59) return ERR_VALUE;

    struct tm tm = {0};
    tm.tm_mday = d; 
    tm.tm_mon = mo - 1; 
    tm.tm_year = y - 1900;
    tm.tm_hour = h; 
    tm.tm_min = mi; 
    tm.tm_sec = se;
    tm.tm_isdst = -1;
    time_t t = mktime(&tm);

    if (t == (time_t)-1) return ERR_VALUE;
    *out = t;

    return SUCCESS;
}

static status_t validate_digits(const String *s, int required_len) {
    if (s == NULL || s->data == NULL) return ERR_NULL;
    if (s->length != required_len) return ERR_VALUE;

    for (int i = 0; i < s->length; i++)
        if (s->data[i] < '0' || s->data[i] > '9') return ERR_VALUE;

    return SUCCESS;
}

static status_t parse_uint(const char *s, unsigned int *out) {
    if (s == NULL || out == NULL) return ERR_NULL;
    if (*s == '\0') return ERR_VALUE;

    unsigned int v = 0;
    for (int i = 0; s[i] != '\0'; i++) {  
        if (s[i] < '0' || s[i] > '9') return ERR_VALUE;
        unsigned int digit = s[i] - '0';     

        if (v > (UINT_MAX - digit) / 10u) return ERR_VALUE;
        v = v * 10u + digit;
    }
    *out = v;
    return SUCCESS;
}

static status_t parse_weight(const char *s, double *out) {
    if (s == NULL || out == NULL) return ERR_NULL;
    if (*s == '\0') return ERR_VALUE;

    char *end = NULL;
    double v = strtod(s, &end);
    if (end == s) return ERR_VALUE;

    while (*end != '\0' && isspace((unsigned char)*end)) end++;
    if (*end != '\0') return ERR_VALUE;
    if (!isfinite(v) || v < 0.0) return ERR_VALUE;

    *out = v;
    return SUCCESS;
}

// вспомогательные 

static void address_clear(Address *a) {
    if (a == NULL) return;

    string_clear(&a->city);
    string_clear(&a->street);
    string_clear(&a->building);
    string_clear(&a->index);

    a->house = 0;
    a->apartment = 0;
}

static void mail_clear(Mail *m) {
    if (m == NULL) return;

    string_clear(&m->id);
    string_clear(&m->creation_time);
    string_clear(&m->delivery_time);
    string_clear(&m->actual_delivery_time);
    address_clear(&m->address);
    
    m->weight = 0.0;
}

static status_t format_time(time_t t, String *out) {
    if (out == NULL) return ERR_NULL;
    if (out->data != NULL) return ERR_VALUE;       /* поле должно быть пустым */
    struct tm *lt = localtime(&t);
    if (lt == NULL) return ERR_VALUE;
    char *buf = malloc(20);                        /* 19 + '\0', в куче */
    if (buf == NULL) return ERR_MEMORY;
    if (strftime(buf, 20, "%d:%m:%Y %H:%M:%S", lt) != 19) {
        free(buf);
        return ERR_VALUE;
    }
    out->data = buf;
    out->length = 19;
    return SUCCESS;
}

static status_t mail_status(const Mail *m, int *out) {
    if (m == NULL || out == NULL) return ERR_NULL;
    if (m->actual_delivery_time.length == 0) { *out = 0; return SUCCESS; }
    time_t plan = 0, fact = 0;
    status_t st = parse_time(m->delivery_time.data, &plan);
    if (st == SUCCESS) st = parse_time(m->actual_delivery_time.data, &fact);
    if (st != SUCCESS) return st;
    *out = (fact <= plan) ? 1 : 2;
    return SUCCESS;
}

static status_t mail_matches(const Mail *m, int expired, time_t now, int *take) {
    if (m == NULL || take == NULL) return ERR_NULL;
    if (!expired) { *take = (m->actual_delivery_time.length > 0); return SUCCESS; }
    if (m->actual_delivery_time.length > 0) { *take = 0; return SUCCESS; }  /* вручена — не просрочена */
    time_t plan = 0;
    status_t st = parse_time(m->delivery_time.data, &plan);
    if (st != SUCCESS) return st;
    *take = (plan < now);
    return SUCCESS;
}

// функции для меню

static int mail_compare(const Mail *a, const Mail *b) {
    int r = strcmp(a->address.index.data, b->address.index.data);
    if (r != 0) return r;
    return strcmp(a->id.data, b->id.data);
}

static status_t post_add_mail(Post *post, Mail *mail) {
    if (post == NULL || mail == NULL) return ERR_NULL;

    for (size_t i = 0; i < post->count; i++)        
        if (strcmp(post->mails[i].id.data, mail->id.data) == 0) return ERR_EXISTS;

    if (post->count == post->capacity) {
        if (post->capacity > SIZE_MAX / sizeof(Mail) / 2) return ERR_MEMORY;

        size_t new_cap = (post->capacity == 0) ? 4 : post->capacity * 2;
        Mail *tmp = realloc(post->mails, new_cap * sizeof *tmp);
        if (tmp == NULL) return ERR_MEMORY;
        post->mails = tmp;
        post->capacity = new_cap;
}

    size_t pos = post->count;
    while (pos > 0 && mail_compare(&post->mails[pos - 1], mail) > 0) {
        post->mails[pos] = post->mails[pos - 1];
        pos--;
    }
    post->mails[pos] = *mail; 
    *mail = (Mail){0};
    post->count++;
    return SUCCESS;
}

static status_t post_remove_mail(Post *post, const char *id) {
    if (post == NULL || id == NULL) return ERR_NULL;

    for (size_t i = 0; i < post->count; i++) {
        if (strcmp(post->mails[i].id.data, id) == 0) {
            mail_clear(&post->mails[i]);

            for (size_t j = i; j + 1 < post->count; j++)
                post->mails[j] = post->mails[j + 1];
            post->count--;

            return SUCCESS;
        }
    }
    return ERR_NOT_FOUND;
}

static status_t post_find_mail(const Post *post, const char *id, const Mail **out) {
    if (post == NULL || id == NULL || out == NULL) return ERR_NULL;

    for (size_t i = 0; i < post->count; i++)
        if (strcmp(post->mails[i].id.data, id) == 0) {
            *out = &post->mails[i];
            return SUCCESS;
        }

    return ERR_NOT_FOUND;
}

static status_t post_deliver_mail(Post *post, const char *id) {
    if (post == NULL || id == NULL) return ERR_NULL;

    for (size_t i = 0; i < post->count; i++) {
        if (strcmp(post->mails[i].id.data, id) != 0) continue;

        Mail *m = &post->mails[i];
        if (m->actual_delivery_time.length > 0) return ERR_VALUE;   
        time_t now = time(NULL);
        if (now == (time_t)-1) return ERR_VALUE;
        time_t created = 0;
        status_t st = parse_time(m->creation_time.data, &created);
        if (st != SUCCESS) return st;
        if (now < created) return ERR_VALUE;
        return format_time(now, &m->actual_delivery_time);
    }
    return ERR_NOT_FOUND;
}

static status_t post_select(const Post *post, int expired, const Mail ***out_list, size_t *out_count) {
    if (post == NULL || out_list == NULL || out_count == NULL) return ERR_NULL;
    *out_list = NULL;
    *out_count = 0;
    time_t now = time(NULL);
    if (now == (time_t)-1) return ERR_VALUE;

    size_t n = 0;
    for (size_t i = 0; i < post->count; i++) {
        int take = 0;
        status_t st = mail_matches(&post->mails[i], expired, now, &take);
        if (st != SUCCESS) return st;
        if (take) n++;
    }
    if (n == 0) return SUCCESS;

    const Mail **list = malloc(n * sizeof *list);
    if (list == NULL) return ERR_MEMORY;

    size_t k = 0;
    for (size_t i = 0; i < post->count; i++) {
        const Mail *m = &post->mails[i];
        int take = 0;
        status_t st = mail_matches(m, expired, now, &take);
        if (st != SUCCESS) { free(list); return st; }
        if (!take) continue;

        time_t tc = 0;
        st = parse_time(m->creation_time.data, &tc);
        if (st != SUCCESS) { free(list); return st; }
        size_t pos = k;                       /* вставка по времени создания */
        while (pos > 0) {
            time_t tp = 0;
            st = parse_time(list[pos - 1]->creation_time.data, &tp);
            if (st != SUCCESS) { free(list); return st; }
            if (tp <= tc) break;
            list[pos] = list[pos - 1];
            pos--;
        }
        list[pos] = m;
        k++;
    }
    *out_list = list;
    *out_count = n;
    return SUCCESS;
}

/* ================= Ввод/вывод ================= */

/* Читает строку любой длины: буфер растёт удвоением, фиксированного лимита нет.
   Буфер становится собственностью String (освободится через string_clear) */
static status_t read_line(FILE *in, String *out) {
    if (in == NULL || out == NULL) return ERR_NULL;
    if (out->data != NULL) return ERR_VALUE;
    size_t cap = 32, len = 0;
    char *buf = malloc(cap);
    if (buf == NULL) return ERR_MEMORY;
    int c;
    while ((c = fgetc(in)) != EOF && c != '\n') {
        if (len + 1 >= cap) {
            size_t new_cap = cap * 2;
            if (new_cap <= cap || new_cap > (size_t)INT_MAX + 1) {
                free(buf);
                return ERR_VALUE;
            }
            char *nb = realloc(buf, new_cap);
            if (nb == NULL) { free(buf); return ERR_MEMORY; }
            buf = nb;
            cap = new_cap;
        }
        buf[len++] = (char)c;
    }
    if (ferror(in)) { free(buf); return ERR_VALUE; }
    if (c == EOF && len == 0) { free(buf); return ERR_EOF; }
    if (len > 0 && buf[len - 1] == '\r') len--;     /* Windows-перенос \r\n */
    buf[len] = '\0';
    out->data = buf;
    out->length = (int)len;
    return SUCCESS;
}

/* Единственная обёртка: показать приглашение и прочитать строку */
static status_t read_field(const char *prompt, String *out) {
    printf("%s", prompt);
    fflush(stdout);   /* вытолкнуть приглашение на экран ДО ожидания ввода */
    return read_line(stdin, out);
}

static status_t input_address(Address *a) {
    status_t st = read_field("Город: ", &a->city);
    if (st == SUCCESS && a->city.length == 0) {
        printf("Ошибка: город пустой\n");
        st = ERR_VALUE;
    }
    if (st != SUCCESS) return st;

    st = read_field("Улица: ", &a->street);
    if (st == SUCCESS && a->street.length == 0) {
        printf("Ошибка: улица пустая\n");
        st = ERR_VALUE;
    }
    if (st != SUCCESS) return st;

    String s = {0};
    st = read_field("Номер дома: ", &s);
    if (st == SUCCESS) {
        st = parse_uint(s.data, &a->house);
        if (st == SUCCESS && a->house == 0) st = ERR_VALUE;   /* натуральное = >= 1 */
        if (st == ERR_VALUE) printf("Ошибка: номер дома — натуральное число\n");
    }
    string_clear(&s);
    if (st != SUCCESS) return st;

    st = read_field("Корпус (можно пусто): ", &a->building);
    if (st != SUCCESS) return st;

    st = read_field("Номер квартиры: ", &s);
    if (st == SUCCESS) {
        st = parse_uint(s.data, &a->apartment);
        if (st == SUCCESS && a->apartment == 0) st = ERR_VALUE;
        if (st == ERR_VALUE) printf("Ошибка: номер квартиры — натуральное число\n");
    }
    string_clear(&s);
    if (st != SUCCESS) return st;

    st = read_field("Индекс (6 цифр): ", &a->index);
    if (st == SUCCESS && validate_digits(&a->index, 6) != SUCCESS) {
        printf("Ошибка: индекс — ровно 6 цифр\n");
        st = ERR_VALUE;
    }
    return st;
}

static status_t input_mail(Mail *m) {
    status_t st = read_field("Почтовый идентификатор (14 цифр): ", &m->id);
    if (st == SUCCESS && validate_digits(&m->id, 14) != SUCCESS) {
        printf("Ошибка: ID — ровно 14 цифр\n");
        st = ERR_VALUE;
    }
    if (st != SUCCESS) return st;

    time_t tc = 0, td = 0;
    st = read_field("Время создания (dd:MM:yyyy hh:mm:ss): ", &m->creation_time);
    if (st == SUCCESS && parse_time(m->creation_time.data, &tc) != SUCCESS) {
        printf("Ошибка: неверный формат времени\n");
        st = ERR_VALUE;
    }
    if (st != SUCCESS) return st;

    st = read_field("Время вручения (dd:MM:yyyy hh:mm:ss): ", &m->delivery_time);
    if (st == SUCCESS && parse_time(m->delivery_time.data, &td) != SUCCESS) {
        printf("Ошибка: неверный формат времени\n");
        st = ERR_VALUE;
    }
    if (st != SUCCESS) return st;

    if (tc > td) {
        printf("Ошибка: вручение раньше создания\n");
        return ERR_VALUE;
    }

    st = input_address(&m->address);
    if (st != SUCCESS) return st;

    String s = {0};
    st = read_field("Вес посылки: ", &s);
    if (st == SUCCESS) {
        st = parse_weight(s.data, &m->weight);
        if (st == ERR_VALUE) printf("Ошибка: вес — неотрицательное число\n");
    }
    string_clear(&s);
    return st;
}

static void print_mail(const Mail *m) {
    int stt = 0;
    const char *label = "не вручена";
    if (mail_status(m, &stt) == SUCCESS)
        label = (stt == 1) ? "вручена вовремя" : (stt == 2) ? "вручена с задержкой" : "не вручена";
    printf("  ID: %s\n  Индекс: %s\n  Адрес: г. %s, ул. %s, д. %u, корп. '%s', кв. %u\n"
           "  Вес: %.3f\n  Создано: %s\n  Срок вручения: %s\n  Факт вручения: %s\n  Статус: %s\n",
           m->id.data, m->address.index.data,
           m->address.city.data, m->address.street.data, m->address.house,
           m->address.building.data, m->address.apartment,
           m->weight, m->creation_time.data, m->delivery_time.data,
           m->actual_delivery_time.length > 0 ? m->actual_delivery_time.data : "—",
           label);
}

static void print_menu(void) {
    printf("\n=== ПОЧТОВОЕ ОТДЕЛЕНИЕ ===\n"
           "1. Добавить посылку\n"
           "2. Удалить посылку по ID\n"
           "3. Найти посылку по ID\n"
           "4. Показать доставленные (по времени создания)\n"
           "5. Показать с истёкшим сроком доставки (по времени создания)\n"
           "6. Показать все (всегда отсортированы: индекс, затем ID)\n"
           "7. Отметить вручение по ID\n"
           "0. Выход\n"
           "Выберите действие: ");
    fflush(stdout);
}

int main(void) {
    Post post = {0};
    Address office = {0};
    int running = 1;

    printf("Адрес почтового отделения:\n");
    status_t st = input_address(&office);
    if (st != SUCCESS) {
        address_clear(&office);
        if (st == ERR_EOF) return 0;
        fprintf(stderr, "Некорректный адрес отделения (код %d)\n", (int)st);
        return 1;
    }
    post.post_address = &office;

    while (running) {
        print_menu();
        String line = {0};
        st = read_line(stdin, &line);
        if (st != SUCCESS) {
            string_clear(&line);
            if (st != ERR_EOF) fprintf(stderr, "Ошибка ввода (код %d)\n", (int)st);
            break;
        }
        unsigned int choice = 0;
        if (parse_uint(line.data, &choice) != SUCCESS || choice > 6) {
            printf("Неверный пункт меню\n");
            string_clear(&line);
            continue;
        }
        string_clear(&line);

        switch (choice) {
            case 0:
                running = 0;
                break;
            case 1: {
                Mail m = {0};
                st = input_mail(&m);
                if (st == SUCCESS) st = post_add_mail(&post, &m);
                mail_clear(&m);   /* после успешного добавления m уже обнулена — no-op */
                if (st == SUCCESS) printf("Посылка добавлена\n");
                else if (st == ERR_EXISTS) printf("Посылка с таким ID уже существует\n");
                else if (st == ERR_EOF) running = 0;
                else printf("Добавление отменено (код %d)\n", (int)st);
                break;
            }
            case 2: {
                String id = {0};
                st = read_field("ID для удаления (14 цифр): ", &id);
                if (st == SUCCESS) {
                    if (validate_digits(&id, 14) != SUCCESS) {
                        printf("Ошибка: ID — ровно 14 цифр\n");
                        st = ERR_VALUE;
                    } else {
                        st = post_remove_mail(&post, id.data);
                    }
                }
                if (st == SUCCESS) printf("Посылка удалена\n");
                else if (st == ERR_NOT_FOUND) printf("Посылка не найдена\n");
                else if (st == ERR_EOF) running = 0;
                string_clear(&id);
                break;
            }
            case 3: {
                String id = {0};
                const Mail *m = NULL;
                st = read_field("ID для поиска (14 цифр): ", &id);
                if (st == SUCCESS) {
                    if (validate_digits(&id, 14) != SUCCESS) {
                        printf("Ошибка: ID — ровно 14 цифр\n");
                        st = ERR_VALUE;
                    } else {
                        st = post_find_mail(&post, id.data, &m);
                    }
                }
                if (st == SUCCESS) print_mail(m);
                else if (st == ERR_NOT_FOUND) printf("Посылка не найдена\n");
                else if (st == ERR_EOF) running = 0;
                string_clear(&id);
                break;
            }
            case 4:
            case 5: {
                const Mail **list = NULL;
                size_t n = 0;
                st = post_select(&post, choice == 5, &list, &n);
                if (st == SUCCESS) {
                    /* case 4/5, внутри if (st == SUCCESS): */
                    printf(choice == 5 ? "С истёкшим сроком доставки:\n" : "Доставленные:\n");
                    for (size_t i = 0; i < n; i++) {
                        const Mail *m = list[i];
                        int stt = 0;
                        const char *label = "";
                        if (choice == 4 && mail_status(m, &stt) == SUCCESS)
                            label = (stt == 2) ? " [задержка]" : " [вовремя]";
                        printf("%s | индекс %s | создано %s | срок %s | факт %s%s\n", m->id.data, m->address.index.data, m->creation_time.data, m->delivery_time.data, m->actual_delivery_time.length > 0 ? m->actual_delivery_time.data : "—", label);
                    }
                    if (n == 0) printf("Нет отправлений\n");
                    free(list);
                } else {
                    printf("Ошибка выборки (код %d)\n", (int)st);
                }
                break;
            }
            case 6:
                if (post.count == 0) printf("Хранилище пусто\n");
                else for (size_t i = 0; i < post.count; i++)
                    printf("%s | индекс %s | создано %s | вручение %s\n",
                           post.mails[i].id.data, post.mails[i].address.index.data,
                           post.mails[i].creation_time.data, post.mails[i].delivery_time.data);
                break;
            case 7: {
                String id = {0};
                st = read_field("ID для вручения (14 цифр): ", &id);
                if (st == SUCCESS) {
                    if (validate_digits(&id, 14) != SUCCESS) {
                        printf("Ошибка: ID — ровно 14 цифр\n");
                        st = ERR_VALUE;
                    } else {
                        st = post_deliver_mail(&post, id.data);
                    }
                }
                if (st == SUCCESS) printf("Вручение отмечено\n");
                else if (st == ERR_NOT_FOUND) printf("Посылка не найдена\n");
                else if (st == ERR_VALUE) printf("Уже вручена или вручение невозможно\n");
                else if (st == ERR_EOF) running = 0;
                string_clear(&id);
                break;
            }
            default:
                break;
        }
    }

    for (size_t i = 0; i < post.count; i++) mail_clear(&post.mails[i]);
    free(post.mails);
    address_clear(&office);
    printf("Программа завершена.\n");
    return 0;
}