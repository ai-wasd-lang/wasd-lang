// WASD Runtime Library Implementation
// Provides runtime support for Vec, String, HashMap

#include "wasd_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// ============== Vec Implementation ==============

#define VEC_INITIAL_CAPACITY 8

WasdVec* wasd_vec_new(size_t elem_size) {
    WasdVec* vec = (WasdVec*)malloc(sizeof(WasdVec));
    if (!vec) return NULL;

    vec->data = malloc(VEC_INITIAL_CAPACITY * elem_size);
    if (!vec->data) {
        free(vec);
        return NULL;
    }

    vec->len = 0;
    vec->capacity = VEC_INITIAL_CAPACITY;
    vec->elem_size = elem_size;
    return vec;
}

void wasd_vec_free(WasdVec* vec) {
    if (vec) {
        free(vec->data);
        free(vec);
    }
}

static void vec_grow(WasdVec* vec) {
    int64_t new_capacity = vec->capacity * 2;
    void* new_data = realloc(vec->data, new_capacity * vec->elem_size);
    if (new_data) {
        vec->data = new_data;
        vec->capacity = new_capacity;
    }
}

void wasd_vec_push(WasdVec* vec, void* element) {
    if (vec->len >= vec->capacity) {
        vec_grow(vec);
    }
    memcpy((char*)vec->data + vec->len * vec->elem_size, element, vec->elem_size);
    vec->len++;
}

bool wasd_vec_pop(WasdVec* vec, void* out) {
    if (vec->len == 0) return false;
    vec->len--;
    if (out) {
        memcpy(out, (char*)vec->data + vec->len * vec->elem_size, vec->elem_size);
    }
    return true;
}

int64_t wasd_vec_len(WasdVec* vec) {
    return vec ? vec->len : 0;
}

bool wasd_vec_is_empty(WasdVec* vec) {
    return vec ? (vec->len == 0) : true;
}

bool wasd_vec_get(WasdVec* vec, int64_t index, void* out) {
    if (!vec || index < 0 || index >= vec->len) return false;
    memcpy(out, (char*)vec->data + index * vec->elem_size, vec->elem_size);
    return true;
}

void wasd_vec_set(WasdVec* vec, int64_t index, void* element) {
    if (!vec || index < 0 || index >= vec->len) return;
    memcpy((char*)vec->data + index * vec->elem_size, element, vec->elem_size);
}

void wasd_vec_clear(WasdVec* vec) {
    if (vec) vec->len = 0;
}

int64_t wasd_vec_capacity(WasdVec* vec) {
    return vec ? vec->capacity : 0;
}

void wasd_vec_reserve(WasdVec* vec, int64_t additional) {
    if (!vec) return;
    int64_t needed = vec->len + additional;
    if (needed > vec->capacity) {
        int64_t new_capacity = vec->capacity;
        while (new_capacity < needed) {
            new_capacity *= 2;
        }
        void* new_data = realloc(vec->data, new_capacity * vec->elem_size);
        if (new_data) {
            vec->data = new_data;
            vec->capacity = new_capacity;
        }
    }
}

// i64 convenience functions
WasdVec* wasd_vec_new_i64(void) {
    return wasd_vec_new(sizeof(int64_t));
}

void wasd_vec_push_i64(WasdVec* vec, int64_t value) {
    wasd_vec_push(vec, &value);
}

int64_t wasd_vec_get_i64(WasdVec* vec, int64_t index) {
    int64_t value = 0;
    wasd_vec_get(vec, index, &value);
    return value;
}

void wasd_vec_set_i64(WasdVec* vec, int64_t index, int64_t value) {
    wasd_vec_set(vec, index, &value);
}

// ============== String Implementation ==============

#define STRING_INITIAL_CAPACITY 16

WasdString* wasd_string_new(void) {
    WasdString* str = (WasdString*)malloc(sizeof(WasdString));
    if (!str) return NULL;

    str->data = (char*)malloc(STRING_INITIAL_CAPACITY);
    if (!str->data) {
        free(str);
        return NULL;
    }

    str->data[0] = '\0';
    str->len = 0;
    str->capacity = STRING_INITIAL_CAPACITY;
    return str;
}

WasdString* wasd_string_from(const char* s) {
    if (!s) return wasd_string_new();

    size_t len = strlen(s);
    WasdString* str = (WasdString*)malloc(sizeof(WasdString));
    if (!str) return NULL;

    size_t capacity = len + 1;
    if (capacity < STRING_INITIAL_CAPACITY) {
        capacity = STRING_INITIAL_CAPACITY;
    }

    str->data = (char*)malloc(capacity);
    if (!str->data) {
        free(str);
        return NULL;
    }

    memcpy(str->data, s, len + 1);
    str->len = len;
    str->capacity = capacity;
    return str;
}

void wasd_string_free(WasdString* str) {
    if (str) {
        free(str->data);
        free(str);
    }
}

int64_t wasd_string_len(WasdString* str) {
    return str ? str->len : 0;
}

bool wasd_string_is_empty(WasdString* str) {
    return str ? (str->len == 0) : true;
}

static void string_grow(WasdString* str, int64_t needed) {
    if (needed <= str->capacity) return;

    int64_t new_capacity = str->capacity;
    while (new_capacity < needed) {
        new_capacity *= 2;
    }

    char* new_data = (char*)realloc(str->data, new_capacity);
    if (new_data) {
        str->data = new_data;
        str->capacity = new_capacity;
    }
}

void wasd_string_push(WasdString* str, char c) {
    if (!str) return;
    string_grow(str, str->len + 2);
    str->data[str->len] = c;
    str->len++;
    str->data[str->len] = '\0';
}

void wasd_string_push_str(WasdString* str, const char* s) {
    if (!str || !s) return;
    size_t s_len = strlen(s);
    string_grow(str, str->len + s_len + 1);
    memcpy(str->data + str->len, s, s_len + 1);
    str->len += s_len;
}

bool wasd_string_contains(WasdString* str, const char* needle) {
    if (!str || !needle) return false;
    return strstr(str->data, needle) != NULL;
}

bool wasd_string_starts_with(WasdString* str, const char* prefix) {
    if (!str || !prefix) return false;
    size_t prefix_len = strlen(prefix);
    if (prefix_len > (size_t)str->len) return false;
    return strncmp(str->data, prefix, prefix_len) == 0;
}

bool wasd_string_ends_with(WasdString* str, const char* suffix) {
    if (!str || !suffix) return false;
    size_t suffix_len = strlen(suffix);
    if (suffix_len > (size_t)str->len) return false;
    return strcmp(str->data + str->len - suffix_len, suffix) == 0;
}

WasdString* wasd_string_trim(WasdString* str) {
    if (!str || str->len == 0) return wasd_string_new();

    const char* start = str->data;
    const char* end = str->data + str->len - 1;

    while (start <= end && isspace((unsigned char)*start)) start++;
    while (end > start && isspace((unsigned char)*end)) end--;

    size_t new_len = end - start + 1;
    WasdString* result = (WasdString*)malloc(sizeof(WasdString));
    if (!result) return NULL;

    result->data = (char*)malloc(new_len + 1);
    if (!result->data) {
        free(result);
        return NULL;
    }

    memcpy(result->data, start, new_len);
    result->data[new_len] = '\0';
    result->len = new_len;
    result->capacity = new_len + 1;
    return result;
}

bool wasd_string_eq(WasdString* a, WasdString* b) {
    if (!a || !b) return false;
    if (a->len != b->len) return false;
    return strcmp(a->data, b->data) == 0;
}

int wasd_string_cmp(WasdString* a, WasdString* b) {
    if (!a && !b) return 0;
    if (!a) return -1;
    if (!b) return 1;
    return strcmp(a->data, b->data);
}

const char* wasd_string_as_ptr(WasdString* str) {
    return str ? str->data : "";
}

// ============== HashMap Implementation ==============

#define HASHMAP_INITIAL_CAPACITY 16
#define HASHMAP_LOAD_FACTOR 0.75

// FNV-1a hash function
static uint64_t fnv1a_hash(const void* key, size_t key_size) {
    const uint8_t* data = (const uint8_t*)key;
    uint64_t hash = 14695981039346656037ULL; // FNV offset basis

    for (size_t i = 0; i < key_size; i++) {
        hash ^= data[i];
        hash *= 1099511628211ULL; // FNV prime
    }

    return hash;
}

static bool default_eq(const void* a, const void* b, size_t size) {
    return memcmp(a, b, size) == 0;
}

WasdHashMap* wasd_hashmap_new(size_t key_size, size_t value_size) {
    WasdHashMap* map = (WasdHashMap*)malloc(sizeof(WasdHashMap));
    if (!map) return NULL;

    map->buckets = (WasdHashNode**)calloc(HASHMAP_INITIAL_CAPACITY, sizeof(WasdHashNode*));
    if (!map->buckets) {
        free(map);
        return NULL;
    }

    map->len = 0;
    map->capacity = HASHMAP_INITIAL_CAPACITY;
    map->key_size = key_size;
    map->value_size = value_size;
    return map;
}

static void free_nodes(WasdHashNode* node) {
    while (node) {
        WasdHashNode* next = node->next;
        free(node->key);
        free(node->value);
        free(node);
        node = next;
    }
}

void wasd_hashmap_free(WasdHashMap* map) {
    if (!map) return;

    for (int64_t i = 0; i < map->capacity; i++) {
        free_nodes(map->buckets[i]);
    }

    free(map->buckets);
    free(map);
}

static void hashmap_resize(WasdHashMap* map) {
    int64_t old_capacity = map->capacity;
    WasdHashNode** old_buckets = map->buckets;

    int64_t new_capacity = old_capacity * 2;
    WasdHashNode** new_buckets = (WasdHashNode**)calloc(new_capacity, sizeof(WasdHashNode*));
    if (!new_buckets) return;

    map->buckets = new_buckets;
    map->capacity = new_capacity;
    map->len = 0;

    for (int64_t i = 0; i < old_capacity; i++) {
        WasdHashNode* node = old_buckets[i];
        while (node) {
            WasdHashNode* next = node->next;

            int64_t idx = node->hash % new_capacity;
            node->next = new_buckets[idx];
            new_buckets[idx] = node;
            map->len++;

            node = next;
        }
    }

    free(old_buckets);
}

bool wasd_hashmap_insert(WasdHashMap* map, const void* key, const void* value, void* old_value) {
    if (!map || !key) return false;

    // Check if we need to resize
    if ((double)map->len / map->capacity >= HASHMAP_LOAD_FACTOR) {
        hashmap_resize(map);
    }

    uint64_t hash = fnv1a_hash(key, map->key_size);
    int64_t idx = hash % map->capacity;

    // Check if key exists
    WasdHashNode* node = map->buckets[idx];
    while (node) {
        if (node->hash == hash && default_eq(node->key, key, map->key_size)) {
            // Key exists, update value
            if (old_value) {
                memcpy(old_value, node->value, map->value_size);
            }
            memcpy(node->value, value, map->value_size);
            return true;
        }
        node = node->next;
    }

    // Key doesn't exist, create new node
    WasdHashNode* new_node = (WasdHashNode*)malloc(sizeof(WasdHashNode));
    if (!new_node) return false;

    new_node->key = malloc(map->key_size);
    new_node->value = malloc(map->value_size);
    if (!new_node->key || !new_node->value) {
        free(new_node->key);
        free(new_node->value);
        free(new_node);
        return false;
    }

    memcpy(new_node->key, key, map->key_size);
    memcpy(new_node->value, value, map->value_size);
    new_node->hash = hash;
    new_node->next = map->buckets[idx];
    map->buckets[idx] = new_node;
    map->len++;

    return false; // No old value
}

bool wasd_hashmap_get(WasdHashMap* map, const void* key, void* out_value) {
    if (!map || !key) return false;

    uint64_t hash = fnv1a_hash(key, map->key_size);
    int64_t idx = hash % map->capacity;

    WasdHashNode* node = map->buckets[idx];
    while (node) {
        if (node->hash == hash && default_eq(node->key, key, map->key_size)) {
            if (out_value) {
                memcpy(out_value, node->value, map->value_size);
            }
            return true;
        }
        node = node->next;
    }

    return false;
}

bool wasd_hashmap_contains_key(WasdHashMap* map, const void* key) {
    return wasd_hashmap_get(map, key, NULL);
}

bool wasd_hashmap_remove(WasdHashMap* map, const void* key, void* out_value) {
    if (!map || !key) return false;

    uint64_t hash = fnv1a_hash(key, map->key_size);
    int64_t idx = hash % map->capacity;

    WasdHashNode* prev = NULL;
    WasdHashNode* node = map->buckets[idx];

    while (node) {
        if (node->hash == hash && default_eq(node->key, key, map->key_size)) {
            if (out_value) {
                memcpy(out_value, node->value, map->value_size);
            }

            if (prev) {
                prev->next = node->next;
            } else {
                map->buckets[idx] = node->next;
            }

            free(node->key);
            free(node->value);
            free(node);
            map->len--;
            return true;
        }
        prev = node;
        node = node->next;
    }

    return false;
}

int64_t wasd_hashmap_len(WasdHashMap* map) {
    return map ? map->len : 0;
}

bool wasd_hashmap_is_empty(WasdHashMap* map) {
    return map ? (map->len == 0) : true;
}

void wasd_hashmap_clear(WasdHashMap* map) {
    if (!map) return;

    for (int64_t i = 0; i < map->capacity; i++) {
        free_nodes(map->buckets[i]);
        map->buckets[i] = NULL;
    }
    map->len = 0;
}

int64_t wasd_hashmap_capacity(WasdHashMap* map) {
    return map ? map->capacity : 0;
}

// i64 convenience functions
WasdHashMap* wasd_hashmap_new_i64(void) {
    return wasd_hashmap_new(sizeof(int64_t), sizeof(int64_t));
}

void wasd_hashmap_insert_i64(WasdHashMap* map, int64_t key, int64_t value) {
    wasd_hashmap_insert(map, &key, &value, NULL);
}

int64_t wasd_hashmap_get_i64(WasdHashMap* map, int64_t key, int64_t default_value) {
    int64_t value;
    if (wasd_hashmap_get(map, &key, &value)) {
        return value;
    }
    return default_value;
}

bool wasd_hashmap_contains_i64(WasdHashMap* map, int64_t key) {
    return wasd_hashmap_contains_key(map, &key);
}
