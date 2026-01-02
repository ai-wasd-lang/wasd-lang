// WASD Runtime Library Header
// Provides runtime support for Vec, String, HashMap

#ifndef WASD_RUNTIME_H
#define WASD_RUNTIME_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

// ============== Vec (Dynamic Array) ==============

typedef struct {
    void* data;        // Pointer to element data
    int64_t len;       // Number of elements
    int64_t capacity;  // Allocated capacity
    size_t elem_size;  // Size of each element
} WasdVec;

// Create a new vector for elements of given size
WasdVec* wasd_vec_new(size_t elem_size);
void wasd_vec_free(WasdVec* vec);
void wasd_vec_push(WasdVec* vec, void* element);
bool wasd_vec_pop(WasdVec* vec, void* out);
int64_t wasd_vec_len(WasdVec* vec);
bool wasd_vec_is_empty(WasdVec* vec);
bool wasd_vec_get(WasdVec* vec, int64_t index, void* out);
void wasd_vec_set(WasdVec* vec, int64_t index, void* element);
void wasd_vec_clear(WasdVec* vec);
int64_t wasd_vec_capacity(WasdVec* vec);
void wasd_vec_reserve(WasdVec* vec, int64_t additional);

// Convenience functions for i64 vectors
WasdVec* wasd_vec_new_i64(void);
void wasd_vec_push_i64(WasdVec* vec, int64_t value);
int64_t wasd_vec_get_i64(WasdVec* vec, int64_t index);
void wasd_vec_set_i64(WasdVec* vec, int64_t index, int64_t value);

// ============== String ==============

typedef struct {
    char* data;        // Null-terminated string data
    int64_t len;       // Length (excluding null terminator)
    int64_t capacity;  // Allocated capacity
} WasdString;

WasdString* wasd_string_new(void);
WasdString* wasd_string_from(const char* s);
void wasd_string_free(WasdString* str);
int64_t wasd_string_len(WasdString* str);
bool wasd_string_is_empty(WasdString* str);
void wasd_string_push(WasdString* str, char c);
void wasd_string_push_str(WasdString* str, const char* s);
bool wasd_string_contains(WasdString* str, const char* needle);
bool wasd_string_starts_with(WasdString* str, const char* prefix);
bool wasd_string_ends_with(WasdString* str, const char* suffix);
WasdString* wasd_string_trim(WasdString* str);
bool wasd_string_eq(WasdString* a, WasdString* b);
int wasd_string_cmp(WasdString* a, WasdString* b);
const char* wasd_string_as_ptr(WasdString* str);

// ============== HashMap ==============

typedef struct WasdHashNode {
    uint64_t hash;
    void* key;
    void* value;
    struct WasdHashNode* next;
} WasdHashNode;

typedef struct {
    WasdHashNode** buckets;  // Array of bucket pointers
    int64_t len;             // Number of entries
    int64_t capacity;        // Number of buckets
    size_t key_size;         // Size of each key
    size_t value_size;       // Size of each value
} WasdHashMap;

// Hash function type (returns 64-bit hash)
typedef uint64_t (*WasdHashFn)(const void* key, size_t key_size);
// Equality function type
typedef bool (*WasdEqFn)(const void* a, const void* b, size_t size);

WasdHashMap* wasd_hashmap_new(size_t key_size, size_t value_size);
void wasd_hashmap_free(WasdHashMap* map);
bool wasd_hashmap_insert(WasdHashMap* map, const void* key, const void* value, void* old_value);
bool wasd_hashmap_get(WasdHashMap* map, const void* key, void* out_value);
bool wasd_hashmap_contains_key(WasdHashMap* map, const void* key);
bool wasd_hashmap_remove(WasdHashMap* map, const void* key, void* out_value);
int64_t wasd_hashmap_len(WasdHashMap* map);
bool wasd_hashmap_is_empty(WasdHashMap* map);
void wasd_hashmap_clear(WasdHashMap* map);
int64_t wasd_hashmap_capacity(WasdHashMap* map);

// Convenience functions for i64 -> i64 hashmaps
WasdHashMap* wasd_hashmap_new_i64(void);
void wasd_hashmap_insert_i64(WasdHashMap* map, int64_t key, int64_t value);
int64_t wasd_hashmap_get_i64(WasdHashMap* map, int64_t key, int64_t default_value);
bool wasd_hashmap_contains_i64(WasdHashMap* map, int64_t key);

#endif // WASD_RUNTIME_H
