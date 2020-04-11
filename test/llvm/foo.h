struct Foo {
    signed bar;
    const char* baz;
};

inline unsigned getBar(const struct Foo* foo) {
    return foo->bar;
}

typedef enum {
    MAGIC_NUMBER = 5
} Qux;

#define ANSWER 42
