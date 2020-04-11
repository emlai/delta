enum {
    FooA = -1,
    FooB = 0x7FFFFFFF
};

enum Bar {
    BarA,
    BarB = 0x7FFFFFFF
};

typedef enum : unsigned long long {
    QuxA,
    QuxB = 0xFFFFFFFFFFFFFFFF
} Qux;
