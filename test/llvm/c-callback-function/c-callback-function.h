typedef int(*pcallback)(int);
typedef int(callback)(int);

void foo(pcallback cb);
void bar(callback* cb);
void baz(int(*cb)(int));
