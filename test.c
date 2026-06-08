int main() {
    struct st1 {
        int a;
        int b;
        char c;
    };
    struct st1 qwer;
    struct st1 *qwer_pointer = &qwer;
    qwer.a = 11;
    qwer.b = 22;
    qwer.c = 33;
    qwer_pointer.a = 12345;
    printf("qwer.a=%d\n", qwer.a);
    printf("qwer.b=%d\n", qwer.b);
    printf("qwer.c=%d\n", qwer.c);

    struct {char a; char b;} x[3];
    char *p=x;
    x[0].a = 99;
    printf("x[0].a is %d!!!!\n", x[0].a);
    foo();
    printf("value:%d\n", 123);
    return 0;
}

int foo() {
    printf("foo being called.\n");
    return 1;
}