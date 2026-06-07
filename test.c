int foo() {
    printf("foo\n");
    return 1;
}

int main() {
    struct {char a; char b;} x[3];
    char *p=x;
    p;
    x[0].a = 99;
    printf("hello %d!!!!\n", x[0].a);
    int a = foo();
    printf("foo return value:%d\n", foo());
    return 0;
}