int main() {
    struct st1 {
        int a;
        char b;
    };
    struct st1 qwer;
    struct st1 *qwer_pointer = &qwer;
    qwer.a = 11;
    qwer.b = 22;
    qwer_pointer.a = 12345;
    printf("qwer.a=%d\n", qwer.a);
    printf("qwer.b=%d\n", qwer.b);

    int i = 0;
    while (1) {
        printf("i=%d\n", i);
        i = i + 1;
        sleep(1);
    }
}

int foo() {
    printf("foo being called.\n");
    return 1;
}