// Demo


// We don't need to declare a function before calling it.
// int foo();
int main() {
    int foofoo = foo();
    printf("returned_value=%d\n", foofoo);

    struct zxc (*zxcp);
    typedef struct coord {
        int x;
        int y;
        char identifier;
    } coord;

    coord location;
    coord *location_pointer = &location;
    location.x = 11;
    location.y = 22;
    // When we have a strcut pointer, we don't need to use '->' to access its struct members, all just use '.'.
    location_pointer.x = 12345;
    printf("location.x=%d\n", location.x);
    printf("location.y=%d\n", location.y);

    typedef int t;
    t var;
    printf("sizeof var is %ld\n", sizeof(var));

    int i = 0;
    while (1) {
        printf("i=%d\n", i);
        i = i + 1;
        if (i == 4) {
            return 0;
        }
        sleep(1);
    // @CompilerBug: If without the RBrace, compiler will trap in an infinite loop.
    }
    return 0;
}

int foo() {
    printf("foo being called.\n");
    return 88;
}
