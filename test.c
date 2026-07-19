// Demo
int printf();
int sleep(int a);


// We can just call any function in the same file without forward declaring it.
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

    int arr[3];
    arr[0] = 100;
    arr[1] = 101;
    arr[2] = 102;
    int index = 0;

    printf("arr element=%d\n", arr[index=2]);

    // @CompilerBug: Uncomment this to see a compile error report (which shouldn't
    // be a error). This is all about precedence stuff.
    // -arr[2];

    int i = 0;
    while (1) {
        printf("i=%d\n", i);
        i = i + 1;
        if (i == 4) {
            return 0;
        }
        sleep(1);
    }
    return 0;
}

int foo() {
    printf("foo being called.\n");
    return 88;
}
