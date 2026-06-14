// Demo




// int foo();

int main() {
    struct coord {
        int x;
        int y;
        char identifier;
    };
    struct coord location;
    struct coord *location_pointer = &location;
    location.x = 11;
    location.y = 22;
    printf("before: location.x=%d\n", location.x);
    location_pointer.x = 12345;
    printf("after: location.x=%d\n", location.x);
    printf("location.y=%d\n", location.y);

    int i = 0;
    while (1) {
        foo();
        printf("i=%d\n", i);
        i = i + 1;
        sleep(1);
    }
    return 0;
}

int foo() {
    printf("foo being called.\n");
    return 1;
}