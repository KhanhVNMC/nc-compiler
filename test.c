uint main() {
    uint a = 10;
    uint b = 11;
    uint* aPtr = &a;
    uint** aaPtr = &aPtr;

    **aaPtr = 12;

    **(&p + 1) = 1;

    if (a > 10 && true) {
        do_thing2(1+1, &a, &b);
        do_thing3();
    } else if (b >= 5 && a > 20) {
        do_thing1();
        while (true) {
            test();
        };
    } else test_func();
};