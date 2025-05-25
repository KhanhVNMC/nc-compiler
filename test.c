uint main() {
    uint a = 10;
    uint* aPtr = &a;

    for (uint i = 0; i < 1000; i++) {
        a++;
    };

    test(*aPtr);

    return 0;
};