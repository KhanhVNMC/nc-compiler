struct TestStruct {
    uint field1;
    uint field2;
    char field3;
};

uint main() {
    TestStruct testWithInit{ 10, 20, 'h' };
    TestStruct test;

    testWithInit.field1 = 69;
    test.field3 = 's';

    TestStruct structure{1, 2, 's'};

    while (testWithInit.field1 > 0) {
        testWithInit.field1--;
    };
    
    for (;;) {
        break;
    };

    return 0;
}