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
        int x = 10;
        testWithInit.field1--;
    };
    
    {
        int y = 10;
    }

    return 0;
}