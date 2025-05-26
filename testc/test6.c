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

    return 0;
}