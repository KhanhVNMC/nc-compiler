struct Address {
    char street[50];
    char city[30];
    uint zipCode;
};

struct Person {
    char name[100];
    uint age;
    uint weight;
    uint height;
    Address homeAddress;
    char phoneNumbers[45];
};

Person globalPerson;
Person globalPersonZeroInitialized{};

uint main() {
    char rodata_string[] = "User info: %s, Age: %u";

    Person user{};

    user.name[0] = 'J';
    user.name[1] = 'o';
    user.name[2] = 'h';
    user.name[3] = 'n';
    user.name[4] = '\0';

    user.age = 30;
    user.weight = 75;
    user.height = 180;

    char streetName[] = "1234 Elm St";
    for (uint i = 0; i < 11; i++) {
        user.homeAddress.street[i] = streetName[i];
    };

    char cityName[] = "Springfield";
    for (uint i = 0; i < 11; i++) {
        user.homeAddress.city[i] = cityName[i];
    };

    user.homeAddress.zipCode = 12345;

    char phone1[] = "555-1234";
    char phone2[] = "555-5678";
    char phone3[] = "555-8765";

    for (uint i = 0; i < 9; i++) {
        user.phoneNumbers[0 * 15 + i] = phone1[i];
    };

    for (uint i = 0; i < 9; i++) {
        user.phoneNumbers[1 * 15 + i] = phone2[i];
    };

    for (uint i = 0; i < 9; i++) {
        user.phoneNumbers[2 * 15 + i] = phone3[i];
    };

    printf(rodata_string, user.name, user.age);

    return 0;
}