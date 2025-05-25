struct Person {
    char name[100];
    uint age;
    uint weight;
    uint height;
};

Person createPerson(
    char* name, uint lname, 
    uint age, uint weight, uint height
) {
    Person p;
    for (uint i = 0; i < lname; i++) {
        p->name[i] = name[i];
    };
    p.weight = weight;
    p.height = height;
    p.age = age;
    return p;
}

uint main() {
    char name[10];
    createPerson(name, 10, 12, 45, 150);
    return 0;
}