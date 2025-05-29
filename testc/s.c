void memcpy(void* src, void* dest, uint len);

uint a = 10;

void func() {
    char s[] = "sex";
    char ch = s[false];
    print("hello");
};

void memcpy(void* s, void* d, uint l) {
    for (uint i = 0; i < l; ++i) {
        *(d + i) = *(s + i);
    };
}