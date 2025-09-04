struct Child {
    uint test[36];
};

int func();

struct Parent {
    Child c;
    Child c2;
    Child c3;
    uint test;
};

struct Compound {
    Parent p;
    Parent p2;
};

uint main() {
    uint a = 1;
}