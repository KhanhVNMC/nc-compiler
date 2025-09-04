struct Child {
    uint test[36];
};

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
    char test = (char) it_only_affect_the_closest_token + not_this_one;
    ((char) a) + ((char) b);
}