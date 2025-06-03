struct SizeOf {
    char c;
};

struct Rotation {
    SizeOf size;
    uint rot;
};

struct Point {
    uint x;
    Rotation rot;
    uint y;
};

struct Player {
    Point p[20];
    uint hp[100];
    Point sp;
    void* s;
};