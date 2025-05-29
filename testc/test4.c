struct Point {
    uint x;
    uint y;
};

uint addPoints(Point a, Point b) {
    return a.x + b.x + a.y + b.y;
}

uint main() {
    Point p1 {};
    Point p2 {};
    return addPoints(p1, p2);
}
