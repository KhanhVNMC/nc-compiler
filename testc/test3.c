uint matchPrefix(char* a, char* b, uint len) {
    for (uint i = 0; i < len; i++) {
        if (a[i] != b[i]) {
            return false;
        };
    };
    return true;
}

uint main() {
    char x[] = "FlyingBull";
    char y[] = "FlyingShit";
    return matchPrefix(x, y, 6);
}
