uint main() {
    uint a = 1;
    {
        uint b = 2;
        {
            uint c = a + b;
        };
    };
    return 0;
}
