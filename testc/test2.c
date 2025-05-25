uint sumArray(uint* arr, uint size) {
    uint sum = 0;
    for (uint i = 0; i < size; i++) {
        sum = sum + arr[i];
    };
    return sum;
}

uint main() {
    uint values[] = {1, 2, 3, 4, 5};
    return sumArray(values, 5);
}
