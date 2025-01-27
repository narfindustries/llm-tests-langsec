#include <stdio.h>
#include <stdlib.h>

// Structure to represent a hammer
typedef struct {
    char *name;
    float weight;
    float length;
} Hammer;

// Function to create a hammer
Hammer* createHammer(char *name, float weight, float length) {
    Hammer *hammer = (Hammer*)malloc(sizeof(Hammer));
    if (hammer == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    hammer->name = name;
    hammer->weight = weight;
    hammer->length = length;
    return hammer;
}

// Function to print hammer information
void printHammer(Hammer *hammer) {
    printf("Hammer Name: %s\n", hammer->name);
    printf("Hammer Weight: %.2f kg\n", hammer->weight);
    printf("Hammer Length: %.2f cm\n", hammer->length);
}


int main() {
    // Create a hammer
    Hammer *myHammer = createHammer("My Hammer", 1.5, 30.0);

    // Print hammer information
    printHammer(myHammer);

    // Free dynamically allocated memory
    free(myHammer);

    return 0;
}
