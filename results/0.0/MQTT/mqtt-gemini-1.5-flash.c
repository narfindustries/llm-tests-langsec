#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a hammer
typedef struct {
    char *name;
    float weight;
    int headSize;
} Hammer;


// Function to create a hammer
Hammer* createHammer(const char *name, float weight, int headSize) {
    Hammer *hammer = (Hammer*)malloc(sizeof(Hammer));
    if (hammer == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        return NULL;
    }
    hammer->name = strdup(name);
    if (hammer->name == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        free(hammer);
        return NULL;
    }
    hammer->weight = weight;
    hammer->headSize = headSize;
    return hammer;
}

// Function to print hammer information
void printHammer(const Hammer *hammer) {
    if (hammer == NULL) {
        fprintf(stderr, "Hammer is NULL\n");
        return;
    }
    printf("Hammer Name: %s\n", hammer->name);
    printf("Hammer Weight: %.2f\n", hammer->weight);
    printf("Hammer Head Size: %d\n", hammer->headSize);
}


// Function to free the dynamically allocated memory for a hammer.
void freeHammer(Hammer *hammer) {
    if (hammer != NULL) {
        free(hammer->name);
        free(hammer);
    }
}


int main() {
    // Create a hammer
    Hammer *myHammer = createHammer("My Hammer", 1.5, 10);

    // Check if hammer creation was successful.
    if (myHammer == NULL) {
        return 1; // Indicate an error.
    }

    // Print hammer information
    printHammer(myHammer);

    // Free the dynamically allocated memory.
    freeHammer(myHammer);

    return 0;
}
