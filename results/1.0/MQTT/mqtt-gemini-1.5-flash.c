#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a hammer
typedef struct {
    char *name;
    float weight;
    int length;
} Hammer;


// Function to create a hammer
Hammer* createHammer(const char *name, float weight, int length) {
    Hammer *hammer = (Hammer*)malloc(sizeof(Hammer));
    if (hammer == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    hammer->name = strdup(name);
    if (hammer->name == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        free(hammer);
        exit(1);
    }
    hammer->weight = weight;
    hammer->length = length;
    return hammer;
}

// Function to print hammer information.
void printHammer(const Hammer *hammer) {
    if (hammer == NULL) {
        printf("NULL hammer pointer\n");
        return;
    }
    printf("Hammer Name: %s\n", hammer->name);
    printf("Hammer Weight: %.2f\n", hammer->weight);
    printf("Hammer Length: %d\n", hammer->length);
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
    Hammer *myHammer = createHammer("My Hammer", 2.5, 12);

    // Print hammer information
    printHammer(myHammer);

    //Free the hammer
    freeHammer(myHammer);


    return 0;
}
