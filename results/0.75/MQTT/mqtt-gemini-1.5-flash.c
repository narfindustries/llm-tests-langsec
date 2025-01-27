#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent a hammer
typedef struct {
    char* name;
    float weight;
    int length;
} Hammer;

// Function to create a hammer
Hammer* createHammer(const char* name, float weight, int length) {
    Hammer* hammer = (Hammer*)malloc(sizeof(Hammer));
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


// Function to free the memory allocated for a hammer
void freeHammer(Hammer* hammer) {
    free(hammer->name);
    free(hammer);
}

int main() {
    // Create a hammer
    Hammer* myHammer = createHammer("My Hammer", 1.5, 12);

    // Print hammer details
    printf("Hammer Name: %s\n", myHammer->name);
    printf("Hammer Weight: %.2f\n", myHammer->weight);
    printf("Hammer Length: %d\n", myHammer->length);

    // Free the dynamically allocated memory
    freeHammer(myHammer);

    return 0;
}
