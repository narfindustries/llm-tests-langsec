#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Structure to represent a hammer
typedef struct {
    char* name;
    float weight;
    int head_size;
} Hammer;


// Function to create a hammer
Hammer* createHammer(const char* name, float weight, int head_size) {
    Hammer* hammer = (Hammer*)malloc(sizeof(Hammer));
    if (hammer == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    hammer->name = (char*)malloc(strlen(name) + 1);
    if (hammer->name == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        free(hammer);
        exit(1);
    }
    strcpy(hammer->name, name);
    hammer->weight = weight;
    hammer->head_size = head_size;
    return hammer;
}


// Function to print hammer information
void printHammer(const Hammer* hammer) {
    printf("Hammer Name: %s\n", hammer->name);
    printf("Hammer Weight: %.2f kg\n", hammer->weight);
    printf("Hammer Head Size: %d cm\n", hammer->head_size);
}


// Function to free the dynamically allocated memory
void freeHammer(Hammer* hammer) {
    free(hammer->name);
    free(hammer);
}

int main() {
    // Example usage
    Hammer* myHammer = createHammer("My Hammer", 1.5, 10);
    printHammer(myHammer);
    freeHammer(myHammer);
    return 0;
}
