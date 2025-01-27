#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>


//Structure to represent a single hammer
typedef struct {
    char* name;
    int weight; //in grams
    int length; //in centimeters
    int hardness; //some arbitrary hardness scale
} Hammer;


// Function to create a hammer
Hammer* createHammer(const char* name, int weight, int length, int hardness) {
    Hammer* newHammer = (Hammer*)malloc(sizeof(Hammer));
    if (newHammer == NULL) {
        perror("Memory allocation failed");
        exit(1);
    }
    newHammer->name = strdup(name);
    if (newHammer->name == NULL) {
        perror("Memory allocation failed");
        free(newHammer);
        exit(1);
    }
    newHammer->weight = weight;
    newHammer->length = length;
    newHammer->hardness = hardness;
    return newHammer;
}

// Function to free the memory allocated for a hammer
void freeHammer(Hammer* hammer) {
    free(hammer->name);
    free(hammer);
}


int main() {
    // Example usage
    Hammer* myHammer = createHammer("My Hammer", 500, 30, 8);

    printf("Hammer Name: %s\n", myHammer->name);
    printf("Hammer Weight: %d grams\n", myHammer->weight);
    printf("Hammer Length: %d cm\n", myHammer->length);
    printf("Hammer Hardness: %d\n", myHammer->hardness);


    freeHammer(myHammer);
    return 0;
}
