#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent an HL7 message segment
typedef struct {
    char *segmentID;
    char *fields[100]; // Adjust size as needed
} HL7Segment;

// Function to parse an HL7 message (simplified for demonstration)
HL7Segment* parseHL7Segment(char *line) {
    HL7Segment *segment = (HL7Segment*)malloc(sizeof(HL7Segment));
    if (segment == NULL) {
        perror("Memory allocation failed");
        exit(1);
    }
    
    segment->segmentID = strtok(line, "|");
    int i = 0;
    char *token;
    while ((token = strtok(NULL, "|")) != NULL && i < 100) {
        segment->fields[i++] = strdup(token); // Duplicate to avoid modifying original string
    }
    return segment;
}


// Function to free allocated memory for an HL7 segment.
void freeHL7Segment(HL7Segment* segment) {
    if (segment == NULL) return;
    free(segment->segmentID);
    for (int i = 0; segment->fields[i] != NULL; i++) {
        free(segment->fields[i]);
    }
    free(segment);
}


int main() {
    char hl7Message[] = "MSH|^~\\&|SendingApp|SendingFacility|ReceivingApp|ReceivingFacility|202310271000||ORU^R01|12345|P|2.3|||"; // Example HL7 message

    char *line = strdup(hl7Message); // Create a copy to avoid modifying the original string

    HL7Segment *segment = parseHL7Segment(line);
    if (segment == NULL){
        return 1;
    }

    printf("Segment ID: %s\n", segment->segmentID);
    for (int i = 0; segment->fields[i] != NULL; i++) {
        printf("Field %d: %s\n", i + 1, segment->fields[i]);
    }

    freeHL7Segment(segment);
    free(line);

    return 0;
}
