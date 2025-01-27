#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent an HL7 message segment
typedef struct {
    char *segment_id;
    char **fields;
    int num_fields;
} HL7Segment;

// Function to parse an HL7 message segment
HL7Segment *parse_segment(char *line) {
    HL7Segment *segment = (HL7Segment *)malloc(sizeof(HL7Segment));
    if (segment == NULL) {
        perror("Memory allocation failed");
        exit(1);
    }

    segment->segment_id = strtok(line, "|");
    segment->fields = (char **)malloc(sizeof(char *) * 100); // Adjust as needed
    if (segment->fields == NULL) {
        perror("Memory allocation failed");
        exit(1);
    }
    segment->num_fields = 0;

    char *field = strtok(NULL, "|");
    while (field != NULL) {
        segment->fields[segment->num_fields++] = strdup(field);
        field = strtok(NULL, "|");
    }

    return segment;
}


// Function to free memory allocated for an HL7 segment
void free_segment(HL7Segment *segment) {
    for (int i = 0; i < segment->num_fields; i++) {
        free(segment->fields[i]);
    }
    free(segment->fields);
    free(segment);
}


int main() {
    // Example HL7 message (replace with your actual message)
    char *hl7_message = "MSH|^~\\&|ADT^A01|Hospital|Hospital|202310271000||ADT^A01|12345|P|2.5\rPID|12345|PatientName|1234567890\r";


    char *line = strtok(hl7_message, "\r");
    while (line != NULL) {
        HL7Segment *segment = parse_segment(line);
        printf("Segment ID: %s\n", segment->segment_id);
        for (int i = 0; i < segment->num_fields; i++) {
            printf("Field %d: %s\n", i + 1, segment->fields[i]);
        }
        free_segment(segment);
        line = strtok(NULL, "\r");
    }

    return 0;
}
