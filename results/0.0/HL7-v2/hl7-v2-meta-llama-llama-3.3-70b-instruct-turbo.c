#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the HL7 v2 message structure
typedef struct {
    char message_type[3];
    char trigger_event[3];
    char message_control_id[20];
    char processing_id[3];
    char version_id[12];
    char sequence_number[15];
    char continuation_pointer[180];
} HL7_Message;

// Define the HL7 v2 segment structure
typedef struct {
    char segment_id[3];
    char fields[200][200];
    int field_count;
} HL7_Segment;

// Define the HL7 v2 field structure
typedef struct {
    char field_id[4];
    char value[200];
} HL7_Field;

// Function to parse an HL7 v2 message
HL7_Message* parse_hl7_message(char* message) {
    HL7_Message* msg = (HL7_Message*) malloc(sizeof(HL7_Message));
    // Parse the message type
    strncpy(msg->message_type, message, 3);
    // Parse the trigger event
    strncpy(msg->trigger_event, message + 3, 3);
    // Parse the message control ID
    strncpy(msg->message_control_id, message + 6, 20);
    // Parse the processing ID
    strncpy(msg->processing_id, message + 26, 3);
    // Parse the version ID
    strncpy(msg->version_id, message + 29, 12);
    // Parse the sequence number
    strncpy(msg->sequence_number, message + 41, 15);
    // Parse the continuation pointer
    strncpy(msg->continuation_pointer, message + 56, 180);
    return msg;
}

// Function to parse an HL7 v2 segment
HL7_Segment* parse_hl7_segment(char* segment) {
    HL7_Segment* seg = (HL7_Segment*) malloc(sizeof(HL7_Segment));
    // Parse the segment ID
    strncpy(seg->segment_id, segment, 3);
    // Parse the fields
    char* field_ptr = segment + 3;
    int field_count = 0;
    while (*field_ptr != '\0') {
        // Parse the field value
        char* field_value_ptr = strchr(field_ptr, '|');
        if (field_value_ptr == NULL) {
            break;
        }
        int field_value_len = field_value_ptr - field_ptr;
        strncpy(seg->fields[field_count], field_ptr, field_value_len);
        seg->fields[field_count][field_value_len] = '\0';
        field_ptr = field_value_ptr + 1;
        field_count++;
    }
    seg->field_count = field_count;
    return seg;
}

// Function to parse an HL7 v2 field
HL7_Field* parse_hl7_field(char* field) {
    HL7_Field* fld = (HL7_Field*) malloc(sizeof(HL7_Field));
    // Parse the field ID
    strncpy(fld->field_id, field, 4);
    // Parse the field value
    char* field_value_ptr = strchr(field, ':');
    if (field_value_ptr == NULL) {
        return NULL;
    }
    int field_value_len = strlen(field) - (field_value_ptr - field) - 1;
    strncpy(fld->value, field_value_ptr + 1, field_value_len);
    fld->value[field_value_len] = '\0';
    return fld;
}

int main() {
    // Example usage:
    char* hl7_message = "MSH|^~\\&|SendingApp|SendingFacility|ReceivingApp|ReceivingFacility|20220101|1134||ORU^R01|12345|P|2.3|||AL|\n";
    HL7_Message* msg = parse_hl7_message(hl7_message);
    printf("Message Type: %s\n", msg->message_type);
    printf("Trigger Event: %s\n", msg->trigger_event);
    printf("Message Control ID: %s\n", msg->message_control_id);
    printf("Processing ID: %s\n", msg->processing_id);
    printf("Version ID: %s\n", msg->version_id);
    printf("Sequence Number: %s\n", msg->sequence_number);
    printf("Continuation Pointer: %s\n", msg->continuation_pointer);

    char* hl7_segment = "PID|1||12345||Doe^John||19600101|M|||123 Main St^^Anytown^CA^12345";
    HL7_Segment* seg = parse_hl7_segment(hl7_segment);
    printf("Segment ID: %s\n", seg->segment_id);
    for (int i = 0; i < seg->field_count; i++) {
        printf("Field %d: %s\n", i + 1, seg->fields[i]);
    }

    char* hl7_field = "PID.1:12345";
    HL7_Field* fld = parse_hl7_field(hl7_field);
    printf("Field ID: %s\n", fld->field_id);
    printf("Field Value: %s\n", fld->value);

    return 0;
}