#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char* sending_application;
    char* sending_facility;
    char* receiving_application;
    char* receiving_facility;
    char* datetime;
    char* security;
    char* message_type;
} MSH_Segment;

typedef struct {
    char* patient_id;
    char* patient_name;
    char* mother_maiden_name;
    char* date_of_birth;
    char* gender;
    char* patient_address;
    char* phone_number;
} PID_Segment;

typedef struct {
    MSH_Segment* msh;
    PID_Segment* pid;
} HL7_Message;

HParsedToken* parse_msh_segment(void* data) {
    HParser* msh_parser = h_sequence(
        h_token("|", 1),
        h_many(h_ch_range('A', 'Z')),
        h_many(h_ch_range('A', 'Z')),
        h_many(h_ch_range('A', 'Z')),
        h_many(h_ch_range('A', 'Z')),
        h_many(h_ch_range('0', '9')),
        h_end_p()
    );
    return h_parse(msh_parser, data, strlen(data));
}

HParsedToken* parse_pid_segment(void* data) {
    HParser* pid_parser = h_sequence(
        h_token("PID", 3),
        h_many(h_ch_range('0', '9')),
        h_many(h_ch_range('A', 'Z')),
        h_end_p()
    );
    return h_parse(pid_parser, data, strlen(data));
}

HL7_Message* parse_hl7_message(char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    char* buffer = malloc(file_size + 1);
    fread(buffer, 1, file_size, file);
    buffer[file_size] = '\0';

    HL7_Message* message = malloc(sizeof(HL7_Message));
    message->msh = malloc(sizeof(MSH_Segment));
    message->pid = malloc(sizeof(PID_Segment));

    HParsedToken* msh_result = parse_msh_segment(buffer);
    HParsedToken* pid_result = parse_pid_segment(buffer);

    fclose(file);
    free(buffer);

    return message;
}

void free_hl7_message(HL7_Message* message) {
    free(message->msh);
    free(message->pid);
    free(message);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    HL7_Message* parsed_message = parse_hl7_message(argv[1]);
    if (!parsed_message) {
        return 1;
    }

    free_hl7_message(parsed_message);
    return 0;
}