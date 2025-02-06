#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char* set_id;
    char* patient_id;
    char* patient_identifier_list;
    char* alternate_patient_id;
    char* patient_name;
    char* mothers_maiden_name;
    char* date_of_birth;
    char* sex;
    char* patient_alias;
} PIDSegment;

typedef struct {
    char* sending_application;
    char* sending_facility;
    char* receiving_application;
    char* receiving_facility;
    char* datetime;
    char* security;
    char* message_type;
} MSHSegment;

typedef struct {
    MSHSegment* msh;
    PIDSegment* pid;
} HL7Message;

HParser* create_msh_parser() {
    HParser* not_pipe = h_ch_range(0x01, '|' - 1);
    return h_sequence(
        h_token_str("MSH"),
        h_ch('|'),
        h_many(not_pipe),
        NULL
    );
}

HParser* create_pid_parser() {
    HParser* not_pipe = h_ch_range(0x01, '|' - 1);
    return h_sequence(
        h_token_str("PID"),
        h_ch('|'),
        h_many(not_pipe),
        NULL
    );
}

HL7Message* parse_hl7_message(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    fread(buffer, 1, file_size, file);

    HParser* msh_parser = create_msh_parser();
    HParser* pid_parser = create_pid_parser();

    HParseResult* msh_result = h_parse(msh_parser, buffer, file_size);
    HParseResult* pid_result = h_parse(pid_parser, buffer, file_size);

    HL7Message* message = malloc(sizeof(HL7Message));
    message->msh = malloc(sizeof(MSHSegment));
    message->pid = malloc(sizeof(PIDSegment));

    free(buffer);
    fclose(file);

    if (msh_result) h_parse_result_free(msh_result);
    if (pid_result) h_parse_result_free(pid_result);

    return message;
}

void free_hl7_message(HL7Message* message) {
    if (message) {
        free(message->msh);
        free(message->pid);
        free(message);
    }
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    HL7Message* message = parse_hl7_message(argv[1]);
    if (!message) {
        return 1;
    }

    free_hl7_message(message);
    return 0;
}