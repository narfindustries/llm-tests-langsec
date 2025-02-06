#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char* field_separator;
    char* encoding_chars;
    char* sending_app;
    char* sending_facility;
    char* receiving_app;
    char* receiving_facility;
    char* datetime;
    char* msg_type;
    char* msg_control_id;
    char* processing_id;
    char* version_id;
} MSH_Segment;

typedef struct {
    char* set_id;
    char* patient_id;
    char* identifier_list;
    char* patient_name;
    char* birth_datetime;
    char* admin_sex;
    char* patient_address;
} PID_Segment;

HParser* create_msh_parser() {
    HParser* field_sep = h_ch('|');
    HParser* enc_chars = h_token((uint8_t*)"^~\\&", 4);
    HParser* app = h_many1(h_not_in((uint8_t*)"|", 1));
    HParser* facility = h_many1(h_not_in((uint8_t*)"|", 1));
    HParser* datetime = h_many1(h_not_in((uint8_t*)"|", 1));
    HParser* msg_type = h_many1(h_not_in((uint8_t*)"|", 1));
    HParser* control_id = h_many1(h_not_in((uint8_t*)"|", 1));
    HParser* proc_id = h_many1(h_not_in((uint8_t*)"|", 1));
    HParser* ver_id = h_many1(h_not_in((uint8_t*)"|", 1));

    return h_sequence(field_sep, enc_chars, app, facility, app, facility,
                     datetime, msg_type, control_id, proc_id, ver_id, NULL);
}

HParser* create_pid_parser() {
    HParser* field = h_many1(h_not_in((uint8_t*)"|", 1));
    return h_sequence(field, field, field, field, field, field, field, NULL);
}

HParser* create_hl7_parser() {
    HParser* msh = create_msh_parser();
    HParser* pid = create_pid_parser();
    return h_sequence(msh, h_ch('\n'), pid, NULL);
}

void print_parse_result(HParseResult* result) {
    if(!result) {
        printf("Parse failed\n");
        return;
    }
    printf("Parse successful\n");
}

int main(int argc, char** argv) {
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if(!file) {
        perror("Could not open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fsize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(fsize);
    fread(buffer, fsize, 1, file);
    fclose(file);

    HParser* parser = create_hl7_parser();
    HParseResult* result = h_parse(parser, buffer, fsize);
    print_parse_result(result);

    free(buffer);
    return 0;
}