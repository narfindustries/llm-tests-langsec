#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    HParser* field_separator;
    HParser* encoding_chars;
    HParser* sending_app;
    HParser* sending_facility;
    HParser* receiving_app;
    HParser* receiving_facility;
    HParser* timestamp;
    HParser* security;
    HParser* message_type;
    HParser* message_control_id;
    HParser* processing_id;
    HParser* version;
} MSHSegment;

typedef struct {
    HParser* set_id;
    HParser* patient_id;
    HParser* patient_name;
    HParser* mother_maiden_name;
    HParser* date_of_birth;
    HParser* gender;
    HParser* patient_alias;
    HParser* race;
    HParser* patient_address;
    HParser* county_code;
    HParser* phone_number;
} PIDSegment;

typedef struct {
    HParser* patient_class;
    HParser* assigned_patient_location;
    HParser* admission_type;
    HParser* preadmit_number;
    HParser* prior_patient_location;
    HParser* attending_doctor;
    HParser* referring_doctor;
    HParser* consulting_doctor;
} PV1Segment;

typedef struct {
    MSHSegment* msh;
    PIDSegment* pid;
    PV1Segment* pv1;
} HL7Message;

HParser* create_hl7_parser() {
    HParser* field_parser = h_choice(
        h_ch_range('0', '9'),
        h_ch('|'),
        h_ch('^'),
        NULL
    );

    MSHSegment* msh = malloc(sizeof(MSHSegment));
    msh->field_separator = h_ch('|');
    msh->encoding_chars = h_token((const uint8_t*)"^~\\&", 4);
    msh->sending_app = field_parser;
    msh->sending_facility = field_parser;
    msh->receiving_app = field_parser;
    msh->receiving_facility = field_parser;
    msh->timestamp = field_parser;
    msh->security = field_parser;
    msh->message_type = field_parser;
    msh->message_control_id = field_parser;
    msh->processing_id = field_parser;
    msh->version = field_parser;

    PIDSegment* pid = malloc(sizeof(PIDSegment));
    pid->set_id = field_parser;
    pid->patient_id = field_parser;
    pid->patient_name = field_parser;
    pid->mother_maiden_name = field_parser;
    pid->date_of_birth = field_parser;
    pid->gender = h_choice(
        h_token((const uint8_t*)"M", 1),
        h_token((const uint8_t*)"F", 1),
        h_token((const uint8_t*)"O", 1),
        NULL
    );
    pid->patient_alias = field_parser;
    pid->race = field_parser;
    pid->patient_address = field_parser;
    pid->county_code = field_parser;
    pid->phone_number = field_parser;

    PV1Segment* pv1 = malloc(sizeof(PV1Segment));
    pv1->patient_class = h_choice(
        h_token((const uint8_t*)"I", 1),
        h_token((const uint8_t*)"O", 1),
        h_token((const uint8_t*)"E", 1),
        NULL
    );
    pv1->assigned_patient_location = field_parser;
    pv1->admission_type = field_parser;
    pv1->preadmit_number = field_parser;
    pv1->prior_patient_location = field_parser;
    pv1->attending_doctor = field_parser;
    pv1->referring_doctor = field_parser;
    pv1->consulting_doctor = field_parser;

    HParser* msh_parser = h_sequence(
        msh->field_separator,
        msh->encoding_chars,
        msh->sending_app,
        msh->sending_facility,
        msh->receiving_app,
        msh->receiving_facility,
        msh->timestamp,
        msh->security,
        msh->message_type,
        msh->message_control_id,
        msh->processing_id,
        msh->version,
        NULL
    );

    HParser* pid_parser = h_sequence(
        pid->set_id,
        pid->patient_id,
        pid->patient_name,
        pid->mother_maiden_name,
        pid->date_of_birth,
        pid->gender,
        pid->patient_alias,
        pid->race,
        pid->patient_address,
        pid->county_code,
        pid->phone_number,
        NULL
    );

    HParser* pv1_parser = h_sequence(
        pv1->patient_class,
        pv1->assigned_patient_location,
        pv1->admission_type,
        pv1->preadmit_number,
        pv1->prior_patient_location,
        pv1->attending_doctor,
        pv1->referring_doctor,
        pv1->consulting_doctor,
        NULL
    );

    return h_sequence(
        msh_parser,
        pid_parser,
        pv1_parser,
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <hl7_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        perror("Error reading file");
        free(buffer);
        return 1;
    }

    HParser* hl7_parser = create_hl7_parser();
    HParseResult* result = h_parse(hl7_parser, buffer, bytes_read);

    if (result && result->ast) {
        printf("HL7 message parsed successfully\n");
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    h_parse_result_free(result);
    h_arena_free(h_parser_arena(hl7_parser));
    free(buffer);

    return 0;
}