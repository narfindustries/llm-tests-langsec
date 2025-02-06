#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HAMMER_NO_PREFIX

typedef struct {
    char field_separator;
    char encoding_characters[4];
    char sending_facility[20];
    char sending_facility_namespace_id[20];
    char receiving_facility[20];
    char receiving_facility_namespace_id[20];
    char date_time_of_message[14];
    char security[10];
    char message_type[10];
    char message_control_id[20];
    char processing_id;
    char version_id[10];
} msh_segment_t;

typedef struct {
    char set_id_patient_id;
    char patient_id[20];
    char patient_id_identifier[10];
    char alternate_patient_id[20];
    char patient_name[50];
    char mothers_maiden_name[20];
    char date_of_birth[8];
    char sex;
    char patient_alias[20];
    char race[10];
    char patient_address[50];
} pid_segment_t;

typedef struct {
    char set_id_patient_visit;
    char visit_number[20];
    char visit_indicator;
    char admission_type;
    char preadmit_number[20];
    char prior_patient_location[20];
    char admission_date_time[14];
    char discharge_date_time[14];
    char visit_number_other[20];
} pv1_segment_t;

typedef struct {
    char order_control[2];
    char placer_order_number[20];
    char filler_order_number[20];
    char order_status[2];
    char response_flag;
    char quantity_timing[10];
} orc_segment_t;

typedef struct {
    char set_id_observation_request;
    char placer_order_number[20];
    char filler_order_number[20];
    char universal_service_id[20];
    char priority[10];
    char requested_date_time[14];
} obr_segment_t;

typedef enum {
    HAMMER_PARSER_STATUS_OK,
    HAMMER_PARSER_STATUS_ERROR
} hammer_parser_status_t;

typedef struct hammer_parser hammer_parser_t;

void msh_segment_parser(hammer_parser_t *parser, void *data) {
    msh_segment_t *segment = (msh_segment_t *)data;
    hammer_string_char(parser, &segment->field_separator);
    hammer_string_chars(parser, segment->encoding_characters, 3);
    hammer_string_chars(parser, segment->sending_facility, 19);
    hammer_string_chars(parser, segment->sending_facility_namespace_id, 19);
    hammer_string_chars(parser, segment->receiving_facility, 19);
    hammer_string_chars(parser, segment->receiving_facility_namespace_id, 19);
    hammer_string_chars(parser, segment->date_time_of_message, 13);
    hammer_string_chars(parser, segment->security, 9);
    hammer_string_chars(parser, segment->message_type, 9);
    hammer_string_chars(parser, segment->message_control_id, 19);
    hammer_string_char(parser, &segment->processing_id);
    hammer_string_chars(parser, segment->version_id, 9);
}

void pid_segment_parser(hammer_parser_t *parser, void *data) {
    pid_segment_t *segment = (pid_segment_t *)data;
    hammer_string_char(parser, &segment->set_id_patient_id);
    hammer_string_chars(parser, segment->patient_id, 19);
    hammer_string_chars(parser, segment->patient_id_identifier, 9);
    hammer_string_chars(parser, segment->alternate_patient_id, 19);
    hammer_string_chars(parser, segment->patient_name, 50);
    hammer_string_chars(parser, segment->mothers_maiden_name, 19);
    hammer_string_chars(parser, segment->date_of_birth, 7);
    hammer_string_char(parser, &segment->sex);
    hammer_string_chars(parser, segment->patient_alias, 19);
    hammer_string_chars(parser, segment->race, 9);
    hammer_string_chars(parser, segment->patient_address, 50);
}

void pv1_segment_parser(hammer_parser_t *parser, void *data) {
    pv1_segment_t *segment = (pv1_segment_t *)data;
    hammer_string_char(parser, &segment->set_id_patient_visit);
    hammer_string_chars(parser, segment->visit_number, 19);
    hammer_string_char(parser, &segment->visit_indicator);
    hammer_string_char(parser, &segment->admission_type);
    hammer_string_chars(parser, segment->preadmit_number, 19);
    hammer_string_chars(parser, segment->prior_patient_location, 19);
    hammer_string_chars(parser, segment->admission_date_time, 13);
    hammer_string_chars(parser, segment->discharge_date_time, 13);
    hammer_string_chars(parser, segment->visit_number_other, 19);
}

void orc_segment_parser(hammer_parser_t *parser, void *data) {
    orc_segment_t *segment = (orc_segment_t *)data;
    hammer_string_chars(parser, segment->order_control, 1);
    hammer_string_chars(parser, segment->placer_order_number, 19);
    hammer_string_chars(parser, segment->filler_order_number, 19);
    hammer_string_chars(parser, segment->order_status, 1);
    hammer_string_char(parser, &segment->response_flag);
    hammer_string_chars(parser, segment->quantity_timing, 9);
}

void obr_segment_parser(hammer_parser_t *parser, void *data) {
    obr_segment_t *segment = (obr_segment_t *)data;
    hammer_string_char(parser, &segment->set_id_observation_request);
    hammer_string_chars(parser, segment->placer_order_number, 19);
    hammer_string_chars(parser, segment->filler_order_number, 19);
    hammer_string_chars(parser, segment->universal_service_id, 19);
    hammer_string_chars(parser, segment->priority, 9);
    hammer_string_chars(parser, segment->requested_date_time, 13);
}

void hl7_message_parser(hammer_parser_t *parser, void *data) {
    void *segment_data = malloc(sizeof(msh_segment_t) + sizeof(pid_segment_t) + sizeof(pv1_segment_t) + sizeof(orc_segment_t) + sizeof(obr_segment_t));
    hammer_repeat(
        hammer_choice(
            msh_segment_parser, segment_data,
            pid_segment_parser, segment_data,
            pv1_segment_parser, segment_data,
            orc_segment_parser, segment_data,
            obr_segment_parser, segment_data
        )
    );
    free(segment_data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *input_file = fopen(argv[1], "rb");
    if (!input_file) {
        printf("Error opening input file\n");
        return 1;
    }

    fseek(input_file, 0, SEEK_END);
    long file_size = ftell(input_file);
    rewind(input_file);

    char *input_data = malloc(file_size);
    if (!input_data) {
        printf("Error allocating memory\n");
        fclose(input_file);
        return 1;
    }

    fread(input_data, 1, file_size, input_file);
    fclose(input_file);

    hammer_parser_t *parser = hammer_parser_new();
    hammer_parser_set_parser(parser, hl7_message_parser, NULL);
    int result = hammer_parser_parse(parser, input_data, file_size);

    if (result == 0) {
        printf("Parsed HL7 message successfully\n");
    } else {
        printf("Error parsing HL7 message\n");
    }

    free(input_data);
    hammer_parser_free(parser);

    return 0;
}