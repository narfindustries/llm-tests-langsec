#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct {
    char* patient_name;
    char* patient_id;
    char* patient_birth_date;
    char* patient_sex;
    char* study_instance_uid;
    char* study_date;
    char* study_time;
    char* accession_number;
    char* modality;
    char* series_instance_uid;
    int series_number;
    char* sop_instance_uid;
    int instance_number;
    double image_position[3];
    double image_orientation[6];
} DICOMHeader;

HParser* h_token_string() {
    return h_many1(h_ch_range('A', 'Z'));
}

HParser* h_int_parser() {
    return h_many1(h_ch_range('0', '9'));
}

HParser* h_double_parser() {
    return h_float();
}

HParseResult* parse_dicom_header(const uint8_t* p, size_t len) {
    HParser* patient_name = h_token_string();
    HParser* patient_id = h_token_string();
    HParser* patient_birth_date = h_token_string();
    HParser* patient_sex = h_choice(
        h_token((const uint8_t*)"M", 1), 
        h_token((const uint8_t*)"F", 1), 
        h_token((const uint8_t*)"O", 1), 
        NULL
    );
    HParser* study_instance_uid = h_token_string();
    HParser* study_date = h_token_string();
    HParser* study_time = h_token_string();
    HParser* accession_number = h_token_string();
    HParser* modality = h_choice(
        h_token((const uint8_t*)"CT", 2), 
        h_token((const uint8_t*)"MR", 2), 
        h_token((const uint8_t*)"US", 2), 
        NULL
    );
    HParser* series_instance_uid = h_token_string();
    HParser* series_number = h_int_parser();
    HParser* sop_instance_uid = h_token_string();
    HParser* instance_number = h_int_parser();
    HParser* image_position = h_repeat_n(h_double_parser(), 3);
    HParser* image_orientation = h_repeat_n(h_double_parser(), 6);

    HParser* dicom_parser = h_sequence(
        patient_name,
        patient_id,
        patient_birth_date,
        patient_sex,
        study_instance_uid,
        study_date,
        study_time,
        accession_number,
        modality,
        series_instance_uid,
        series_number,
        sop_instance_uid,
        instance_number,
        image_position,
        image_orientation,
        NULL
    );

    return h_parse(dicom_parser, p, len);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
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

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParseResult* result = parse_dicom_header(buffer, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}