#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char* patient_name;
    char* patient_id;
    char* patient_birthdate;
    char* patient_sex;
    int patient_age;
    char* study_instance_uid;
    char* study_date;
    char* study_time;
    char* accession_number;
    char* referring_physician;
    char* series_instance_uid;
    char* modality;
    int series_number;
    char* series_description;
    uint8_t* pixel_data;
    size_t pixel_data_length;
} DICOMImage;

HParser* create_dicom_parser() {
    HParser* transfer_syntax = h_choice(
        h_literal("1.2.840.10008.1.2"),
        h_literal("1.2.840.10008.1.2.1"),
        h_literal("1.2.840.10008.1.2.2"),
        NULL
    );

    HParser* patient_name = h_many1(h_ch_range('A', 'Z'));
    HParser* patient_id = h_many1(h_ch_range('0', '9'));
    HParser* patient_birthdate = h_many1(h_ch_range('0', '9'));
    HParser* patient_sex = h_choice(
        h_ch('M'),
        h_ch('F'),
        h_ch('O'),
        NULL
    );

    HParser* dicom_parser = h_sequence(
        transfer_syntax,
        patient_name,
        patient_id,
        patient_birthdate,
        patient_sex,
        NULL
    );

    return dicom_parser;
}

HParseResult* parse_dicom(void* data, size_t len) {
    HParser* parser = create_dicom_parser();
    return h_parse(parser, data, len);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Cannot open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult* result = parse_dicom(buffer, file_size);
    if (result && result->ast) {
        printf("DICOM file parsed successfully\n");
    } else {
        printf("DICOM parsing failed\n");
    }

    free(buffer);
    return 0;
}