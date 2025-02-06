#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    char* patient_name;
    char* patient_id;
    char* patient_birth_date;
    char* patient_sex;
    char* study_instance_uid;
    char* study_date;
    char* accession_number;
    char* referring_physician_name;
    char* modality;
    char* series_instance_uid;
    int series_number;
    char* series_description;
    char* sop_class_uid;
    char* sop_instance_uid;
    int image_number;
    uint8_t* pixel_data;
    size_t pixel_data_length;
} DICOMImage;

static HParsedToken* parse_patient_module(const HParseResult* p, void* user_data) {
    return h_get_parse_result_token(p);
}

static HParsedToken* parse_study_module(const HParseResult* p, void* user_data) {
    return h_get_parse_result_token(p);
}

static HParsedToken* parse_series_module(const HParseResult* p, void* user_data) {
    return h_get_parse_result_token(p);
}

static HParsedToken* parse_image_module(const HParseResult* p, void* user_data) {
    return h_get_parse_result_token(p);
}

HParser* create_dicom_parser() {
    HParser* patient_name = h_token_c_string();
    HParser* patient_id = h_token_c_string();
    HParser* patient_birth_date = h_token_c_string();
    HParser* patient_sex = h_choice(h_token_c_string(), NULL);

    HParser* study_uid = h_token_c_string();
    HParser* study_date = h_token_c_string();
    HParser* accession_number = h_token_c_string();
    HParser* referring_physician = h_token_c_string();

    HParser* modality = h_token_c_string();
    HParser* series_uid = h_token_c_string();
    HParser* series_number = h_int64();
    HParser* series_description = h_token_c_string();

    HParser* sop_class_uid = h_token_c_string();
    HParser* sop_instance_uid = h_token_c_string();
    HParser* image_number = h_int64();
    HParser* pixel_data = h_many(h_bits(8, false));

    const HParser* patient_module = h_action(
        h_seq(patient_name, patient_id, patient_birth_date, patient_sex, NULL), 
        parse_patient_module, 
        NULL
    );

    const HParser* study_module = h_action(
        h_seq(study_uid, study_date, accession_number, referring_physician, NULL), 
        parse_study_module, 
        NULL
    );

    const HParser* series_module = h_action(
        h_seq(modality, series_uid, series_number, series_description, NULL), 
        parse_series_module, 
        NULL
    );

    const HParser* image_module = h_action(
        h_seq(sop_class_uid, sop_instance_uid, image_number, pixel_data, NULL), 
        parse_image_module, 
        NULL
    );

    return h_seq(patient_module, study_module, series_module, image_module, NULL);
}

int main(int argc, char* argv[]) {
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

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser* dicom_parser = create_dicom_parser();
    HParseResult* result = h_parse(dicom_parser, buffer, file_size);

    if (result && result->ast) {
        printf("DICOM file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    free(buffer);
    h_parser_destroy(dicom_parser);
    return 0;
}