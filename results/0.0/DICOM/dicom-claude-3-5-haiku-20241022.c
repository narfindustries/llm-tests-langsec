#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct {
    char* patient_name;
    char* patient_id;
    char* patient_birth_date;
    char patient_sex;
    char* study_instance_uid;
    char* study_date;
    char* study_time;
    char* accession_number;
    char* modality;
    int64_t series_number;
    char* series_description;
    char* sop_class_uid;
    char* image_type;
    uint8_t* pixel_data;
    size_t pixel_data_length;
} DICOMImage;

static HParsedToken* parse_patient_module(void* p) {
    HParser* patient_name = h_repeat_n(h_ch_range('A', 'Z'), 64);
    HParser* patient_id = h_repeat_n(h_ch_range('0', '9'), 16);
    HParser* patient_birth_date = h_repeat_n(h_ch_range('0', '9'), 8);
    HParser* patient_sex = h_choice(
        h_ch('M'), h_ch('F'), h_ch('O'), NULL
    );

    HParser* patient_parser = h_sequence(patient_name, patient_id, patient_birth_date, patient_sex, NULL);
    return h_parse(patient_parser, NULL, 0)->ast;
}

static HParsedToken* parse_study_module(void* p) {
    HParser* study_uid = h_repeat_n(h_ch_range('0', '9'), 64);
    HParser* study_date = h_repeat_n(h_ch_range('0', '9'), 8);
    HParser* study_time = h_repeat_n(h_ch_range('0', '9'), 6);
    HParser* accession_number = h_repeat_n(h_ch_range('0', '9'), 16);

    HParser* study_parser = h_sequence(study_uid, study_date, study_time, accession_number, NULL);
    return h_parse(study_parser, NULL, 0)->ast;
}

static HParsedToken* parse_series_module(void* p) {
    HParser* modality = h_choice(
        h_string("CT"), h_string("MR"), 
        h_string("US"), h_string("XA"), NULL
    );
    HParser* series_number = h_int_range(h_ch_range('0', '9'), 0, 65535);
    HParser* series_description = h_repeat_n(h_ch_range('A', 'Z'), 64);

    HParser* series_parser = h_sequence(modality, series_number, series_description, NULL);
    return h_parse(series_parser, NULL, 0)->ast;
}

static HParsedToken* parse_image_module(void* p) {
    HParser* sop_class_uid = h_repeat_n(h_ch_range('0', '9'), 64);
    HParser* image_type = h_choice(
        h_string("ORIGINAL"), h_string("DERIVED"), 
        h_string("SECONDARY"), NULL
    );
    HParser* pixel_data = h_many(h_ch_range(0, 255));

    HParser* image_parser = h_sequence(sop_class_uid, image_type, pixel_data, NULL);
    return h_parse(image_parser, NULL, 0)->ast;
}

static HParser* parse_dicom(void* p) {
    return h_sequence(
        parse_patient_module,
        parse_study_module,
        parse_series_module,
        parse_image_module,
        NULL
    );
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

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* dicom_parser = parse_dicom(NULL);
    HParseResult* result = h_parse(dicom_parser, buffer, read_size);

    if (result && result->ast) {
        printf("DICOM file parsed successfully\n");
    } else {
        printf("DICOM parsing failed\n");
    }

    h_parse_result_free(result);
    h_arena_free(dicom_parser->arena);
    free(buffer);

    return 0;
}