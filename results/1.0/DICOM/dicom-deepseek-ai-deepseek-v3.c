#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

HParser *dicom_parser() {
    // Basic types
    HParser *ui = h_uint32();
    HParser *us = h_uint16();
    HParser *ul = h_uint32();
    HParser *cs = h_token((uint8_t *)"CS", 2);
    HParser *lo = h_token((uint8_t *)"LO", 2);
    HParser *pn = h_token((uint8_t *)"PN", 2);
    HParser *da = h_token((uint8_t *)"DA", 2);
    HParser *tm = h_token((uint8_t *)"TM", 2);
    HParser *dt = h_token((uint8_t *)"DT", 2);
    HParser *as = h_token((uint8_t *)"AS", 2);
    HParser *is = h_token((uint8_t *)"IS", 2);
    HParser *ds = h_token((uint8_t *)"DS", 2);
    HParser *ae = h_token((uint8_t *)"AE", 2);
    HParser *sh = h_token((uint8_t *)"SH", 2);
    HParser *l = h_uint32();
    HParser *lt = h_token((uint8_t *)"LT", 2);
    HParser *ob = h_token((uint8_t *)"OB", 2);
    HParser *of = h_token((uint8_t *)"OF", 2);
    HParser *ow = h_token((uint8_t *)"OW", 2);
    HParser *sq = h_token((uint8_t *)"SQ", 2);
    HParser *ur = h_token((uint8_t *)"UR", 2);
    HParser *un = h_token((uint8_t *)"UN", 2);

    // Complex parsers
    HParser *patient_name = h_sequence(pn, h_end_p(), NULL);
    HParser *patient_id = h_sequence(lo, h_end_p(), NULL);
    HParser *patient_sex = h_sequence(cs, h_end_p(), NULL);
    HParser *study_instance_uid = h_sequence(ui, h_end_p(), NULL);
    HParser *study_date = h_sequence(da, h_end_p(), NULL);
    HParser *modality = h_sequence(cs, h_end_p(), NULL);
    HParser *sop_class_uid = h_sequence(ui, h_end_p(), NULL);
    HParser *rows = h_sequence(us, h_end_p(), NULL);
    HParser *columns = h_sequence(us, h_end_p(), NULL);

    // Combine into a complete DICOM parser
    HParser *dicom = h_sequence(patient_name, patient_id, patient_sex, study_instance_uid, study_date, modality, sop_class_uid, rows, columns, h_end_p(), NULL);

    return dicom;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <DICOM file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = dicom_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Successfully parsed DICOM file.\n");
        free(result->ast);
        free(result);
    } else {
        printf("Failed to parse DICOM file.\n");
    }

    free(buffer);
    return 0;
}