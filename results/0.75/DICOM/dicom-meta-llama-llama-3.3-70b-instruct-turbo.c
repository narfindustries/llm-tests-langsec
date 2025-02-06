#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define UI_LEN 64
#define LO_LEN 64
#define PN_LEN 64
#define DA_LEN 8
#define TM_LEN 6
#define CS_LEN 16
#define IS_LEN 12
#define DS_LEN 32
#define US_LEN 4

typedef struct {
    char transfer_syntax_uid[UI_LEN];
    char media_storage_sop_instance_uid[UI_LEN];
    char media_storage_sop_class_uid[UI_LEN];
} file_meta_info;

typedef struct {
    char patient_name[PN_LEN];
    char patient_id[LO_LEN];
    char patient_birth_date[DA_LEN];
    char patient_sex[CS_LEN];
} patient_info;

typedef struct {
    char study_instance_uid[UI_LEN];
    char study_date[DA_LEN];
    char study_time[TM_LEN];
    char study_description[LO_LEN];
} study_info;

typedef struct {
    char series_instance_uid[UI_LEN];
    char series_number[IS_LEN];
    char series_description[LO_LEN];
} series_info;

typedef struct {
    char sop_instance_uid[UI_LEN];
    char instance_number[IS_LEN];
    char image_type[CS_LEN];
    char photometric_interpretation[CS_LEN];
} image_info;

typedef struct {
    uint16_t rows;
    uint16_t columns;
    char pixel_spacing[DS_LEN];
    uint8_t* pixel_data;
} pixel_data;

typedef struct {
    file_meta_info file_meta;
    patient_info patient;
    study_info study;
    series_info series;
    image_info image;
    pixel_data pixel;
} dicom_data;

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    dicom_data data;
    HParser* parser = h_file(file);
    if (!parser) {
        printf("Error creating parser\n");
        return 1;
    }

    HParser* file_meta_parser = h_sequence(
        h_string("TransferSyntaxUID", data.file_meta.transfer_syntax_uid),
        h_string("MediaStorageSOPInstanceUID", data.file_meta.media_storage_sop_instance_uid),
        h_string("MediaStorageSOPClassUID", data.file_meta.media_storage_sop_class_uid)
    );

    HParser* patient_parser = h_sequence(
        h_string("PatientName", data.patient.patient_name),
        h_string("PatientID", data.patient.patient_id),
        h_string("PatientBirthDate", data.patient.patient_birth_date),
        h_string("PatientSex", data.patient.patient_sex)
    );

    HParser* study_parser = h_sequence(
        h_string("StudyInstanceUID", data.study.study_instance_uid),
        h_string("StudyDate", data.study.study_date),
        h_string("StudyTime", data.study.study_time),
        h_string("StudyDescription", data.study.study_description)
    );

    HParser* series_parser = h_sequence(
        h_string("SeriesInstanceUID", data.series.series_instance_uid),
        h_string("SeriesNumber", data.series.series_number),
        h_string("SeriesDescription", data.series.series_description)
    );

    HParser* image_parser = h_sequence(
        h_string("SOPInstanceUID", data.image.sop_instance_uid),
        h_string("InstanceNumber", data.image.instance_number),
        h_string("ImageType", data.image.image_type),
        h_string("PhotometricInterpretation", data.image.photometric_interpretation)
    );

    HParser* pixel_parser = h_sequence(
        h_uint16(),
        h_uint16(),
        h_string("PixelSpacing", data.pixel.pixel_spacing),
        h_bytes()
    );

    HParser* dicom_parser = h_sequence(
        file_meta_parser,
        patient_parser,
        study_parser,
        series_parser,
        image_parser,
        pixel_parser
    );

    if (h_parse(parser, dicom_parser)) {
        printf("Error parsing DICOM file\n");
        return 1;
    }

    printf("Transfer Syntax UID: %s\n", data.file_meta.transfer_syntax_uid);
    printf("Media Storage SOP Instance UID: %s\n", data.file_meta.media_storage_sop_instance_uid);
    printf("Media Storage SOP Class UID: %s\n", data.file_meta.media_storage_sop_class_uid);
    printf("Patient Name: %s\n", data.patient.patient_name);
    printf("Patient ID: %s\n", data.patient.patient_id);
    printf("Patient Birth Date: %s\n", data.patient.patient_birth_date);
    printf("Patient Sex: %s\n", data.patient.patient_sex);
    printf("Study Instance UID: %s\n", data.study.study_instance_uid);
    printf("Study Date: %s\n", data.study.study_date);
    printf("Study Time: %s\n", data.study.study_time);
    printf("Study Description: %s\n", data.study.study_description);
    printf("Series Instance UID: %s\n", data.series.series_instance_uid);
    printf("Series Number: %s\n", data.series.series_number);
    printf("Series Description: %s\n", data.series.series_description);
    printf("SOP Instance UID: %s\n", data.image.sop_instance_uid);
    printf("Instance Number: %s\n", data.image.instance_number);
    printf("Image Type: %s\n", data.image.image_type);
    printf("Photometric Interpretation: %s\n", data.image.photometric_interpretation);

    fclose(file);
    return 0;
}