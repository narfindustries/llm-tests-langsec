#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// DICOM Tags
#define GROUP_LENGTH 0x00020000
#define VERSION 0x00020001
#define MEDIA_STORAGE_SOP_CLASS_UID 0x00020002
#define MEDIA_STORAGE_SOP_INSTANCE_UID 0x00020003
#define TRANSFER_SYNTAX_UID 0x00020010
#define IMPLEMENTATION_CLASS_UID 0x00020012
#define IMPLEMENTATION_VERSION_NAME 0x00020013
#define SOURCE_APPLICATION_ENTITY_TITLE 0x00020016
#define PRIVATE_INFORMATION_CREATOR_UID 0x00020100
#define PRIVATE_INFORMATION 0x00020102

// DICOM VR (Value Representation) Definitions
#define VR_AE h_length_value(h_uint8(), h_ignore(15)) // Application Entity
#define VR_AS h_length_value(h_uint8(), h_ignore(3))  // Age String
#define VR_AT h_length_value(h_uint8(), h_ignore(3))  // Attribute Tag
#define VR_CS h_length_value(h_uint8(), h_ignore(15)) // Code String
#define VR_DA h_length_value(h_uint8(), h_ignore(7))  // Date
#define VR_DS h_length_value(h_uint8(), h_ignore(15)) // Decimal String
#define VR_DT h_length_value(h_uint8(), h_ignore(25)) // Date Time
#define VR_FL h_float()   // Floating Point Single
#define VR_FD h_double()  // Floating Point Double
#define VR_IS h_length_value(h_uint8(), h_ignore(11)) // Integer String
#define VR_LO h_length_value(h_uint8(), h_ignore(63)) // Long String
#define VR_LT h_length_value(h_uint8(), h_ignore(10239)) // Long Text
#define VR_OB h_many1(h_uint8())  // Other Byte
#define VR_OW h_many1(h_uint16())  // Other Word
#define VR_PN h_length_value(h_uint8(), h_ignore(63)) // Person Name
#define VR_SH h_length_value(h_uint8(), h_ignore(15)) // Short String
#define VR_SL h_int32()   // Signed Long
#define VR_SQ h_sequence(h_int32()) // Sequence of Items
#define VR_SS h_int16()   // Signed Short
#define VR_ST h_length_value(h_uint8(), h_ignore(1023)) // Short Text
#define VR_TM h_length_value(h_uint8(), h_ignore(15)) // Time
#define VR_UI h_length_value(h_uint8(), h_ignore(63)) // Unique Identifier
#define VR_UL h_uint32()  // Unsigned Long
#define VR_US h_uint16()  // Unsigned Short
#define VR_UT h_length_value(h_uint8(), h_ignore(65535)) // Unlimited Text

// DICOM Element Parser
HParser *dicom_element(uint32_t tag, HParser *vr_parser) {
    return h_sequence(h_uint32(), h_uint32(), vr_parser, NULL);
}

// Main DICOM Parser
HParser *dicom_parser() {
    HParser *p_group_length = dicom_element(GROUP_LENGTH, VR_UL);
    HParser *p_version = dicom_element(VERSION, VR_OB);
    HParser *p_media_storage_sop_class_uid = dicom_element(MEDIA_STORAGE_SOP_CLASS_UID, VR_UI);
    HParser *p_media_storage_sop_instance_uid = dicom_element(MEDIA_STORAGE_SOP_INSTANCE_UID, VR_UI);
    HParser *p_transfer_syntax_uid = dicom_element(TRANSFER_SYNTAX_UID, VR_UI);
    HParser *p_implementation_class_uid = dicom_element(IMPLEMENTATION_CLASS_UID, VR_UI);
    HParser *p_implementation_version_name = dicom_element(IMPLEMENTATION_VERSION_NAME, VR_SH);
    HParser *p_source_application_entity_title = dicom_element(SOURCE_APPLICATION_ENTITY_TITLE, VR_AE);
    HParser *p_private_information_creator_uid = dicom_element(PRIVATE_INFORMATION_CREATOR_UID, VR_UI);
    HParser *p_private_information = dicom_element(PRIVATE_INFORMATION, VR_OB);

    return h_choice(p_group_length, p_version, p_media_storage_sop_class_uid, p_media_storage_sop_instance_uid,
                    p_transfer_syntax_uid, p_implementation_class_uid, p_implementation_version_name,
                    p_source_application_entity_title, p_private_information_creator_uid, p_private_information, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <DICOM file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(fsize);
    fread(buffer, 1, fsize, fp);
    fclose(fp);

    HParser *parser = dicom_parser();
    HParseResult *result = h_parse(parser, buffer, fsize);
    if (result) {
        printf("DICOM file parsed successfully.\n");
    } else {
        printf("Failed to parse DICOM file.\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return 0;
}