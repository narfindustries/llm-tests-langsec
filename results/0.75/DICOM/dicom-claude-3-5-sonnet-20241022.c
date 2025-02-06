#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint16_t group;
    uint16_t element;
    char vr[3];
    uint32_t length;
    uint8_t* value;
} DicomElement;

typedef struct {
    uint8_t* prefix;
    uint32_t length;
    DicomElement* elements;
} DicomFile;

static HParser* dicom_prefix() {
    return h_sequence(
        h_token((uint8_t*)"DICM", 4),
        NULL);
}

static HParser* dicom_vr() {
    return h_choice(
        h_token((uint8_t*)"AE", 2),
        h_token((uint8_t*)"AS", 2),
        h_token((uint8_t*)"AT", 2),
        h_token((uint8_t*)"CS", 2),
        h_token((uint8_t*)"DA", 2),
        h_token((uint8_t*)"DS", 2),
        h_token((uint8_t*)"DT", 2),
        h_token((uint8_t*)"FL", 2),
        h_token((uint8_t*)"FD", 2),
        h_token((uint8_t*)"IS", 2),
        h_token((uint8_t*)"LO", 2),
        h_token((uint8_t*)"LT", 2),
        h_token((uint8_t*)"OB", 2),
        h_token((uint8_t*)"OF", 2),
        h_token((uint8_t*)"OW", 2),
        h_token((uint8_t*)"PN", 2),
        h_token((uint8_t*)"SH", 2),
        h_token((uint8_t*)"SL", 2),
        h_token((uint8_t*)"SQ", 2),
        h_token((uint8_t*)"SS", 2),
        h_token((uint8_t*)"ST", 2),
        h_token((uint8_t*)"TM", 2),
        h_token((uint8_t*)"UI", 2),
        h_token((uint8_t*)"UL", 2),
        h_token((uint8_t*)"UN", 2),
        h_token((uint8_t*)"US", 2),
        h_token((uint8_t*)"UT", 2),
        NULL);
}

static HParser* dicom_element() {
    return h_sequence(
        h_uint16(),    // group
        h_uint16(),    // element
        dicom_vr(),    // VR
        h_uint32(),    // length
        h_length_value(h_uint32(), h_uint8()),  // value
        NULL);
}

static HParser* dicom_file() {
    return h_sequence(
        dicom_prefix(),
        h_many(dicom_element()),
        NULL);
}

static void print_element(const HParsedToken* elem) {
    if (!elem || elem->token_type != TT_SEQUENCE) return;
    
    uint16_t group = (uint16_t)elem->seq->elements[0]->uint;
    uint16_t element = (uint16_t)elem->seq->elements[1]->uint;
    const uint8_t* vr = elem->seq->elements[2]->bytes.token;
    uint32_t length = (uint32_t)elem->seq->elements[3]->uint;
    
    printf("(%04X,%04X) VR=%.2s Length=%u\n",
           group, element, vr, length);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* buffer = malloc(fsize);
    if (fread(buffer, fsize, 1, f) != 1) {
        perror("Failed to read file");
        fclose(f);
        free(buffer);
        return 1;
    }
    fclose(f);

    HParser* parser = dicom_file();
    HParseResult* result = h_parse(parser, buffer, fsize);

    if (!result) {
        fprintf(stderr, "Failed to parse DICOM file\n");
        free(buffer);
        return 1;
    }

    // Process parsed result
    const HParsedToken* elements = result->ast->seq->elements[1];
    if (elements->token_type == TT_SEQUENCE) {
        for (size_t i = 0; i < elements->seq->used; i++) {
            print_element(elements->seq->elements[i]);
        }
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}