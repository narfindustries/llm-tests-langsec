#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// DICOM Data Element Parser
HParser* dicom_tag() {
    return h_sequence(h_uint16(), h_uint16(), NULL);
}

HParser* vr() {
    return h_token((const uint8_t*)"AE", 2);
    // Add all other VR types similarly
}

HParser* length() {
    return h_choice(h_uint16(), h_uint32(), NULL);
}

HParser* value_field() {
    return h_many(h_uint8());
}

HParser* data_element() {
    return h_sequence(dicom_tag(), vr(), length(), value_field(), NULL);
}

// DICOM Meta Information
HParser* file_preamble() {
    return h_repeat_n(h_uint8(), 128);
}

HParser* dicom_prefix() {
    return h_token((const uint8_t*)"DICM", 4);
}

HParser* meta_information() {
    return h_sequence(
        file_preamble(),
        dicom_prefix(),
        h_many(data_element()),
        NULL
    );
}

// DICOM Dataset
HParser* dataset() {
    return h_many(data_element());
}

// Complete DICOM File Parser
HParser* dicom_file() {
    return h_sequence(meta_information(), dataset(), NULL);
}

void print_parse_result(HParseResult* result) {
    if (!result) {
        printf("Parse failed\n");
        return;
    }
    // Implement result printing based on AST
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        free(buffer);
        fclose(fp);
        return 1;
    }

    HParser* parser = dicom_file();
    HParseResult* result = h_parse(parser, buffer, size);
    
    print_parse_result(result);

    h_parse_result_free(result);
    free(buffer);
    fclose(fp);
    return 0;
}