#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function prototypes for creating parsers
HParser *create_AE();
HParser *create_CS();
HParser *create_DA();
HParser *create_DS();
HParser *create_DT();
HParser *create_IS();
HParser *create_OB();
HParser *create_OW();
HParser *create_PN();
HParser *create_SH();
HParser *create_TM();
HParser *create_UI();
HParser *create_tag();
HParser *create_data_element();
HParser *create_dicom_file();

// Main parsing function
void parse_dicom(const char *filename) {
    FILE *fp = fopen(filename, "rb");
    if (!fp) {
        fprintf(stderr, "Failed to open file %s\n", filename);
        return;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(fp);
        return;
    }

    if (fread(buffer, 1, size, fp) != size) {
        fprintf(stderr, "Failed to read file\n");
        free(buffer);
        fclose(fp);
        return;
    }

    fclose(fp);

    HParser *dicom_file = create_dicom_file();
    HParseResult *result = h_parse(dicom_file, buffer, size);
    if (result) {
        printf("DICOM file parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 4);
    } else {
        fprintf(stderr, "Failed to parse DICOM file.\n");
    }

    free(buffer);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    parse_dicom(argv[1]);
    return 0;
}

// Parser creation functions
HParser *create_AE() { return h_token("AE", 2); }
HParser *create_CS() { return h_token("CS", 2); }
HParser *create_DA() { return h_token("DA", 2); }
HParser *create_DS() { return h_token("DS", 2); }
HParser *create_DT() { return h_token("DT", 2); }
HParser *create_IS() { return h_token("IS", 2); }
HParser *create_OB() { return h_token("OB", 2); }
HParser *create_OW() { return h_token("OW", 2); }
HParser *create_PN() { return h_token("PN", 2); }
HParser *create_SH() { return h_token("SH", 2); }
HParser *create_TM() { return h_token("TM", 2); }
HParser *create_UI() { return h_token("UI", 2); }
HParser *create_tag() { return h_bits(32, false); }

HParser *create_data_element() {
    return h_sequence(create_tag(), h_choice(create_AE(), create_CS(), create_DA(), create_DS(), create_DT(), create_IS(), create_OB(), create_OW(), create_PN(), create_SH(), create_TM(), create_UI(), NULL), NULL);
}

HParser *create_dicom_file() {
    return h_many(create_data_element());
}