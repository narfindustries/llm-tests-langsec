#include <hammer/hammer.h>
#include <hammer/glue.h>

// Basic DICOM Tags
static HParser *p_tag;
static HParser *p_vr;
static HParser *p_length;
static HParser *p_value;

// DICOM Element
static HParser *p_element;

// DICOM File
static HParser *p_dicom_file;

static void init_dicom_parser() {
    // VR (Value Representation) - 2 bytes, typically ASCII
    p_vr = h_token("AEASATCSDAFLFDISLOLTOPNSHSLSSSTTMUIULUNUSOWODSQ", 2);

    // Length - 4 bytes, unsigned integer (little endian)
    p_length = h_le_u32();

    // Value - variable length, defined by the length field
    p_value = h_bits(8, false);

    // Tag - 2 groups of 2 bytes each (little endian)
    p_tag = h_sequence(h_le_u16(), h_le_u16(), NULL);

    // DICOM Element - Tag, VR, Length, Value
    p_element = h_sequence(p_tag, p_vr, p_length, h_repeat_n(p_value, h_indirect()), NULL);

    // DICOM File - sequence of elements
    p_dicom_file = h_many(p_element);
}

int main(int argc, char **argv) {
    HParser *dicom_parser;
    init_dicom_parser();
    dicom_parser = p_dicom_file;

    // Assuming input is provided as a binary file
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t length = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(length);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, length, fp) != length) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    fclose(fp);

    HParseResult *result = h_parse(dicom_parser, buffer, length);
    if (!result) {
        fprintf(stderr, "Parsing failed.\n");
        free(buffer);
        return 1;
    }

    // Assuming some function to handle or display the parsed data
    // display_dicom(result->ast);

    h_parse_result_free(result);
    free(buffer);
    return 0;
}