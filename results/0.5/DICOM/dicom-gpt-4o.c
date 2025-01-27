#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *create_dicom_parser() {
    // Define the basic elements of the DICOM format
    HParser *element_tag = h_sequence(h_uint16(), h_uint16(), NULL);
    HParser *element_length = h_choice(h_uint16(), h_uint32(), NULL);
    
    // Define the DICOM element
    HParser *dicom_element = h_sequence(
        element_tag,
        element_length,
        h_data(h_length_value(element_length)),
        NULL
    );

    // Define the DICOM preamble and prefix
    HParser *dicom_preamble = h_data(h_constant(128));
    HParser *dicom_prefix = h_token("DICM", 4);

    // Define the full DICOM file structure
    HParser *dicom_file = h_sequence(
        dicom_preamble,
        dicom_prefix,
        h_many(dicom_element),
        NULL
    );

    return dicom_file;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <dicom_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = (unsigned char *)malloc(file_size);
    if (!buffer) {
        perror("Error allocating memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = create_dicom_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result->ast) {
        printf("DICOM file parsed successfully.\n");
        h_dump_ast(stdout, result->ast);
    } else {
        fprintf(stderr, "Failed to parse DICOM file.\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return EXIT_SUCCESS;
}