#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static HParser *LocalFileHeaderGrammar() {
    return h_sequence(
        h_constant(4, 0x04034B50),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint16(),
        h_uint16(),
        h_length_value(h_uint16(), h_sequence(h_chunk(), NULL)),
        h_length_value(h_uint16(), h_sequence(h_chunk(), NULL)),
        NULL
    );
}

static HParser *CentralDirectoryFileHeaderGrammar() {
    return h_sequence(
        h_constant(4, 0x02014B50),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_uint32(),
        h_length_value(h_uint16(), h_sequence(h_chunk(), NULL)),
        h_length_value(h_uint16(), h_sequence(h_chunk(), NULL)),
        h_length_value(h_uint16(), h_sequence(h_chunk(), NULL)),
        NULL
    );
}

static HParser *EndOfCentralDirectoryGrammar() {
    return h_sequence(
        h_constant(4, 0x06054B50),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_uint32(),
        h_uint16(),
        h_length_value(h_uint16(), h_sequence(h_chunk(), NULL)),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <zipfile>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *local_file_header_parser = LocalFileHeaderGrammar();
    HParser *central_directory_parser = CentralDirectoryFileHeaderGrammar();
    HParser *eocd_parser = EndOfCentralDirectoryGrammar();

    HParseResult *result;

    result = h_parse(local_file_header_parser, buffer, file_size);
    if (result) {
        printf("Local File Header Parsed Successfully\n");
    }

    result = h_parse(central_directory_parser, buffer, file_size);
    if (result) {
        printf("Central Directory File Header Parsed Successfully\n");
    }

    result = h_parse(eocd_parser, buffer, file_size);
    if (result) {
        printf("End of Central Directory Parsed Successfully\n");
    }

    free(buffer);
    return 0;
}