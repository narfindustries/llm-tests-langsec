#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>

// Define the HParser structure for a PNG file
static HParser *png_signature;
static HParser *chunk_length;
static HParser *chunk_type;
static HParser *chunk_data;
static HParser *chunk_crc;
static HParser *chunk;
static HParser *chunks;
static HParser *png_file;

// Define helper functions for building the parsers
static void init_parsers() {
    png_signature = h_token("\x89PNG\r\n\x1a\n", 8);
    chunk_length = h_uint32be();
    chunk_type = h_token_s(4);
    chunk_data = h_bytes(0);
    chunk_crc = h_uint32be();
    chunk = h_sequence(chunk_length,
                       chunk_type,
                       h_length_value(chunk_data, h_this(chunk_length)),
                       chunk_crc,
                       NULL);
    chunks = h_many(chunk);
    png_file = h_sequence(png_signature, chunks, NULL);
}

int main(int argc, char **argv) {
    init_parsers();

    // Check that the parser was created successfully
    if (!png_file) {
        fprintf(stderr, "Failed to create the PNG parser\n");
        return EXIT_FAILURE;
    }

    // Open the file
    FILE *fp;
    if ((fp = fopen("example.png", "rb")) == NULL) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    // Load the file content into memory
    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buf = malloc(fsize);
    fread(buf, fsize, 1, fp);
    fclose(fp);

    // Parse the data
    HParseResult *result = h_parse(png_file, buf, fsize);
    if (result == NULL) {
        fprintf(stderr, "Failed to parse PNG file\n");
        free(buf);
        return EXIT_FAILURE;
    }

    printf("The PNG file has been parsed successfully.\n");

    // Cleanup
    free(buf);
    h_parse_result_free(result);

    return EXIT_SUCCESS;
}