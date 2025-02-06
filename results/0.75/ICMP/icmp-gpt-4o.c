#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *icmp_parser() {
    // Define the ICMP header fields
    HParser *type = h_uint8();
    HParser *code = h_uint8();
    HParser *checksum = h_uint16();

    // Capture all remaining bytes in the packet
    HParser *rest_of_header = h_many(h_uint8());

    // Construct the ICMP parser
    return h_sequence(type, code, checksum, rest_of_header, NULL);
}

void parse_icmp(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Could not open file");
        return;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = (unsigned char *)malloc(file_size);
    if (!data) {
        perror("Could not allocate memory");
        fclose(file);
        return;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = icmp_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        const HParsedToken *ast = result->ast;
        printf("Parsed ICMP Packet:\n");
        printf("Type: %u\n", ast->seq->elements[0]->uint);
        printf("Code: %u\n", ast->seq->elements[1]->uint);
        printf("Checksum: %u\n", ast->seq->elements[2]->uint);
        
        // Print the rest of the header
        printf("Rest of Header: ");
        HParsedToken *rest = ast->seq->elements[3];
        for (size_t i = 0; i < rest->seq->used; i++) {
            printf("%02x ", rest->seq->elements[i]->uint);
        }
        printf("\n");

        h_parse_result_free(result);
    } else {
        printf("Failed to parse ICMP packet.\n");
    }

    free(data);
    h_parser_free(parser);
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_icmp(argv[1]);

    return EXIT_SUCCESS;
}