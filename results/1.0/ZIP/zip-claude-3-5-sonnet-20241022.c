#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parser combinators for ZIP format structures
HParser* zip_local_file_header_signature() {
    return h_token((const uint8_t*)"\x50\x4b\x03\x04", 4);
}

HParser* zip_central_directory_signature() {
    return h_token((const uint8_t*)"\x50\x4b\x01\x02", 4);
}

HParser* zip_end_of_central_directory_signature() {
    return h_token((const uint8_t*)"\x50\x4b\x05\x06", 4);
}

HParser* zip_local_file_header() {
    return h_sequence(
        zip_local_file_header_signature(),
        h_uint16(),  // version needed to extract
        h_uint16(),  // general purpose bit flag
        h_uint16(),  // compression method
        h_uint16(),  // last mod file time
        h_uint16(),  // last mod file date
        h_uint32(),  // crc-32
        h_uint32(),  // compressed size
        h_uint32(),  // uncompressed size
        h_length_value(h_uint16(), h_uint8()),  // filename
        h_length_value(h_uint16(), h_uint8()),  // extra field
        NULL
    );
}

HParser* zip_central_directory_header() {
    return h_sequence(
        zip_central_directory_signature(),
        h_uint16(),  // version made by
        h_uint16(),  // version needed to extract
        h_uint16(),  // general purpose bit flag
        h_uint16(),  // compression method
        h_uint16(),  // last mod file time
        h_uint16(),  // last mod file date
        h_uint32(),  // crc-32
        h_uint32(),  // compressed size
        h_uint32(),  // uncompressed size
        h_length_value(h_uint16(), h_uint8()),  // filename length
        h_length_value(h_uint16(), h_uint8()),  // extra field length
        h_length_value(h_uint16(), h_uint8()),  // file comment length
        h_uint16(),  // disk number start
        h_uint16(),  // internal file attributes
        h_uint32(),  // external file attributes
        h_uint32(),  // relative offset of local header
        NULL
    );
}

HParser* zip_end_of_central_directory() {
    return h_sequence(
        zip_end_of_central_directory_signature(),
        h_uint16(),  // number of this disk
        h_uint16(),  // number of disk with central directory
        h_uint16(),  // total entries in central directory on this disk
        h_uint16(),  // total entries in central directory
        h_uint32(),  // size of central directory
        h_uint32(),  // offset of start of central directory
        h_length_value(h_uint16(), h_uint8()),  // zip file comment
        NULL
    );
}

HParser* zip_file() {
    return h_sequence(
        h_many(zip_local_file_header()),
        h_many(zip_central_directory_header()),
        zip_end_of_central_directory(),
        NULL
    );
}

void print_parse_tree(const HParsedToken* token, int depth) {
    for (int i = 0; i < depth; i++) printf("  ");
    
    if (token->token_type == TT_SEQUENCE) {
        printf("Sequence:\n");
        for (size_t i = 0; i < token->seq->used; i++) {
            print_parse_tree(token->seq->elements[i], depth + 1);
        }
    } else if (token->token_type == TT_UINT) {
        printf("Integer: %lu\n", (unsigned long)token->uint);
    } else if (token->token_type == TT_BYTES) {
        printf("Bytes: length=%zu\n", token->bytes.len);
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *buf = malloc(size);
    if (!buf) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(buf, 1, size, f) != size) {
        perror("Failed to read file");
        free(buf);
        fclose(f);
        return 1;
    }

    HParser *zip_parser = zip_file();
    const HParseResult *result = h_parse(zip_parser, buf, size);

    if (!result) {
        fprintf(stderr, "Failed to parse ZIP file\n");
        free(buf);
        fclose(f);
        return 1;
    }

    print_parse_tree(result->ast, 0);

    free(buf);
    fclose(f);
    return 0;
}