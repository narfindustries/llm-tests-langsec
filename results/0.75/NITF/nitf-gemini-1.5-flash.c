#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

hammer_parser_t* string_parser(size_t len) {
    return hammer_string(len);
}

hammer_parser_t* uint_parser(size_t len) {
    return hammer_uint(len * 8);
}

hammer_parser_t* int_parser(size_t len) {
    return hammer_int(len * 8);
}


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <NITF_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buffer = (char *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_parser_t* file_id_parser = string_parser(8);
    hammer_parser_t* version_parser = uint_parser(1);
    hammer_parser_t* nrows_parser = uint_parser(4);
    hammer_parser_t* ncols_parser = uint_parser(4);

    hammer_parser_t* header_parser = hammer_seq(file_id_parser, version_parser, nrows_parser, ncols_parser, NULL);

    hammer_result_t result = hammer_parse(header_parser, buffer, fsize);

    if (result.success) {
        printf("File ID: %.*s\n", (int)hammer_get_string_len(result.values[0]), hammer_get_string(result.values[0]));
        printf("Version: %u\n", hammer_get_uint(result.values[1]));
        printf("Number of Rows: %u\n", hammer_get_uint(result.values[2]));
        printf("Number of Columns: %u\n", hammer_get_uint(result.values[3]));

        hammer_free_result(&result);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    hammer_free_parser(header_parser);
    hammer_free_parser(file_id_parser);
    hammer_free_parser(version_parser);
    hammer_free_parser(nrows_parser);
    hammer_free_parser(ncols_parser);

    return 0;
}
