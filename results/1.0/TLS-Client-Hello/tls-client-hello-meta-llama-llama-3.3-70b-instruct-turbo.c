#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

void* define_parser(const char* name, void* parser) {
    return parser;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        printf("Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Failed to read file %s\n", argv[1]);
        free(buffer);
        fclose(file);
        return 1;
    }

    fclose(file);

    // Initialize Hamer context
    void* ctx = malloc(sizeof(void*));
    int result = 0;

    // Call hammer_parse to parse the TLS Client Hello
    if (hammer_parse(ctx, NULL, buffer, file_size) == 0) {
        printf("Parsed TLS Client Hello successfully\n");
    } else {
        printf("Failed to parse TLS Client Hello\n");
    }

    free(buffer);
    free(ctx);
    return 0;
}