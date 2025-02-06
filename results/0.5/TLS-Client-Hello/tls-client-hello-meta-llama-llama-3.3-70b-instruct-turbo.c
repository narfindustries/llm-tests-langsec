#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint16_t legacy_version;
    uint8_t random[32];
    uint8_t legacy_session_id_length;
    uint8_t legacy_session_id[32];
    uint16_t cipher_suites_length;
    uint16_t cipher_suites[256];
    uint8_t compression_methods_length;
    uint8_t compression_methods[256];
    uint16_t extensions_length;
    struct {
        uint16_t type;
        uint16_t length;
        uint8_t data[65535];
    } extensions[256];
} client_hello_t;

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file\n");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        printf("Failed to allocate memory\n");
        return 1;
    }

    size_t read_size = fread(data, 1, file_size, file);
    if (read_size != file_size) {
        printf("Failed to read file\n");
        return 1;
    }

    client_hello_t *hello = (client_hello_t *)data;

    printf("Legacy Version: %u\n", hello->legacy_version);
    printf("Random: ");
    for (int i = 0; i < 32; i++) {
        printf("%02x", hello->random[i]);
    }
    printf("\n");
    printf("Legacy Session ID Length: %u\n", hello->legacy_session_id_length);
    printf("Legacy Session ID: ");
    for (int i = 0; i < hello->legacy_session_id_length; i++) {
        printf("%02x", hello->legacy_session_id[i]);
    }
    printf("\n");
    printf("Cipher Suites Length: %u\n", hello->cipher_suites_length);
    printf("Cipher Suites: ");
    for (int i = 0; i < hello->cipher_suites_length / 2; i++) {
        printf("%04x ", hello->cipher_suites[i]);
    }
    printf("\n");
    printf("Compression Methods Length: %u\n", hello->compression_methods_length);
    printf("Compression Methods: ");
    for (int i = 0; i < hello->compression_methods_length; i++) {
        printf("%02x ", hello->compression_methods[i]);
    }
    printf("\n");
    printf("Extensions Length: %u\n", hello->extensions_length);
    for (int i = 0; i < hello->extensions_length; i++) {
        printf("Extension %u:\n", i);
        printf("Type: %04x\n", hello->extensions[i].type);
        printf("Length: %u\n", hello->extensions[i].length);
        printf("Data: ");
        for (int j = 0; j < hello->extensions[i].length; j++) {
            printf("%02x", hello->extensions[i].data[j]);
        }
        printf("\n");
    }

    free(data);
    fclose(file);
    return 0;
}