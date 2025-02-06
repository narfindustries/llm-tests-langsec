#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *http_request() {
    return h_sequence(
        h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch(' '))), h_end_p()
    );
}

HParser *http_response() {
    return h_sequence(
        h_many1(h_choice(h_ch_range('a', 'z'), h_ch_range('A', 'Z'), h_ch(' '))), h_end_p()
    );
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(http_request(), buffer, file_size);
    if (!result) {
        result = h_parse(http_response(), buffer, file_size);
    }

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}