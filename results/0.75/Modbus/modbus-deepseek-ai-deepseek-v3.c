#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *modbus_parser() {
    HParser *transaction_id = h_int16();
    HParser *protocol_id = h_int16();
    HParser *length = h_int16();
    HParser *unit_id = h_int8();
    HParser *function_code = h_int8();
    HParser *data = h_many(h_int8());
    HParser *error_code = h_int8();
    HParser *checksum = h_int16();
    HParser *exception_status = h_int8();
    HParser *object_type = h_int8();
    HParser *object_value = h_many(h_int8());
    HParser *device_identification = h_many(h_int8());
    HParser *conformity_level = h_int8();
    HParser *more_follows = h_int8();
    HParser *next_object_id = h_int8();

    return h_sequence(
        transaction_id,
        protocol_id,
        length,
        unit_id,
        function_code,
        data,
        error_code,
        checksum,
        exception_status,
        object_type,
        object_value,
        device_identification,
        conformity_level,
        more_follows,
        next_object_id,
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
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

    HParser *parser = modbus_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}