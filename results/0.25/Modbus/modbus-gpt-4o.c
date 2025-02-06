#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_modbus_parser();

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = create_modbus_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result->ast) {
        printf("Parsing successful!\n");
    } else {
        printf("Parsing failed!\n");
    }

    h_parse_result_free(result);
    free(buffer);
    return EXIT_SUCCESS;
}

HParser *create_modbus_parser() {
    HParser *device_address_parser = h_uint8();
    HParser *function_code_parser = h_uint8();

    HParser *read_coils_parser = h_sequence(
        h_uint16(), // Starting Address
        h_uint16(), // Quantity of Coils
        NULL
    );

    HParser *read_discrete_inputs_parser = h_sequence(
        h_uint16(), // Starting Address
        h_uint16(), // Quantity of Inputs
        NULL
    );

    HParser *read_holding_registers_parser = h_sequence(
        h_uint16(), // Starting Address
        h_uint16(), // Quantity of Registers
        NULL
    );

    HParser *read_input_registers_parser = h_sequence(
        h_uint16(), // Starting Address
        h_uint16(), // Quantity of Registers
        NULL
    );

    HParser *write_single_coil_parser = h_sequence(
        h_uint16(), // Coil Address
        h_uint16(), // Coil Value
        NULL
    );

    HParser *write_single_register_parser = h_sequence(
        h_uint16(), // Register Address
        h_uint16(), // Register Value
        NULL
    );

    HParser *write_multiple_coils_parser = h_sequence(
        h_uint16(), // Starting Address
        h_uint16(), // Quantity of Coils
        h_uint8(),  // Byte Count
        h_repeat_n(h_uint8(), 1), // Coil Values (placeholder for actual count)
        NULL
    );

    HParser *write_multiple_registers_parser = h_sequence(
        h_uint16(), // Starting Address
        h_uint16(), // Quantity of Registers
        h_uint8(),  // Byte Count
        h_repeat_n(h_uint8(), 1), // Register Values (placeholder for actual count)
        NULL
    );

    HParser *exception_response_parser = h_sequence(
        h_uint8(), // Exception Code
        NULL
    );

    HParser *data_parser = h_choice(
        read_coils_parser,
        read_discrete_inputs_parser,
        read_holding_registers_parser,
        read_input_registers_parser,
        write_single_coil_parser,
        write_single_register_parser,
        write_multiple_coils_parser,
        write_multiple_registers_parser,
        exception_response_parser,
        NULL
    );

    HParser *crc_parser = h_uint16();

    return h_sequence(
        device_address_parser,
        function_code_parser,
        data_parser,
        crc_parser,
        NULL
    );
}