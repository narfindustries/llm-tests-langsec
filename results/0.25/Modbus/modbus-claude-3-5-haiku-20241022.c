#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t slave_address;
    uint8_t function_code;
    void* data;
    uint16_t crc;
} ModbusFrame;

HParser* parse_read_coils() {
    return h_sequence(
        h_uint16(),   
        h_uint16(),   
        NULL
    );
}

HParser* parse_read_discrete_inputs() {
    return h_sequence(
        h_uint16(),   
        h_uint16(),   
        NULL
    );
}

HParser* parse_read_holding_registers() {
    return h_sequence(
        h_uint16(),   
        h_uint16(),   
        NULL
    );
}

HParser* parse_read_input_registers() {
    return h_sequence(
        h_uint16(),   
        h_uint16(),   
        NULL
    );
}

HParser* parse_write_single_coil() {
    return h_sequence(
        h_uint16(),   
        h_uint16(),   
        NULL
    );
}

HParser* parse_write_single_register() {
    return h_sequence(
        h_uint16(),   
        h_uint16(),   
        NULL
    );
}

HParser* parse_write_multiple_coils() {
    return h_sequence(
        h_uint16(),   
        h_uint16(),   
        h_uint8(),    
        h_repeat_n(h_uint8(), -1),  
        NULL
    );
}

HParser* parse_write_multiple_registers() {
    return h_sequence(
        h_uint16(),   
        h_uint16(),   
        h_uint8(),    
        h_repeat_n(h_uint16(), -1),  
        NULL
    );
}

HParser* parse_file_record_read() {
    return h_sequence(
        h_uint8(),    
        h_uint16(),   
        h_uint16(),   
        h_uint16(),   
        NULL
    );
}

HParser* parse_modbus_frame() {
    return h_sequence(
        h_uint8(),    
        h_uint8(),    
        h_choice(
            parse_read_coils(),
            parse_read_discrete_inputs(),
            parse_read_holding_registers(),
            parse_read_input_registers(),
            parse_write_single_coil(),
            parse_write_single_register(),
            parse_write_multiple_coils(),
            parse_write_multiple_registers(),
            parse_file_record_read(),
            NULL
        ),
        h_uint16(),   
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <modbus_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        perror("Error reading file");
        free(buffer);
        return 1;
    }

    HParser *modbus_parser = parse_modbus_frame();
    HParseResult *result = h_parse(modbus_parser, buffer, bytes_read);

    if (result) {
        printf("Modbus frame parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    free(buffer);
    return 0;
}