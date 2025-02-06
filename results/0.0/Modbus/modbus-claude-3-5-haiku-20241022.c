#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t slave_address;
    uint8_t function_code;
    union {
        struct {
            uint16_t start_address;
            uint16_t quantity;
        } read_request;
        struct {
            uint16_t start_address;
            uint16_t quantity;
            uint8_t byte_count;
            uint8_t* data;
            size_t data_len;
        } write_request;
        struct {
            uint8_t exception_code;
        } error_response;
    } payload;
    uint16_t crc;
} ModbusFrame;

static HParser* modbus_parser;

static HParseResult* parse_modbus_frame(void* data, size_t len) {
    return h_parse(modbus_parser, data, len);
}

static HParsedToken* action_create_modbus_frame(const HParseResult* p, void* user_data) {
    ModbusFrame* frame = malloc(sizeof(ModbusFrame));
    
    frame->slave_address = p->ast->seq->elements[0]->uint;
    frame->function_code = p->ast->seq->elements[1]->uint;
    
    switch(frame->function_code) {
        case 0x01: // Read Coils
        case 0x02: // Read Discrete Inputs
        case 0x03: // Read Holding Registers
        case 0x04: // Read Input Registers
            frame->payload.read_request.start_address = p->ast->seq->elements[2]->seq->elements[0]->uint;
            frame->payload.read_request.quantity = p->ast->seq->elements[2]->seq->elements[1]->uint;
            break;
        
        case 0x05: // Write Single Coil
        case 0x06: // Write Single Register
            frame->payload.write_request.start_address = p->ast->seq->elements[2]->seq->elements[0]->uint;
            frame->payload.write_request.quantity = p->ast->seq->elements[2]->seq->elements[1]->uint;
            break;
        
        case 0x0F: // Write Multiple Coils
        case 0x10: // Write Multiple Registers
            frame->payload.write_request.start_address = p->ast->seq->elements[2]->seq->elements[0]->uint;
            frame->payload.write_request.quantity = p->ast->seq->elements[2]->seq->elements[1]->uint;
            frame->payload.write_request.byte_count = p->ast->seq->elements[2]->seq->elements[2]->uint;
            frame->payload.write_request.data = (uint8_t*)p->ast->seq->elements[2]->seq->elements[3]->bytes;
            frame->payload.write_request.data_len = p->ast->seq->elements[2]->seq->elements[3]->token_length;
            break;
        
        case 0x83: // Exception Response
            frame->payload.error_response.exception_code = p->ast->seq->elements[2]->seq->elements[0]->uint;
            break;
    }
    
    frame->crc = p->ast->seq->elements[3]->uint;
    
    HParsedToken* token = h_make_tagged_token(TT_USER, frame);
    return token;
}

static HParser* create_modbus_parser() {
    HParser* slave_address = h_uint8();
    HParser* function_code = h_uint8();
    
    HParser* read_request = h_sequence(
        h_uint16(),   // Start Address
        h_uint16(),   // Quantity
        NULL
    );
    
    HParser* write_single = h_sequence(
        h_uint16(),   // Address
        h_uint16(),   // Value
        NULL
    );
    
    HParser* write_multiple = h_sequence(
        h_uint16(),   // Start Address
        h_uint16(),   // Quantity
        h_uint8(),    // Byte Count
        h_repeat_n(h_uint8(), 1),  // Data
        NULL
    );
    
    HParser* exception_response = h_sequence(
        h_uint8(),    // Exception Code
        NULL
    );
    
    HParser* crc = h_uint16();
    
    HParser* modbus_frame = h_choice(
        h_sequence(
            slave_address,
            function_code,
            h_choice(
                read_request,     // 0x01, 0x02, 0x03, 0x04
                write_single,     // 0x05, 0x06
                write_multiple,   // 0x0F, 0x10
                exception_response // 0x83
            ),
            crc,
            NULL
        ),
        NULL
    );
    
    return h_action(modbus_frame, action_create_modbus_frame, NULL);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <modbus_binary_file>\n", argv[0]);
        return 1;
    }
    
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);
    
    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }
    
    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);
    
    modbus_parser = create_modbus_parser();
    HParseResult* result = parse_modbus_frame(buffer, read_size);
    
    if (result) {
        printf("Modbus frame parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }
    
    free(buffer);
    h_parser_free(modbus_parser);
    
    return 0;
}