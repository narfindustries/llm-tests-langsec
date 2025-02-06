#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t transaction_id;
    uint16_t protocol_id;
    uint16_t length;
    uint8_t unit_id;
    uint8_t function_code;
    uint16_t starting_address;
    uint16_t quantity;
    uint8_t byte_count;
    uint8_t *data;
    uint16_t crc;
} ModbusFrame;

HParser *modbus_pdu_parser() {
    return h_sequence(
        h_int16(),
        h_int16(),
        h_int16(),
        h_uint8(),
        h_uint8(),
        h_int16(),
        h_int16(),
        h_uint8(),
        h_many(h_uint8()),
        h_int16(),
        NULL
    );
}

ModbusFrame *parse_modbus_frame(const uint8_t *data, size_t length) {
    HParseResult *result = h_parse(modbus_pdu_parser(), data, length);
    if (!result || !result->ast) {
        return NULL;
    }
    ModbusFrame *frame = malloc(sizeof(ModbusFrame));
    frame->transaction_id = result->ast->seq->elements[0]->uint;
    frame->protocol_id = result->ast->seq->elements[1]->uint;
    frame->length = result->ast->seq->elements[2]->uint;
    frame->unit_id = result->ast->seq->elements[3]->uint;
    frame->function_code = result->ast->seq->elements[4]->uint;
    frame->starting_address = result->ast->seq->elements[5]->uint;
    frame->quantity = result->ast->seq->elements[6]->uint;
    frame->byte_count = result->ast->seq->elements[7]->uint;
    frame->data = malloc(frame->byte_count);
    for (int i = 0; i < frame->byte_count; i++) {
        frame->data[i] = result->ast->seq->elements[8]->seq->elements[i]->uint;
    }
    frame->crc = result->ast->seq->elements[9]->uint;
    h_parse_result_free(result);
    return frame;
}

void free_modbus_frame(ModbusFrame *frame) {
    if (frame->data) {
        free(frame->data);
    }
    free(frame);
}

void print_modbus_frame(const ModbusFrame *frame) {
    printf("Transaction ID: %d\n", frame->transaction_id);
    printf("Protocol ID: %d\n", frame->protocol_id);
    printf("Length: %d\n", frame->length);
    printf("Unit ID: %d\n", frame->unit_id);
    printf("Function Code: %d\n", frame->function_code);
    printf("Starting Address: %d\n", frame->starting_address);
    printf("Quantity: %d\n", frame->quantity);
    printf("Byte Count: %d\n", frame->byte_count);
    printf("Data: ");
    for (int i = 0; i < frame->byte_count; i++) {
        printf("%02x ", frame->data[i]);
    }
    printf("\n");
    printf("CRC: %d\n", frame->crc);
}

int main(int argc, char **argv) {
    if (argc < 2) {
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
    rewind(file);
    uint8_t *data = malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);
    ModbusFrame *frame = parse_modbus_frame(data, file_size);
    if (!frame) {
        fprintf(stderr, "Failed to parse Modbus frame\n");
        free(data);
        return 1;
    }
    print_modbus_frame(frame);
    free_modbus_frame(frame);
    free(data);
    return 0;
}