#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define MODBUS_MAX_DATA_LENGTH 256

typedef enum {
    MODBUS_FUNCTION_READ_COIL_STATUS = 0x01,
    MODBUS_FUNCTION_READ_INPUT_STATUS = 0x02,
    MODBUS_FUNCTION_READ_HOLDING_REGISTERS = 0x03,
    MODBUS_FUNCTION_READ_INPUT_REGISTERS = 0x04,
    MODBUS_FUNCTION_WRITE_SINGLE_COIL = 0x05,
    MODBUS_FUNCTION_WRITE_SINGLE_HOLDING_REGISTER = 0x06,
    MODBUS_FUNCTION_WRITE_MULTIPLE_COILS = 0x0F,
    MODBUS_FUNCTION_WRITE_MULTIPLE_HOLDING_REGISTERS = 0x10,
    MODBUS_FUNCTION_REPORT_SLAVE_ID = 0x11,
    MODBUS_FUNCTION_READ_FILE_RECORD = 0x14,
    MODBUS_FUNCTION_WRITE_FILE_RECORD = 0x15,
    MODBUS_FUNCTION_MASK_WRITE_REGISTER = 0x16,
    MODBUS_FUNCTION_READ_WRITE_MULTIPLE_REGISTERS = 0x17,
    MODBUS_FUNCTION_READ_FIFO_QUEUE = 0x18,
    MODBUS_FUNCTION_ENCAPSULATED_INTERFACE_TRANSPORT = 0x2B
} modbus_function_code_t;

typedef enum {
    MODBUS_EXCEPTION_ILLEGAL_FUNCTION = 0x01,
    MODBUS_EXCEPTION_ILLEGAL_DATA_ADDRESS = 0x02,
    MODBUS_EXCEPTION_ILLEGAL_DATA_VALUE = 0x03,
    MODBUS_EXCEPTION_SLAVE_DEVICE_FAILURE = 0x04,
    MODBUS_EXCEPTION_ACKNOWLEDGE = 0x05,
    MODBUS_EXCEPTION_SLAVE_DEVICE_BUSY = 0x06,
    MODBUS_EXCEPTION_NEGATIVE_ACKNOWLEDGE = 0x07,
    MODBUS_EXCEPTION_MEMORY_PARITY_ERROR = 0x08,
    MODBUS_EXCEPTION_GATEWAY_PATH_UNAVAILABLE = 0x0A,
    MODBUS_EXCEPTION_GATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND = 0x0B
} modbus_exception_code_t;

typedef struct {
    uint8_t address;
    uint8_t function_code;
    uint8_t* data;
    size_t data_length;
    uint16_t crc;
} modbus_message_t;

typedef struct {
    uint8_t address;
    uint8_t exception_code;
} modbus_exception_response_t;

HParser* modbus_address(void) {
    HParser* parser = H_CHOICE(
        H_LIT(0x00),
        H_LIT(0x01),
        H_LIT(0x02),
        H_LIT(0x03),
        H_LIT(0x04),
        H_LIT(0x05),
        H_LIT(0x06),
        H_LIT(0x07),
        H_LIT(0x08),
        H_LIT(0x09),
        H_LIT(0x0A),
        H_LIT(0x0B),
        H_LIT(0x0C),
        H_LIT(0x0D),
        H_LIT(0x0E),
        H_LIT(0x0F),
        H_LIT(0x10),
        H_LIT(0x11),
        H_LIT(0x12),
        H_LIT(0x13),
        H_LIT(0x14),
        H_LIT(0x15),
        H_LIT(0x16),
        H_LIT(0x17),
        H_LIT(0x18),
        H_LIT(0x19),
        H_LIT(0x1A),
        H_LIT(0x1B),
        H_LIT(0x1C),
        H_LIT(0x1D),
        H_LIT(0x1E),
        H_LIT(0x1F),
        H_LIT(0x20),
        H_LIT(0x21),
        H_LIT(0x22),
        H_LIT(0x23),
        H_LIT(0x24),
        H_LIT(0x25),
        H_LIT(0x26),
        H_LIT(0x27),
        H_LIT(0x28),
        H_LIT(0x29),
        H_LIT(0x2A),
        H_LIT(0x2B),
        H_LIT(0x2C),
        H_LIT(0x2D),
        H_LIT(0x2E),
        H_LIT(0x2F),
        H_LIT(0x30),
        H_LIT(0x31),
        H_LIT(0x32),
        H_LIT(0x33),
        H_LIT(0x34),
        H_LIT(0x35),
        H_LIT(0x36),
        H_LIT(0x37),
        H_LIT(0x38),
        H_LIT(0x39),
        H_LIT(0x3A),
        H_LIT(0x3B),
        H_LIT(0x3C),
        H_LIT(0x3D),
        H_LIT(0x3E),
        H_LIT(0x3F),
        H_LIT(0x40),
        H_LIT(0x41),
        H_LIT(0x42),
        H_LIT(0x43),
        H_LIT(0x44),
        H_LIT(0x45),
        H_LIT(0x46),
        H_LIT(0x47),
        H_LIT(0x48),
        H_LIT(0x49),
        H_LIT(0x4A),
        H_LIT(0x4B),
        H_LIT(0x4C),
        H_LIT(0x4D),
        H_LIT(0x4E),
        H_LIT(0x4F),
        H_LIT(0x50),
        H_LIT(0x51),
        H_LIT(0x52),
        H_LIT(0x53),
        H_LIT(0x54),
        H_LIT(0x55),
        H_LIT(0x56),
        H_LIT(0x57),
        H_LIT(0x58),
        H_LIT(0x59),
        H_LIT(0x5A),
        H_LIT(0x5B),
        H_LIT(0x5C),
        H_LIT(0x5D),
        H_LIT(0x5E),
        H_LIT(0x5F),
        H_LIT(0x60),
        H_LIT(0x61),
        H_LIT(0x62),
        H_LIT(0x63),
        H_LIT(0x64),
        H_LIT(0x65),
        H_LIT(0x66),
        H_LIT(0x67),
        H_LIT(0x68),
        H_LIT(0x69),
        H_LIT(0x6A),
        H_LIT(0x6B),
        H_LIT(0x6C),
        H_LIT(0x6D),
        H_LIT(0x6E),
        H_LIT(0x6F),
        H_LIT(0x70),
        H_LIT(0x71),
        H_LIT(0x72),
        H_LIT(0x73),
        H_LIT(0x74),
        H_LIT(0x75),
        H_LIT(0x76),
        H_LIT(0x77),
        H_LIT(0x78),
        H_LIT(0x79),
        H_LIT(0x7A),
        H_LIT(0x7B),
        H_LIT(0x7C),
        H_LIT(0x7D),
        H_LIT(0x7E),
        H_LIT(0x7F),
        H_LIT(0x80),
        H_LIT(0x81),
        H_LIT(0x82),
        H_LIT(0x83),
        H_LIT(0x84),
        H_LIT(0x85),
        H_LIT(0x86),
        H_LIT(0x87),
        H_LIT(0x88),
        H_LIT(0x89),
        H_LIT(0x8A),
        H_LIT(0x8B),
        H_LIT(0x8C),
        H_LIT(0x8D),
        H_LIT(0x8E),
        H_LIT(0x8F),
        H_LIT(0x90),
        H_LIT(0x91),
        H_LIT(0x92),
        H_LIT(0x93),
        H_LIT(0x94),
        H_LIT(0x95),
        H_LIT(0x96),
        H_LIT(0x97),
        H_LIT(0x98),
        H_LIT(0x99),
        H_LIT(0x9A),
        H_LIT(0x9B),
        H_LIT(0x9C),
        H_LIT(0x9D),
        H_LIT(0x9E),
        H_LIT(0x9F),
        H_LIT(0xA0),
        H_LIT(0xA1),
        H_LIT(0xA2),
        H_LIT(0xA3),
        H_LIT(0xA4),
        H_LIT(0xA5),
        H_LIT(0xA6),
        H_LIT(0xA7),
        H_LIT(0xA8),
        H_LIT(0xA9),
        H_LIT(0xAA),
        H_LIT(0xAB),
        H_LIT(0xAC),
        H_LIT(0xAD),
        H_LIT(0xAE),
        H_LIT(0xAF),
        H_LIT(0xB0),
        H_LIT(0xB1),
        H_LIT(0xB2),
        H_LIT(0xB3),
        H_LIT(0xB4),
        H_LIT(0xB5),
        H_LIT(0xB6),
        H_LIT(0xB7),
        H_LIT(0xB8),
        H_LIT(0xB9),
        H_LIT(0xBA),
        H_LIT(0xBB),
        H_LIT(0xBC),
        H_LIT(0xBD),
        H_LIT(0xBE),
        H_LIT(0xBF),
        H_LIT(0xC0),
        H_LIT(0xC1),
        H_LIT(0xC2),
        H_LIT(0xC3),
        H_LIT(0xC4),
        H_LIT(0xC5),
        H_LIT(0xC6),
        H_LIT(0xC7),
        H_LIT(0xC8),
        H_LIT(0xC9),
        H_LIT(0xCA),
        H_LIT(0xCB),
        H_LIT(0xCC),
        H_LIT(0xCD),
        H_LIT(0xCE),
        H_LIT(0xCF),
        H_LIT(0xD0),
        H_LIT(0xD1),
        H_LIT(0xD2),
        H_LIT(0xD3),
        H_LIT(0xD4),
        H_LIT(0xD5),
        H_LIT(0xD6),
        H_LIT(0xD7),
        H_LIT(0xD8),
        H_LIT(0xD9),
        H_LIT(0xDA),
        H_LIT(0xDB),
        H_LIT(0xDC),
        H_LIT(0xDD),
        H_LIT(0xDE),
        H_LIT(0xDF),
        H_LIT(0xE0),
        H_LIT(0xE1),
        H_LIT(0xE2),
        H_LIT(0xE3),
        H_LIT(0xE4),
        H_LIT(0xE5),
        H_LIT(0xE6),
        H_LIT(0xE7),
        H_LIT(0xE8),
        H_LIT(0xE9),
        H_LIT(0xEA),
        H_LIT(0xEB),
        H_LIT(0xEC),
        H_LIT(0xED),
        H_LIT(0xEE),
        H_LIT(0xEF),
        H_LIT(0xF0),
        H_LIT(0xF1),
        H_LIT(0xF2),
        H_LIT(0xF3),
        H_LIT(0xF4),
        H_LIT(0xF5),
        H_LIT(0xF6),
        H_LIT(0xF7)
    );
    return parser;
}

HParser* modbus_function_code(void) {
    HParser* parser = H_CHOICE(
        H_LIT(MODBUS_FUNCTION_READ_COIL_STATUS),
        H_LIT(MODBUS_FUNCTION_READ_INPUT_STATUS),
        H_LIT(MODBUS_FUNCTION_READ_HOLDING_REGISTERS),
        H_LIT(MODBUS_FUNCTION_READ_INPUT_REGISTERS),
        H_LIT(MODBUS_FUNCTION_WRITE_SINGLE_COIL),
        H_LIT(MODBUS_FUNCTION_WRITE_SINGLE_HOLDING_REGISTER),
        H_LIT(MODBUS_FUNCTION_WRITE_MULTIPLE_COILS),
        H_LIT(MODBUS_FUNCTION_WRITE_MULTIPLE_HOLDING_REGISTERS),
        H_LIT(MODBUS_FUNCTION_REPORT_SLAVE_ID),
        H_LIT(MODBUS_FUNCTION_READ_FILE_RECORD),
        H_LIT(MODBUS_FUNCTION_WRITE_FILE_RECORD),
        H_LIT(MODBUS_FUNCTION_MASK_WRITE_REGISTER),
        H_LIT(MODBUS_FUNCTION_READ_WRITE_MULTIPLE_REGISTERS),
        H_LIT(MODBUS_FUNCTION_READ_FIFO_QUEUE),
        H_LIT(MODBUS_FUNCTION_ENCAPSULATED_INTERFACE_TRANSPORT)
    );
    return parser;
}

HParser* modbus_data(void) {
    HParser* parser = H_REP(MODBUS_MAX_DATA_LENGTH, H_BYTE);
    return parser;
}

HParser* modbus_crc(void) {
    HParser* parser = H_TUPLE(H_BYTE, H_BYTE);
    return parser;
}

HParser* modbus_message(void) {
    HParser* parser = H_TUPLE(modbus_address(), modbus_function_code(), modbus_data(), modbus_crc());
    return parser;
}

HParser* modbus_exception_code(void) {
    HParser* parser = H_CHOICE(
        H_LIT(MODBUS_EXCEPTION_ILLEGAL_FUNCTION),
        H_LIT(MODBUS_EXCEPTION_ILLEGAL_DATA_ADDRESS),
        H_LIT(MODBUS_EXCEPTION_ILLEGAL_DATA_VALUE),
        H_LIT(MODBUS_EXCEPTION_SLAVE_DEVICE_FAILURE),
        H_LIT(MODBUS_EXCEPTION_ACKNOWLEDGE),
        H_LIT(MODBUS_EXCEPTION_SLAVE_DEVICE_BUSY),
        H_LIT(MODBUS_EXCEPTION_NEGATIVE_ACKNOWLEDGE),
        H_LIT(MODBUS_EXCEPTION_MEMORY_PARITY_ERROR),
        H_LIT(MODBUS_EXCEPTION_GATEWAY_PATH_UNAVAILABLE),
        H_LIT(MODBUS_EXCEPTION_GATEWAY_TARGET_DEVICE_FAILED_TO_RESPOND)
    );
    return parser;
}

HParser* modbus_exception_response(void) {
    HParser* parser = H_TUPLE(modbus_address(), H_LIT(0x80), modbus_exception_code());
    return parser;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        fclose(file);
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    HParser* parser = modbus_message();
    HCaseResult result = H_PARSE(parser, data, file_size);
    if (H_IS_OK(result)) {
        modbus_message_t* message = (modbus_message_t*)result.value;
        printf("Address: 0x%02X\n", message->address);
        printf("Function Code: 0x%02X\n", message->function_code);
        printf("Data: ");
        for (size_t i = 0; i < message->data_length; i++) {
            printf("0x%02X ", message->data[i]);
        }
        printf("\n");
        printf("CRC: 0x%04X\n", message->crc);
    } else {
        parser = modbus_exception_response();
        result = H_PARSE(parser, data, file_size);
        if (H_IS_OK(result)) {
            modbus_exception_response_t* response = (modbus_exception_response_t*)result.value;
            printf("Address: 0x%02X\n", response->address);
            printf("Exception Code: 0x%02X\n", response->exception_code);
        } else {
            printf("Error parsing message\n");
        }
    }

    free(data);
    return 0;
}