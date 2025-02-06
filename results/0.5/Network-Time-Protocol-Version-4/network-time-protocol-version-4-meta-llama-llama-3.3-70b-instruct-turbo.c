#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    uint8_t leap_indicator : 2;
    uint8_t version_number : 3;
    uint8_t mode : 3;
} ntp_header_t;

typedef struct {
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_clock_identifier;
    uint64_t reference_timestamp;
    uint64_t origin_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_timestamps_t;

typedef struct {
    ntp_header_t* header;
    ntp_timestamps_t* timestamps;
} ntp_packet_t;

typedef struct {
    uint16_t field_type;
    uint16_t field_length;
    uint8_t* data;
} ntp_extension_field_t;

#define NTP_MAX_EXTENSIONS 10

typedef struct {
    ntp_packet_t* packet;
    ntp_extension_field_t extensions[NTP_MAX_EXTENSIONS];
    uint8_t num_extensions;
} ntp_message_t;

#define NTP_LEAP_INDICATOR_NO_WARNING 0
#define NTP_LEAP_INDICATOR_LAST_MINUTE_HAS_61_SECONDS 1
#define NTP_LEAP_INDICATOR_LAST_MINUTE_HAS_59_SECONDS 2
#define NTP_LEAP_INDICATOR_ALARM_CONDITION 3

#define NTP_VERSION_NUMBER 4

#define NTP_MODE_RESERVED 0
#define NTP_MODE_SYMMETRIC_ACTIVE 1
#define NTP_MODE_SYMMETRIC_PASSIVE 2
#define NTP_MODE_CLIENT 3
#define NTP_MODE_SERVER 4
#define NTP_MODE_BROADCAST 5
#define NTP_MODE_RESERVED_FOR_NTP_CONTROL_MESSAGES 6
#define NTP_MODE_RESERVED_FOR_PRIVATE_USE 7

void* ntp_header_parser(void* input) {
    ntp_header_t* header = malloc(sizeof(ntp_header_t));
    uint8_t byte;
    if (fread(&byte, 1, 1, (FILE*)input) != 1) {
        free(header);
        return NULL;
    }
    header->leap_indicator = (byte >> 6) & 0x3;
    header->version_number = (byte >> 3) & 0x7;
    header->mode = byte & 0x7;
    return header;
}

void* ntp_timestamps_parser(void* input) {
    ntp_timestamps_t* timestamps = malloc(sizeof(ntp_timestamps_t));
    if (fread(&timestamps->poll, 1, 1, (FILE*)input) != 1 ||
        fread(&timestamps->precision, 1, 1, (FILE*)input) != 1 ||
        fread(&timestamps->root_delay, 4, 1, (FILE*)input) != 1 ||
        fread(&timestamps->root_dispersion, 4, 1, (FILE*)input) != 1 ||
        fread(&timestamps->reference_clock_identifier, 4, 1, (FILE*)input) != 1 ||
        fread(&timestamps->reference_timestamp, 8, 1, (FILE*)input) != 1 ||
        fread(&timestamps->origin_timestamp, 8, 1, (FILE*)input) != 1 ||
        fread(&timestamps->receive_timestamp, 8, 1, (FILE*)input) != 1 ||
        fread(&timestamps->transmit_timestamp, 8, 1, (FILE*)input) != 1) {
        free(timestamps);
        return NULL;
    }
    return timestamps;
}

void* ntp_extension_field_parser(void* input) {
    ntp_extension_field_t* extension = malloc(sizeof(ntp_extension_field_t));
    if (fread(&extension->field_type, 2, 1, (FILE*)input) != 1 ||
        fread(&extension->field_length, 2, 1, (FILE*)input) != 1) {
        free(extension);
        return NULL;
    }
    extension->data = malloc(extension->field_length);
    if (fread(extension->data, extension->field_length, 1, (FILE*)input) != 1) {
        free(extension->data);
        free(extension);
        return NULL;
    }
    return extension;
}

void* ntp_packet_parser(void* input) {
    ntp_packet_t* packet = malloc(sizeof(ntp_packet_t));
    packet->header = ntp_header_parser(input);
    if (!packet->header) {
        free(packet);
        return NULL;
    }
    packet->timestamps = ntp_timestamps_parser(input);
    if (!packet->timestamps) {
        free(packet->header);
        free(packet);
        return NULL;
    }
    return packet;
}

void* ntp_message_parser(void* input) {
    ntp_message_t* message = malloc(sizeof(ntp_message_t));
    message->packet = ntp_packet_parser(input);
    if (!message->packet) {
        free(message);
        return NULL;
    }
    for (int i = 0; i < NTP_MAX_EXTENSIONS; i++) {
        message->extensions[i].data = NULL;
    }
    message->num_extensions = 0;
    while (1) {
        ntp_extension_field_t* extension = ntp_extension_field_parser(input);
        if (!extension) {
            break;
        }
        if (message->num_extensions < NTP_MAX_EXTENSIONS) {
            message->extensions[message->num_extensions] = *extension;
            message->num_extensions++;
        } else {
            free(extension->data);
            free(extension);
            break;
        }
    }
    return message;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Could not open file %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);

    uint8_t* data = malloc(file_size);
    if (!data) {
        printf("Could not allocate memory\n");
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Could not read file %s\n", argv[1]);
        return 1;
    }

    fclose(file);

    ntp_message_t* message = ntp_message_parser(file);
    if (!message) {
        printf("Could not parse file %s\n", argv[1]);
        return 1;
    }

    printf("Leap Indicator: %u\n", message->packet->header->leap_indicator);
    printf("Version Number: %u\n", message->packet->header->version_number);
    printf("Mode: %u\n", message->packet->header->mode);
    printf("Poll: %u\n", message->packet->timestamps->poll);
    printf("Precision: %u\n", message->packet->timestamps->precision);
    printf("Root Delay: %u\n", message->packet->timestamps->root_delay);
    printf("Root Dispersion: %u\n", message->packet->timestamps->root_dispersion);
    printf("Reference Clock Identifier: %u\n", message->packet->timestamps->reference_clock_identifier);
    printf("Reference Timestamp: %llu\n", message->packet->timestamps->reference_timestamp);
    printf("Origin Timestamp: %llu\n", message->packet->timestamps->origin_timestamp);
    printf("Receive Timestamp: %llu\n", message->packet->timestamps->receive_timestamp);
    printf("Transmit Timestamp: %llu\n", message->packet->timestamps->transmit_timestamp);

    for (int i = 0; i < message->num_extensions; i++) {
        printf("Extension Field %d: Type %u, Length %u\n", i, message->extensions[i].field_type, message->extensions[i].field_length);
    }

    free(data);
    free(message->packet->header);
    free(message->packet->timestamps);
    free(message->packet);
    for (int i = 0; i < message->num_extensions; i++) {
        free(message->extensions[i].data);
    }
    free(message);

    return 0;
}