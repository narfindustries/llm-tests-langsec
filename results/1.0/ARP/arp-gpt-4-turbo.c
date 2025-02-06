#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <arp_packet_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("File opening failed");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fsize = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(fsize);
    size_t read_size = fread(buffer, 1, fsize, file);
    fclose(file);

    if (read_size != fsize) {
        fprintf(stderr, "Error reading file: read size differs from file size\n");
        free(buffer);
        return 1;
    }

    // ARP packet fields declaration
    HParser *hw_type = h_uint16();
    HParser *proto_type = h_uint16();
    HParser *hw_addr_len = h_uint8();
    HParser *proto_addr_len = h_uint8();
    HParser *operation = h_uint16();
    HParser *sender_hw_addr = h_bits(48, false);
    HParser *sender_proto_addr = h_bits(32, false);
    HParser *target_hw_addr = h_bits(48, false);
    HParser *target_proto_addr = h_bits(32, false);

    HParser *arp_parser = h_sequence(hw_type, proto_type, hw_addr_len, proto_addr_len,
                                     operation, sender_hw_addr, sender_proto_addr,
                                     target_hw_addr, target_proto_addr, NULL);

    HParseResult *parsed = h_parse(arp_parser, buffer, fsize);
    if (parsed && parsed->ast) {
        printf("ARP Packet parsed successfully:\n");
        printf("Hardware Type: %u\n", *(uint16_t*)h_value_ptr(parsed, 0));
        printf("Protocol Type: 0x%X\n", *(uint16_t*)h_value_ptr(parsed, 1));
        printf("Hardware Address Length: %u\n", *(uint8_t*)h_value_ptr(parsed, 2));
        printf("Protocol Address Length: %u\n", *(uint8_t*)h_value_ptr(parsed, 3));
        printf("Operation: %u\n", *(uint16_t*)h_value_ptr(parsed, 4));

        printf("Sender Hardware Address: ");
        uint8_t *sender_hw_data = (uint8_t*)h_value_ptr(parsed, 5);
        for (int i = 0; i < 6; i++)
            printf("%02X", sender_hw_data[i]);

        printf("\nSender Protocol Address: ");
        uint8_t *sender_proto_data = (uint8_t*)h_value_ptr(parsed, 6);
        for (int i = 0; i < 4; i++)
            printf("%d.", sender_proto_data[i]);

        printf("\nTarget Hardware Address: ");
        uint8_t *target_hw_data = (uint8_t*)h_value_ptr(parsed, 7);
        for (int i = 0; i < 6; i++)
            printf("%02X", target_hw_data[i]);

        printf("\nTarget Protocol Address: ");
        uint8_t *target_proto_data = (uint8_t*)h_value_ptr(parsed, 8);
        for (int i = 0; i < 4; i++)
            printf("%d.", target_proto_data[i]);

        printf("\n");
    } else {
        fprintf(stderr, "Failed to parse ARP packet.\n");
    }

    h_parse_result_free(parsed);
    h_parser_unref(arp_parser);
    free(buffer);
    return 0;
}