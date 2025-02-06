#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
HParser* init_icmp_parser(void);
HParser* create_echo_parser(void);
HParser* create_dest_unreachable_parser(void);
HParser* create_source_quench_parser(void);
HParser* create_redirect_parser(void);
HParser* create_time_exceeded_parser(void);
HParser* create_parameter_problem_parser(void);
HParser* create_timestamp_parser(void);
HParser* create_info_parser(void);

HParsedToken* act_noop(const HParseResult* p, void* user_data) {
    return (HParsedToken*)p->ast;
}

HParser* create_echo_parser(void) {
    return h_sequence(
        h_uint16(), // Identifier
        h_uint16(), // Sequence Number
        h_many(h_uint8()) // Data
    );
}

HParser* create_dest_unreachable_parser(void) {
    return h_sequence(
        h_uint32(), // Unused
        h_many(h_uint8()) // Internet Header + 64 bits of Original Data
    );
}

HParser* create_source_quench_parser(void) {
    return h_sequence(
        h_uint32(), // Unused
        h_many(h_uint8()) // Internet Header + 64 bits of Original Data
    );
}

HParser* create_redirect_parser(void) {
    return h_sequence(
        h_uint32(), // Gateway Internet Address
        h_many(h_uint8()) // Internet Header + 64 bits of Original Data
    );
}

HParser* create_time_exceeded_parser(void) {
    return h_sequence(
        h_uint32(), // Unused
        h_many(h_uint8()) // Internet Header + 64 bits of Original Data
    );
}

HParser* create_parameter_problem_parser(void) {
    return h_sequence(
        h_uint8(),  // Pointer
        h_uint8(), h_uint8(), h_uint8(), // 3 bytes unused (24 bits)
        h_many(h_uint8()) // Internet Header + 64 bits of Original Data
    );
}

HParser* create_timestamp_parser(void) {
    return h_sequence(
        h_uint16(), // Identifier
        h_uint16(), // Sequence Number
        h_uint32(), // Originate Timestamp
        h_uint32(), // Receive Timestamp
        h_uint32()  // Transmit Timestamp
    );
}

HParser* create_info_parser(void) {
    return h_sequence(
        h_uint16(), // Identifier
        h_uint16()  // Sequence Number
    );
}

HParser* init_icmp_parser(void) {
    // Type field parser with specific values
    HParser* type = h_int_range(h_uint8(), 0, 16);
    
    // Code field parser
    HParser* code = h_uint8();
    
    // Checksum field parser
    HParser* checksum = h_uint16();

    // Create type-specific data parser based on type
    HParser* type_specific_data = h_choice(
        h_action(h_sequence(h_ch(0), create_echo_parser()), act_noop, NULL),      // Echo Reply
        h_action(h_sequence(h_ch(3), create_dest_unreachable_parser()), act_noop, NULL),
        h_action(h_sequence(h_ch(4), create_source_quench_parser()), act_noop, NULL),
        h_action(h_sequence(h_ch(5), create_redirect_parser()), act_noop, NULL),
        h_action(h_sequence(h_ch(8), create_echo_parser()), act_noop, NULL),      // Echo Request
        h_action(h_sequence(h_ch(11), create_time_exceeded_parser()), act_noop, NULL),
        h_action(h_sequence(h_ch(12), create_parameter_problem_parser()), act_noop, NULL),
        h_action(h_sequence(h_ch(13), create_timestamp_parser()), act_noop, NULL),
        h_action(h_sequence(h_ch(14), create_timestamp_parser()), act_noop, NULL),
        h_action(h_sequence(h_ch(15), create_info_parser()), act_noop, NULL),
        h_action(h_sequence(h_ch(16), create_info_parser()), act_noop, NULL),
        NULL
    );

    return h_sequence(type, code, checksum, type_specific_data, NULL);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    HParser* parser = init_icmp_parser();
    HParseResult* result = h_parse(parser, buffer, size);

    if (result) {
        printf("Successfully parsed ICMP packet\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ICMP packet\n");
    }

    free(buffer);
    fclose(fp);
    return 0;
}