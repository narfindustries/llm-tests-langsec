#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser* init_tiff_parser(void);
HParser* init_ifd_parser(void);
HParser* init_directory_entry_parser(void);

HParser* byte_order_parser(void) {
    return h_choice(h_token((uint8_t*)"II", 2),  // Little-endian
                   h_token((uint8_t*)"MM", 2),   // Big-endian
                   NULL);
}

HParser* version_parser(void) {
    return h_int_range(h_uint16(), 42, 42);
}

HParser* offset_parser(void) {
    return h_uint32();
}

HParser* byte_parser(void) {
    return h_uint8();
}

HParser* ascii_parser(void) {
    return h_many1(h_ch_range(0x00, 0x7F));
}

HParser* short_parser(void) {
    return h_uint16();
}

HParser* long_parser(void) {
    return h_uint32();
}

HParser* rational_parser(void) {
    return h_sequence(long_parser(), long_parser(), NULL);
}

HParser* sbyte_parser(void) {
    return h_int8();
}

HParser* undefined_parser(void) {
    return h_uint8();
}

HParser* sshort_parser(void) {
    return h_int16();
}

HParser* slong_parser(void) {
    return h_int32();
}

HParser* srational_parser(void) {
    return h_sequence(slong_parser(), slong_parser(), NULL);
}

HParser* tag_value_parser(uint16_t type, uint32_t count) {
    switch(type) {
        case 1: return h_repeat_n(byte_parser(), count);
        case 2: return h_repeat_n(ascii_parser(), count);
        case 3: return h_repeat_n(short_parser(), count);
        case 4: return h_repeat_n(long_parser(), count);
        case 5: return h_repeat_n(rational_parser(), count);
        case 6: return h_repeat_n(sbyte_parser(), count);
        case 7: return h_repeat_n(undefined_parser(), count);
        case 8: return h_repeat_n(sshort_parser(), count);
        case 9: return h_repeat_n(slong_parser(), count);
        case 10: return h_repeat_n(srational_parser(), count);
        default: return h_nothing_p();
    }
}

HParser* init_directory_entry_parser(void) {
    return h_sequence(
        short_parser(),  // Tag
        short_parser(),  // Type
        long_parser(),   // Count
        long_parser(),   // Value/Offset
        NULL
    );
}

HParser* init_ifd_parser(void) {
    return h_sequence(
        short_parser(),  // Entry count
        h_many1(init_directory_entry_parser()),  // Directory entries
        offset_parser(), // Next IFD offset
        NULL
    );
}

HParser* init_tiff_parser(void) {
    return h_sequence(
        byte_order_parser(),
        version_parser(),
        offset_parser(),
        init_ifd_parser(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, file_size, fp) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    HParser *tiff_parser = init_tiff_parser();
    
    HParseResult *result = h_parse(tiff_parser, buffer, file_size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse TIFF file\n");
        free(buffer);
        fclose(fp);
        return 1;
    }

    printf("Successfully parsed TIFF file\n");

    h_parse_result_free(result);
    free(buffer);
    fclose(fp);
    return 0;
}