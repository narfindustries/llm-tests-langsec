#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

HParser *init_client_hello_parser(void) {
    HParser *legacy_version_p = h_uint16();
    HParser *random_p = h_repeat_n(h_uint8(), 32);
    HParser *legacy_session_id_p = h_length_value(h_uint8(), h_uint8());
    HParser *cipher_suite_p = h_uint16();
    HParser *cipher_suites_p = h_length_value(h_uint16(), h_repeat_n(cipher_suite_p, 2));
    HParser *compression_methods_p = h_length_value(h_uint8(), h_uint8());

    HParser *supported_versions_p = h_sequence(
        h_bits(16, 0x002b),
        h_length_value(h_uint16(), h_repeat_n(h_uint16(), 1)),
        NULL
    );

    HParser *signature_algorithms_p = h_sequence(
        h_bits(16, 0x000d),
        h_length_value(h_uint16(), h_repeat_n(h_uint16(), 1)),
        NULL
    );

    HParser *supported_groups_p = h_sequence(
        h_bits(16, 0x000a),
        h_length_value(h_uint16(), h_repeat_n(h_uint16(), 1)),
        NULL
    );

    HParser *key_share_entry_p = h_sequence(
        h_uint16(),
        h_length_value(h_uint16(), h_many1(h_uint8())),
        NULL
    );

    HParser *key_share_p = h_sequence(
        h_bits(16, 0x0033),
        h_length_value(h_uint16(), h_many1(key_share_entry_p)),
        NULL
    );

    HParser *server_name_p = h_sequence(
        h_bits(16, 0x0000),
        h_length_value(h_uint16(), h_many1(h_uint8())),
        NULL
    );

    HParser *psk_key_exchange_modes_p = h_sequence(
        h_bits(16, 0x002d),
        h_length_value(h_uint16(), h_uint8()),
        NULL
    );

    HParser *pre_shared_key_p = h_sequence(
        h_bits(16, 0x0029),
        h_length_value(h_uint16(), 
            h_sequence(
                h_length_value(h_uint16(), h_many1(h_uint8())),
                h_length_value(h_uint16(), h_many1(h_uint8())),
                NULL
            )
        ),
        NULL
    );

    HParser *early_data_p = h_sequence(
        h_bits(16, 0x002a),
        h_bits(16, 0x0000),
        NULL
    );

    HParser *cookie_p = h_sequence(
        h_bits(16, 0x002c),
        h_length_value(h_uint16(), h_many1(h_uint8())),
        NULL
    );

    HParser *padding_p = h_sequence(
        h_bits(16, 0x0015),
        h_length_value(h_uint16(), h_many1(h_ch(0x00))),
        NULL
    );

    HParser *extension_p = h_choice(
        supported_versions_p,
        signature_algorithms_p,
        supported_groups_p,
        key_share_p,
        server_name_p,
        psk_key_exchange_modes_p,
        pre_shared_key_p,
        early_data_p,
        cookie_p,
        padding_p,
        NULL
    );

    HParser *extensions_p = h_length_value(h_uint16(), h_many1(extension_p));

    return h_sequence(
        h_ch(0x01),           // HandshakeType
        h_uint8(),            // Length (3 bytes split)
        h_uint16(),
        legacy_version_p,     // Legacy version
        random_p,             // Random
        legacy_session_id_p,  // Legacy session ID
        cipher_suites_p,      // Cipher suites
        compression_methods_p, // Compression methods
        extensions_p,         // Extensions
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(data, 1, size, f) != size) {
        perror("Failed to read file");
        free(data);
        fclose(f);
        return 1;
    }

    fclose(f);

    HParser *parser = init_client_hello_parser();
    HParseResult *result = h_parse(parser, data, size);
    
    if (!result) {
        fprintf(stderr, "Failed to parse ClientHello\n");
        free(data);
        return 1;
    }

    h_parse_result_free(result);
    free(data);
    return 0;
}