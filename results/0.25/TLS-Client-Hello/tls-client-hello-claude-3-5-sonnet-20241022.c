#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser* init_client_hello_parser() {
    // Basic types
    HParser* uint8 = h_uint8();
    HParser* uint16 = h_uint16();
    
    // Legacy version (must be 0x0303)
    HParser* legacy_version = h_uint16();
    
    // Random (32 bytes)
    HParser* random = h_repeat_n(h_uint8(), 32);
    
    // Legacy session ID
    HParser* legacy_session_id = h_length_value(h_uint8(), h_uint8());
    
    // Cipher suites
    HParser* cipher_suite = h_uint16();
    HParser* cipher_suites = h_length_value(h_uint16(), h_many1(cipher_suite));
    
    // Legacy compression methods
    HParser* compression_methods = h_length_value(h_uint8(), h_uint8());
    
    // Extension parsers
    HParser* extension_type = h_uint16();
    HParser* extension_length = h_uint16();
    
    // Supported versions extension
    HParser* supported_version = h_uint16();
    HParser* supported_versions_ext = h_sequence(
        h_int_range(uint16, 0x002b, 0x002b),
        h_length_value(extension_length, 
            h_length_value(h_uint8(), supported_version)));
    
    // Supported groups extension
    HParser* supported_group = h_uint16();
    HParser* supported_groups_ext = h_sequence(
        h_int_range(uint16, 0x000a, 0x000a),
        h_length_value(extension_length,
            h_length_value(h_uint16(), h_many1(supported_group))));
    
    // Signature algorithms extension
    HParser* sig_alg = h_uint16();
    HParser* signature_algorithms_ext = h_sequence(
        h_int_range(uint16, 0x000d, 0x000d),
        h_length_value(extension_length,
            h_length_value(h_uint16(), h_many1(sig_alg))));
    
    // Key share extension
    HParser* key_share_entry = h_sequence(
        h_uint16(),  // group
        h_length_value(h_uint16(), h_uint8()));  // key exchange
    HParser* key_share_ext = h_sequence(
        h_int_range(uint16, 0x0033, 0x0033),
        h_length_value(extension_length,
            h_length_value(h_uint16(), h_many1(key_share_entry))));
    
    // Server name extension
    HParser* server_name_ext = h_sequence(
        h_int_range(uint16, 0x0000, 0x0000),
        h_length_value(extension_length,
            h_length_value(h_uint16(),
                h_sequence(
                    h_uint8(),  // name type
                    h_length_value(h_uint16(), h_uint8())))));
    
    // PSK modes extension
    HParser* psk_modes_ext = h_sequence(
        h_int_range(uint16, 0x002d, 0x002d),
        h_length_value(extension_length,
            h_length_value(h_uint8(), h_uint8())));
    
    // Pre-shared key extension
    HParser* psk_identity = h_length_value(h_uint16(), h_uint8());
    HParser* psk_binder = h_length_value(h_uint8(), h_uint8());
    HParser* pre_shared_key_ext = h_sequence(
        h_int_range(uint16, 0x0029, 0x0029),
        h_length_value(extension_length,
            h_sequence(
                h_length_value(h_uint16(), h_many1(psk_identity)),
                h_length_value(h_uint16(), h_many1(psk_binder)))));
    
    // Early data extension
    HParser* early_data_ext = h_sequence(
        h_int_range(uint16, 0x002a, 0x002a),
        h_int_range(uint16, 0x0000, 0x0000));
    
    // Cookie extension
    HParser* cookie_ext = h_sequence(
        h_int_range(uint16, 0x002c, 0x002c),
        h_length_value(extension_length, h_uint8()));
    
    // Padding extension
    HParser* padding_ext = h_sequence(
        h_int_range(uint16, 0x0015, 0x0015),
        h_length_value(extension_length, h_uint8()));
    
    // Any extension
    HParser* extension = h_choice(supported_versions_ext,
                                supported_groups_ext,
                                signature_algorithms_ext,
                                key_share_ext,
                                server_name_ext,
                                psk_modes_ext,
                                pre_shared_key_ext,
                                early_data_ext,
                                cookie_ext,
                                padding_ext,
                                NULL);
    
    // Extensions wrapper
    HParser* extensions = h_length_value(h_uint16(), h_many1(extension));
    
    // Complete Client Hello
    return h_sequence(
        legacy_version,
        random,
        legacy_session_id,
        cipher_suites,
        compression_methods,
        extensions,
        NULL);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(buffer, 1, size, f) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(f);
        return 1;
    }
    fclose(f);

    HParser* parser = init_client_hello_parser();
    HParseResult* result = h_parse(parser, buffer, size);

    if (result) {
        printf("Successfully parsed TLS Client Hello\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse TLS Client Hello\n");
    }

    free(buffer);
    return 0;
}