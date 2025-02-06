#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Parser for fixed-length fields
HParser* legacy_version_p;
HParser* random_p;
HParser* legacy_session_id_p;
HParser* cipher_suites_p;
HParser* legacy_compression_p;

// Extension type parsers
HParser* supported_versions_p;
HParser* signature_algorithms_p;
HParser* supported_groups_p;
HParser* key_share_p;
HParser* server_name_p;
HParser* early_data_p;
HParser* psk_key_exchange_modes_p;
HParser* pre_shared_key_p;
HParser* cookie_p;
HParser* extension_p;
HParser* extensions_p;
HParser* client_hello_p;

void init_parsers() {
    // Fixed-length fields
    legacy_version_p = h_uint16();
    random_p = h_repeat_n(h_uint8(), 32);
    
    // Legacy session ID
    HParser* session_id_length = h_uint8();
    HParser* session_id_data = h_length_value(session_id_length, h_uint8());
    legacy_session_id_p = h_sequence(session_id_length, session_id_data, NULL);
    
    // Cipher suites
    HParser* cipher_suites_length = h_uint16();
    HParser* cipher_suite = h_uint16();
    HParser* cipher_suites_data = h_length_value(cipher_suites_length, cipher_suite);
    cipher_suites_p = h_sequence(cipher_suites_length, cipher_suites_data, NULL);
    
    // Legacy compression
    HParser* compression_length = h_uint8();
    HParser* compression_data = h_length_value(compression_length, h_uint8());
    legacy_compression_p = h_sequence(compression_length, compression_data, NULL);

    // Extension parsers
    supported_versions_p = h_sequence(
        h_uint16(), // Extension type 0x002b
        h_uint16(), // Length
        h_sequence(
            h_uint8(), // Length of supported versions
            h_repeat_n(h_uint16(), 1), // List of versions
            NULL
        ),
        NULL
    );

    signature_algorithms_p = h_sequence(
        h_uint16(), // Extension type 0x000d
        h_uint16(), // Length
        h_sequence(
            h_uint16(), // Length of algorithms
            h_many1(h_uint16()), // List of algorithms
            NULL
        ),
        NULL
    );

    supported_groups_p = h_sequence(
        h_uint16(), // Extension type 0x000a
        h_uint16(), // Length
        h_sequence(
            h_uint16(), // Length of groups
            h_many1(h_uint16()), // List of groups
            NULL
        ),
        NULL
    );

    key_share_p = h_sequence(
        h_uint16(), // Extension type 0x0033
        h_uint16(), // Length
        h_sequence(
            h_uint16(), // Length of key shares
            h_many1(h_sequence(
                h_uint16(), // Named group
                h_uint16(), // Key exchange length
                h_length_value(h_uint16(), h_uint8()), // Key exchange
                NULL
            )),
            NULL
        ),
        NULL
    );

    server_name_p = h_sequence(
        h_uint16(), // Extension type 0x0000
        h_uint16(), // Length
        h_sequence(
            h_uint16(), // List length
            h_sequence(
                h_uint8(),  // Name type
                h_uint16(), // Hostname length
                h_length_value(h_uint16(), h_uint8()), // Hostname
                NULL
            ),
            NULL
        ),
        NULL
    );

    early_data_p = h_sequence(
        h_uint16(), // Extension type 0x002a
        h_uint16(), // Length (0)
        NULL
    );

    psk_key_exchange_modes_p = h_sequence(
        h_uint16(), // Extension type 0x002d
        h_uint16(), // Length
        h_sequence(
            h_uint8(), // Length
            h_many1(h_uint8()), // Modes
            NULL
        ),
        NULL
    );

    pre_shared_key_p = h_sequence(
        h_uint16(), // Extension type 0x0029
        h_uint16(), // Length
        h_sequence(
            h_uint16(), // Identities length
            h_many1(h_sequence(
                h_length_value(h_uint16(), h_uint8()), // Identity
                h_uint32(), // Obfuscated ticket age
                NULL
            )),
            h_uint16(), // Binders length
            h_many1(h_length_value(h_uint8(), h_uint8())), // Binders
            NULL
        ),
        NULL
    );

    cookie_p = h_sequence(
        h_uint16(), // Extension type 0x002c
        h_uint16(), // Length
        h_length_value(h_uint16(), h_uint8()), // Cookie
        NULL
    );

    // Generic extension parser
    extension_p = h_choice(
        supported_versions_p,
        signature_algorithms_p,
        supported_groups_p,
        key_share_p,
        server_name_p,
        early_data_p,
        psk_key_exchange_modes_p,
        pre_shared_key_p,
        cookie_p,
        NULL
    );

    // Extensions wrapper
    HParser* extensions_length = h_uint16();
    HParser* extensions_data = h_length_value(extensions_length, h_many1(extension_p));
    extensions_p = h_sequence(extensions_length, extensions_data, NULL);

    // Complete ClientHello parser
    client_hello_p = h_sequence(
        legacy_version_p,
        random_p,
        legacy_session_id_p,
        cipher_suites_p,
        legacy_compression_p,
        extensions_p,
        NULL
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open input file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* input = malloc(size);
    if (fread(input, 1, size, f) != size) {
        perror("Failed to read input file");
        fclose(f);
        free(input);
        return 1;
    }
    fclose(f);

    init_parsers();

    HParseResult* result = h_parse(client_hello_p, input, size);
    if (result) {
        printf("Successfully parsed TLS ClientHello\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse TLS ClientHello\n");
    }

    free(input);
    return 0;
}