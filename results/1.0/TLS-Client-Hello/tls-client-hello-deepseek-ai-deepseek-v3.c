#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// ProtocolVersion (legacy_version)
HParser *protocol_version_parser() {
    return h_uint16();
}

// Random (random)
HParser *random_parser() {
    return h_repeat_n(h_uint8(), 32);
}

// LegacySessionID (legacy_session_id)
HParser *legacy_session_id_parser() {
    return h_length_value(h_uint8(), h_repeat_n(h_uint8(), 32));
}

// CipherSuites (cipher_suites)
HParser *cipher_suites_parser() {
    return h_length_value(h_uint16(), h_repeat_n(h_uint16(), 65535 / 2));
}

// LegacyCompressionMethods (legacy_compression_methods)
HParser *legacy_compression_methods_parser() {
    return h_length_value(h_uint8(), h_repeat_n(h_uint8(), 1));
}

// Extensions SupportedVersions (43)
HParser *supported_versions_parser() {
    return h_sequence(
        h_uint16(),  // Extension type
        h_length_value(h_uint16(), h_repeat_n(h_uint16(), 255 / 2)), NULL);
}

// Extensions SupportedGroups (10)
HParser *supported_groups_parser() {
    return h_sequence(
        h_uint16(),  // Extension type
        h_length_value(h_uint16(), h_repeat_n(h_uint16(), 255 / 2)), NULL);
}

// Extensions KeyShare (51)
HParser *key_share_parser() {
    return h_sequence(
        h_uint16(),  // Extension type
        h_length_value(h_uint16(), h_repeat_n(h_uint8(), 255)), NULL);
}

// Extensions SignatureAlgorithms (13)
HParser *signature_algorithms_parser() {
    return h_sequence(
        h_uint16(),  // Extension type
        h_length_value(h_uint16(), h_repeat_n(h_uint16(), 255 / 2)), NULL);
}

// Extensions ServerName (0)
HParser *server_name_parser() {
    return h_sequence(
        h_uint16(),  // Extension type
        h_length_value(h_uint16(), h_repeat_n(h_uint8(), 255)), NULL);
}

// Extensions PSKKeyExchangeModes (45)
HParser *psk_key_exchange_modes_parser() {
    return h_sequence(
        h_uint16(),  // Extension type
        h_length_value(h_uint8(), h_repeat_n(h_uint8(), 255)), NULL);
}

// Extensions PreSharedKey (41)
HParser *pre_shared_key_parser() {
    return h_sequence(
        h_uint16(),  // Extension type
        h_length_value(h_uint16(), h_repeat_n(h_uint8(), 255)), NULL);
}

// Extensions
HParser *extensions_parser() {
    return h_length_value(h_uint16(), h_repeat_n(h_sequence(
        h_choice(
            supported_versions_parser(),
            supported_groups_parser(),
            key_share_parser(),
            signature_algorithms_parser(),
            server_name_parser(),
            psk_key_exchange_modes_parser(),
            pre_shared_key_parser(),
            NULL
        ),
        NULL
    ), 65535));
}

// ClientHello
HParser *client_hello_parser() {
    return h_sequence(
        protocol_version_parser(),
        random_parser(),
        legacy_session_id_parser(),
        cipher_suites_parser(),
        legacy_compression_methods_parser(),
        extensions_parser(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(data, 1, file_size, fp) != file_size) {
        perror("Failed to read file");
        free(data);
        fclose(fp);
        return 1;
    }

    fclose(fp);

    HParser *parser = client_hello_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}