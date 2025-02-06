#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct {
    uint8_t handshake_type;
    uint32_t length;
    uint16_t client_version;
    uint8_t random[32];
    struct {
        uint8_t length;
        uint8_t* session_id;
    } legacy_session_id;
    struct {
        uint16_t length;
        uint16_t* cipher_suites;
    } cipher_suites;
    uint8_t legacy_compression_methods_length;
    uint8_t* legacy_compression_methods;
    struct {
        uint16_t type;
        uint16_t length;
        uint8_t* data;
    }* extensions;
    uint16_t extensions_count;
} TLSClientHello;

static HParser* h_custom_uint24() {
    return h_bits(24, false);
}

static size_t h_custom_get_uint(const HParsedToken* token) {
    if (token && token->type == TT_UINT) {
        return token->val.uintval;
    }
    return 0;
}

static HParsedToken* parse_tls_client_hello(const uint8_t* data, size_t data_length) {
    HParser* handshake_type = h_token("\x01", 1);
    HParser* length = h_custom_uint24();
    HParser* client_version = h_uint16();
    HParser* random_parser = h_token(NULL, 32);
    
    HParser* session_id_length = h_uint8();
    HParser* session_id_parser = h_repeat_n(h_token(NULL, 1), 
        h_custom_get_uint(session_id_length));
    
    HParser* cipher_suites_length = h_uint16();
    HParser* cipher_suites_parser = h_repeat_n(h_uint16(), 
        h_custom_get_uint(cipher_suites_length) / 2);
    
    HParser* compression_methods_length = h_uint8();
    HParser* compression_methods_parser = h_repeat_n(h_uint8(), 
        h_custom_get_uint(compression_methods_length));

    HParser* extensions_count_parser = h_uint16();
    HParser* extension_parser = h_sequence(
        h_uint16(), 
        h_uint16(), 
        h_token(NULL, h_custom_get_uint(h_index_token(1, 1)))
    );
    HParser* extensions_parser = h_repeat_n(extension_parser, 
        h_custom_get_uint(extensions_count_parser));

    HParser* client_hello_parser = h_sequence(
        handshake_type,
        length,
        client_version,
        random_parser,
        session_id_length,
        session_id_parser,
        cipher_suites_length,
        cipher_suites_parser,
        compression_methods_length,
        compression_methods_parser,
        extensions_count_parser,
        extensions_parser
    );

    HParseResult* result = h_parse(client_hello_parser, data, data_length);
    return result ? result->ast : NULL;
}

void print_tls_client_hello(TLSClientHello* hello) {
    printf("Handshake Type: 0x%02x\n", hello->handshake_type);
    printf("Length: %d\n", hello->length);
    printf("Client Version: 0x%04x\n", hello->client_version);
    
    printf("Random: ");
    for(int i = 0; i < 32; i++) printf("%02x", hello->random[i]);
    printf("\n");
    
    printf("Session ID Length: %d\n", hello->legacy_session_id.length);
    
    printf("Cipher Suites Length: %d\n", hello->cipher_suites.length);
    for(int i = 0; i < hello->cipher_suites.length/2; i++) {
        printf("Cipher Suite %d: 0x%04x\n", i, hello->cipher_suites.cipher_suites[i]);
    }
    
    printf("Compression Methods Length: %d\n", hello->legacy_compression_methods_length);
    
    printf("Extensions Count: %d\n", hello->extensions_count);
}

int main(int argc, char** argv) {
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if(!file) {
        perror("Could not open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if(!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    size_t read_bytes = fread(buffer, 1, file_size, file);
    fclose(file);

    if(read_bytes != file_size) {
        perror("Could not read entire file");
        free(buffer);
        return 1;
    }

    HParseResult* result = h_parse_result_new();
    result->ast = parse_tls_client_hello(buffer, file_size);

    if(result->ast) {
        print_tls_client_hello((TLSClientHello*)result->ast);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Parsing failed\n");
    }

    free(buffer);
    return 0;
}