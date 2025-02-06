#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Define the TLS version structure
typedef struct {
    uint8_t major;
    uint8_t minor;
} tls_version_t;

// Define the random structure
typedef struct {
    uint8_t data[32];
} random_t;

// Define the cipher suite structure
typedef struct {
    uint16_t suite;
} cipher_suite_t;

// Define the compression method structure
typedef struct {
    uint8_t method;
} compression_method_t;

// Define the extension structure
typedef struct {
    uint16_t type;
    uint16_t length;
    uint8_t* data;
} extension_t;

// Define the supported versions extension structure
typedef struct {
    uint16_t length;
    tls_version_t* versions;
} supported_versions_extension_t;

// Define the key share extension structure
typedef struct {
    uint16_t length;
    uint8_t* data;
} key_share_extension_t;

// Define the pre shared key extension structure
typedef struct {
    uint16_t length;
    uint8_t* data;
} pre_shared_key_extension_t;

// Define the client hello structure
typedef struct {
    tls_version_t legacy_version;
    random_t random;
    uint8_t legacy_session_id_length;
    uint8_t* legacy_session_id;
    cipher_suite_t* cipher_suites;
    uint16_t cipher_suites_length;
    compression_method_t* compression_methods;
    uint16_t compression_methods_length;
    extension_t* extensions;
    uint16_t extensions_length;
} client_hello_t;

// Define the parser for the TLS version
int tls_version_parser(uint8_t** input, size_t* input_length, tls_version_t* output) {
    if (*input_length < 2) {
        return 0;
    }
    output->major = **input;
    (*input)++;
    (*input_length)--;
    output->minor = **input;
    (*input)++;
    (*input_length)--;
    return 1;
}

// Define the parser for the random structure
int random_parser(uint8_t** input, size_t* input_length, random_t* output) {
    if (*input_length < 32) {
        return 0;
    }
    memcpy(output->data, *input, 32);
    *input += 32;
    *input_length -= 32;
    return 1;
}

// Define the parser for the cipher suite structure
int cipher_suite_parser(uint8_t** input, size_t* input_length, cipher_suite_t* output) {
    if (*input_length < 2) {
        return 0;
    }
    output->suite = (**input << 8) | *((*input) + 1);
    *input += 2;
    *input_length -= 2;
    return 1;
}

// Define the parser for the compression method structure
int compression_method_parser(uint8_t** input, size_t* input_length, compression_method_t* output) {
    if (*input_length < 1) {
        return 0;
    }
    output->method = **input;
    (*input)++;
    (*input_length)--;
    return 1;
}

// Define the parser for the extension structure
int extension_parser(uint8_t** input, size_t* input_length, extension_t* output) {
    if (*input_length < 4) {
        return 0;
    }
    output->type = (**input << 8) | *((*input) + 1);
    *input += 2;
    *input_length -= 2;
    output->length = (**input << 8) | *((*input) + 1);
    *input += 2;
    *input_length -= 2;
    if (*input_length < output->length) {
        return 0;
    }
    output->data = *input;
    *input += output->length;
    *input_length -= output->length;
    return 1;
}

// Define the parser for the supported versions extension structure
int supported_versions_extension_parser(uint8_t** input, size_t* input_length, supported_versions_extension_t* output) {
    if (*input_length < 2) {
        return 0;
    }
    output->length = (**input << 8) | *((*input) + 1);
    *input += 2;
    *input_length -= 2;
    if (*input_length < output->length) {
        return 0;
    }
    output->versions = (tls_version_t*)*input;
    *input += output->length;
    *input_length -= output->length;
    return 1;
}

// Define the parser for the key share extension structure
int key_share_extension_parser(uint8_t** input, size_t* input_length, key_share_extension_t* output) {
    if (*input_length < 2) {
        return 0;
    }
    output->length = (**input << 8) | *((*input) + 1);
    *input += 2;
    *input_length -= 2;
    if (*input_length < output->length) {
        return 0;
    }
    output->data = *input;
    *input += output->length;
    *input_length -= output->length;
    return 1;
}

// Define the parser for the pre shared key extension structure
int pre_shared_key_extension_parser(uint8_t** input, size_t* input_length, pre_shared_key_extension_t* output) {
    if (*input_length < 2) {
        return 0;
    }
    output->length = (**input << 8) | *((*input) + 1);
    *input += 2;
    *input_length -= 2;
    if (*input_length < output->length) {
        return 0;
    }
    output->data = *input;
    *input += output->length;
    *input_length -= output->length;
    return 1;
}

// Define the parser for the client hello structure
int client_hello_parser(uint8_t** input, size_t* input_length, client_hello_t* output) {
    if (!tls_version_parser(input, input_length, &output->legacy_version)) {
        return 0;
    }
    if (!random_parser(input, input_length, &output->random)) {
        return 0;
    }
    if (*input_length < 1) {
        return 0;
    }
    output->legacy_session_id_length = **input;
    (*input)++;
    (*input_length)--;
    if (*input_length < output->legacy_session_id_length) {
        return 0;
    }
    output->legacy_session_id = *input;
    *input += output->legacy_session_id_length;
    *input_length -= output->legacy_session_id_length;
    if (*input_length < 2) {
        return 0;
    }
    uint16_t cipher_suites_length = (**input << 8) | *((*input) + 1);
    *input += 2;
    *input_length -= 2;
    if (*input_length < cipher_suites_length * 2) {
        return 0;
    }
    output->cipher_suites = (cipher_suite_t*)*input;
    output->cipher_suites_length = cipher_suites_length;
    *input += cipher_suites_length * 2;
    *input_length -= cipher_suites_length * 2;
    if (*input_length < 1) {
        return 0;
    }
    uint16_t compression_methods_length = **input;
    (*input)++;
    (*input_length)--;
    if (*input_length < compression_methods_length) {
        return 0;
    }
    output->compression_methods = (compression_method_t*)*input;
    output->compression_methods_length = compression_methods_length;
    *input += compression_methods_length;
    *input_length -= compression_methods_length;
    if (*input_length < 2) {
        return 0;
    }
    uint16_t extensions_length = (**input << 8) | *((*input) + 1);
    *input += 2;
    *input_length -= 2;
    if (*input_length < extensions_length) {
        return 0;
    }
    output->extensions = (extension_t*)*input;
    output->extensions_length = extensions_length / 4;
    *input += extensions_length;
    *input_length -= extensions_length;
    return 1;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    // Open the input file
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    // Read the input file into a buffer
    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    rewind(file);
    uint8_t* buffer = malloc(length);
    fread(buffer, 1, length, file);
    fclose(file);

    // Parse the client hello structure
    client_hello_t client_hello;
    if (client_hello_parser(&buffer, &length, &client_hello)) {
        printf("Legacy Version: %u.%u\n", client_hello.legacy_version.major, client_hello.legacy_version.minor);
        printf("Random: ");
        for (int i = 0; i < 32; i++) {
            printf("%02x", client_hello.random.data[i]);
        }
        printf("\n");
        printf("Legacy Session ID Length: %u\n", client_hello.legacy_session_id_length);
        printf("Legacy Session ID: ");
        for (int i = 0; i < client_hello.legacy_session_id_length; i++) {
            printf("%02x", client_hello.legacy_session_id[i]);
        }
        printf("\n");
        printf("Cipher Suites: ");
        for (int i = 0; i < client_hello.cipher_suites_length; i++) {
            printf("%04x ", client_hello.cipher_suites[i].suite);
        }
        printf("\n");
        printf("Compression Methods: ");
        for (int i = 0; i < client_hello.compression_methods_length; i++) {
            printf("%02x ", client_hello.compression_methods[i].method);
        }
        printf("\n");
        printf("Extensions: ");
        for (int i = 0; i < client_hello.extensions_length; i++) {
            printf("%04x ", client_hello.extensions[i].type);
        }
        printf("\n");
    } else {
        printf("Error parsing client hello\n");
    }

    free(buffer);
    return 0;
}