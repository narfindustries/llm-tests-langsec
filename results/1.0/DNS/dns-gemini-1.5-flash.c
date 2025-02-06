#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

//Helper function to read a domain name
static hm_result_t parse_domain_name(const uint8_t* buffer, size_t len, void* result) {
    char* domain_name = (char*)malloc(256); // Adjust size as needed
    size_t offset = 0;
    char* ptr = domain_name;
    uint8_t label_len;

    while (offset < len) {
        label_len = buffer[offset++];
        if (label_len > 63) return HM_ERROR; //Invalid label length
        if (label_len > 0) {
            memcpy(ptr, buffer + offset, label_len);
            ptr += label_len;
            *ptr++ = '.';
            offset += label_len;
        } else {
            break; // End of domain name
        }
    }
    ptr[-1] = '\0'; //Remove trailing dot
    *((char**)result) = domain_name;
    return HM_OK;
}

static hm_parser_t* create_domain_name_parser() {
    return hm_map(hm_bytes(parse_domain_name), hm_string());
}

// Define Hammer parsers for DNS header
static hm_parser_t* create_dns_header_parser() {
    return hm_seq(
        hm_uint16_le(), 
        hm_uint16_le(), 
        hm_uint16_le(), 
        hm_uint16_le(), 
        hm_uint16_le(), 
        hm_uint16_le()
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    rewind(file);

    uint8_t* buffer = (uint8_t*)malloc(fileSize);
    if (fread(buffer, 1, fileSize, file) != fileSize) {
        perror("Error reading file");
        fclose(file);
        free(buffer);
        return 1;
    }
    fclose(file);

    hm_parser_t* dns_header_parser = create_dns_header_parser();
    hm_parser_t* domain_name_parser = create_domain_name_parser();

    hm_result_t result = hm_parse(dns_header_parser, buffer, fileSize, NULL);

    if (result != HM_OK) {
        fprintf(stderr, "Error parsing DNS header: %s\n", hm_error_string(result));
        free(buffer);
        return 1;
    }

    free(buffer);
    return 0;
}

