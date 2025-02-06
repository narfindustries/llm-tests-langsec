#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <arpa/inet.h>
#include <string.h>

hammer_parser_t* parse_label(void) {
    hammer_parser_t* len = hammer_uint8;
    hammer_parser_t* data = hammer_bytes(hammer_len(len)); 
    return hammer_bind(len, data, (hammer_bind_func_t)hammer_take);
}

hammer_parser_t* parse_domain_name(void) {
    hammer_parser_t* parser = hammer_end_by(parse_label(), hammer_uint8(0));
    return parser;
}

hammer_parser_t* parse_uint16(void) {
    return hammer_map(hammer_uint16, (hammer_map_func_t) ntohs);
}

hammer_parser_t* parse_uint32(void) {
    return hammer_map(hammer_uint32, (hammer_map_func_t) ntohl);
}

hammer_parser_t* parse_flags(void) {
    return hammer_tuple(
        hammer_bits(1), 
        hammer_bits(4), 
        hammer_bits(1), 
        hammer_bits(1), 
        hammer_bits(1), 
        hammer_bits(1), 
        hammer_bits(3), 
        hammer_bits(4)  
    );
}

hammer_parser_t* parse_question(void) {
    return hammer_tuple(
        parse_domain_name(), 
        parse_uint16(),     
        parse_uint16()      
    );
}


hammer_parser_t* parse_rdata_a(void){
  return hammer_ipv4;
}

hammer_parser_t* parse_rdata_aaaa(void){
  return hammer_ipv6;
}

hammer_parser_t* parse_resource_record(void) {
    hammer_parser_t* type = parse_uint16();
    hammer_parser_t* rdlength = parse_uint16();
    hammer_parser_t* rdata_parser = hammer_choice(
        hammer_case(hammer_uint16(1), parse_rdata_a()), 
        hammer_case(hammer_uint16(28), parse_rdata_aaaa()), 
        hammer_default(hammer_bytes(hammer_len(rdlength))) 
    );

    return hammer_tuple(
        parse_domain_name(), 
        type,      
        parse_uint16(),      
        parse_uint32(),      
        rdlength,      
        rdata_parser
    );
}

hammer_parser_t* parse_dns_header(void) {
    return hammer_tuple(
        parse_uint16(), 
        parse_flags(),   
        parse_uint16(), 
        parse_uint16(), 
        parse_uint16(), 
        parse_uint16()  
    );
}

hammer_parser_t* parse_dns_message(void) {
    return hammer_tuple(
        parse_dns_header(),
        hammer_many(parse_question()), 
        hammer_many(parse_resource_record()), 
        hammer_many(parse_resource_record()), 
        hammer_many(parse_resource_record())  
    );
}


int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char* buffer = (char*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    hammer_parser_t* parser = parse_dns_message();
    hammer_result_t result = hammer_parse(parser, buffer, fileSize);

    if (result.success) {
        printf("DNS message parsed successfully!\n");
    } else {
        fprintf(stderr, "DNS message parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    hammer_free(parser);
    return 0;
}