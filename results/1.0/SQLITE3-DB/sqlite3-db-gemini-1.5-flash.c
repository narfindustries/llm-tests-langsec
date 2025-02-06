#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

// Helper function to read a fixed-size value from the file
static uint64_t read_fixed(FILE *fp, size_t size) {
    uint64_t value = 0;
    fread(&value, size, 1, fp);
    return value;
}

// Helper function to read a variable-length string
static char* read_string(FILE *fp, size_t len) {
    char *str = (char*)malloc(len + 1);
    if (str == NULL) {
        perror("malloc");
        exit(1);
    }
    fread(str, len, 1, fp);
    str[len] = '\0';
    return str;
}


// Parser for the SQLite header
static hm_parser sqlite_header_parser() {
    return hm_seq(
        hm_string("SQLite format 3\000", 16),
        hm_uint16_t(),
        hm_uint16_t(),
        hm_uint32_t(),
        hm_uint32_t(),
        hm_uint32_t(),
        hm_uint32_t(),
        hm_uint32_t(),
        hm_uint32_t(),
        hm_end()
    );
}



//Simplified page parser (complexities omitted for brevity)

static hm_parser sqlite_page_parser(){
    return hm_seq(
        hm_uint32_t(), //page number
        hm_uint8_t(), //page type
        hm_uint16_t(), //freeblock offset
        hm_uint16_t(), //number of cells
        hm_uint16_t(), //number of free bytes
        hm_uint32_t(), //rightmost child page
        hm_bytes(6), //unused
        hm_end()
    );
}


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite_database_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("fopen");
        return 1;
    }

    //Parse Header
    hm_result header_result = hm_parse(sqlite_header_parser(), fp);
    if (!header_result.success) {
        fprintf(stderr, "Error parsing header: %s\n", header_result.error);
        fclose(fp);
        return 1;
    }

    //Print header (example)
    printf("Page Size: %u\n", *(uint16_t*)header_result.value[1]);



    //Parse a single page (example)  -  Needs significant expansion for real use
    hm_result page_result = hm_parse(sqlite_page_parser(), fp);
    if(!page_result.success){
        fprintf(stderr, "Error parsing page: %s\n", page_result.error);
        fclose(fp);
        return 1;
    }
    printf("Page Number: %u\n", *(uint32_t*)page_result.value[0]);


    fclose(fp);
    hm_free_result(header_result);
    hm_free_result(page_result);
    return 0;
}

