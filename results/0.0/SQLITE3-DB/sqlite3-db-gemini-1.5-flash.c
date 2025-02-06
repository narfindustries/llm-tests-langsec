#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Helper function to read a fixed-size value from the file
static uint64_t read_value(FILE *fp, size_t size) {
    uint64_t value = 0;
    fread(&value, size, 1, fp);
    return value;
}

// Helper function to read a string from the file
static char* read_string(FILE *fp, size_t size) {
    char* str = (char*)malloc(size + 1);
    fread(str, size, 1, fp);
    str[size] = '\0';
    return str;
}

// Parser for the SQLite header
static HammerParser sqlite_header_parser() {
    return seq(
        string_parser("SQLite format 3\000"),
        uint16_t_parser(),
        uint16_t_parser(),
        uint16_t_parser(),
        uint8_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint64_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        uint32_t_parser(),
        bytes_parser(20)
    );
}

//Simplified page header parser (omitting complex cell and payload parsing)
static HammerParser sqlite_page_header_parser() {
    return seq(
        uint32_t_parser(),
        uint8_t_parser(),
        uint8_t_parser(),
        uint16_t_parser(),
        uint16_t_parser(),
        uint32_t_parser()
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite_db_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    // Parse the SQLite header
    HammerResult header_result = hammer_parse(sqlite_header_parser(), fp);
    if (header_result.status != HAMMER_SUCCESS) {
        fprintf(stderr, "Error parsing header: %s\n", header_result.error);
        fclose(fp);
        return 1;
    }

    // Access parsed header values (example)
    uint16_t pageSize = *(uint16_t*)hammer_get_result_value(&header_result, 1);
    printf("Page Size: %u\n", pageSize);

    //Example of parsing a single page header.  Error handling omitted for brevity.
    HammerResult pageHeaderResult = hammer_parse(sqlite_page_header_parser(), fp);
    if (pageHeaderResult.status == HAMMER_SUCCESS) {
        uint32_t pageNumber = *(uint32_t*)hammer_get_result_value(&pageHeaderResult, 0);
        printf("Page Number: %u\n", pageNumber);
    }

    fclose(fp);
    hammer_free_result(&header_result);
    hammer_free_result(&pageHeaderResult);
    return 0;
}

