#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

// ELF Header Structures
typedef struct {
    uint8_t e_ident[16];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    uint64_t e_entry;
    uint64_t e_phoff;
    uint64_t e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint16_t e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
} Elf64_Ehdr;

// Hammer parser definitions
static HParser *elf_magic_number;
static HParser *elf_class;
static HParser *elf_data_encoding;
static HParser *elf_version;
static HParser *elf_abi;
static HParser *elf_abi_version;
static HParser *elf_pad;
static HParser *elf_type;
static HParser *elf_machine;
static HParser *elf_header_parser;

// Initialize Hammer parsers
void init_elf_parsers() {
    elf_magic_number = h_token("\x7fELF", 4);
    elf_class = h_choice(
        h_token("\x01", 1),  // 32-bit
        h_token("\x02", 1),  // 64-bit
        NULL
    );
    elf_data_encoding = h_choice(
        h_token("\x01", 1),  // Little endian
        h_token("\x02", 1),  // Big endian
        NULL
    );
    elf_version = h_token("\x01", 1);
    elf_abi = h_uint8();
    elf_abi_version = h_uint8();
    elf_pad = h_repeat_n(h_uint8(), 7);
    elf_type = h_uint16();
    elf_machine = h_uint16();

    elf_header_parser = h_sequence(
        elf_magic_number,
        elf_class,
        elf_data_encoding,
        elf_version,
        elf_abi,
        elf_abi_version,
        elf_pad,
        elf_type,
        elf_machine,
        h_uint32(),  // e_version
        h_uint64(),  // e_entry
        h_uint64(),  // e_phoff
        h_uint64(),  // e_shoff
        h_uint32(),  // e_flags
        h_uint16(),  // e_ehsize
        h_uint16(),  // e_phentsize
        h_uint16(),  // e_phnum
        h_uint16(),  // e_shentsize
        h_uint16(),  // e_shnum
        h_uint16(),  // e_shstrndx
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    init_elf_parsers();

    HParseResult *result = h_parse(elf_header_parser, buffer, read_size);
    if (result && result->ast) {
        printf("ELF file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("ELF file parsing failed\n");
    }

    free(buffer);
    return 0;
}