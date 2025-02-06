#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

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
} ELFHeader;

HParser *elf_parser() {
    HParser *e_ident = h_sequence(h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);
    HParser *e_class = h_choice(h_uint8(), h_uint8__m(0), h_uint8__m(1), h_uint8__m(2), NULL);
    HParser *e_data = h_choice(h_uint8(), h_uint8__m(0), h_uint8__m(1), h_uint8__m(2), NULL);
    HParser *e_version = h_choice(h_uint8(), h_uint8__m(0), h_uint8__m(1), NULL);
    HParser *e_osabi = h_uint8();
    HParser *e_abiversion = h_uint8();
    HParser *e_pad = h_repeat_n(h_uint8(), 7);
    HParser *e_type = h_uint16();
    HParser *e_machine = h_uint16();
    HParser *e_version_full = h_uint32();
    HParser *e_entry = h_uint64();
    HParser *e_phoff = h_uint64();
    HParser *e_shoff = h_uint64();
    HParser *e_flags = h_uint32();
    HParser *e_ehsize = h_uint16();
    HParser *e_phentsize = h_uint16();
    HParser *e_phnum = h_uint16();
    HParser *e_shentsize = h_uint16();
    HParser *e_shnum = h_uint16();
    HParser *e_shstrndx = h_uint16();

    HParser *elf_header = h_sequence(
        e_ident, e_class, e_data, e_version, e_osabi, e_abiversion, e_pad,
        e_type, e_machine, e_version_full, e_entry, e_phoff, e_shoff, e_flags,
        e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx,
        NULL
    );

    return elf_header;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = elf_parser();
    HParseResult *result = h_parse(parser, data, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse ELF file\n");
        free(data);
        return 1;
    }

    ELFHeader *header = (ELFHeader *)result->ast;
    printf("ELF Header parsed successfully\n");

    h_parse_result_free(result);
    free(data);
    return 0;
}