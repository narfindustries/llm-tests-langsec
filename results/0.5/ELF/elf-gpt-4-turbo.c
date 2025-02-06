#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define magic numbers and constants
#define EI_NIDENT 16
#define ET_NONE 0
#define ET_REL 1
#define ET_EXEC 2
#define ET_DYN 3
#define ET_CORE 4
#define ET_LOOS 0xFE00
#define ET_HIOS 0xFEFF
#define ET_LOPROC 0xFF00
#define ET_HIPROC 0xFFFF

// Parser declarations
static HParser *elf_header_parser;

HParser *init_elf_parser() {
    HParser *ei_magic = h_sequence(h_int8(), h_ch('E'), h_ch('L'), h_ch('F'), NULL);
    HParser *ei_class = h_int8();
    HParser *ei_data = h_int8();
    HParser *ei_version = h_int8();
    HParser *ei_osabi = h_int8();
    HParser *ei_abiversion = h_int8();
    HParser *ei_pad = h_repeat_n(h_int8(), 7);

    HParser *e_ident = h_sequence(ei_magic, ei_class, ei_data, ei_version, ei_osabi, ei_abiversion, ei_pad, NULL);
    HParser *e_type = h_int16();
    HParser *e_machine = h_int16();
    HParser *e_version = h_int32();
    HParser *e_entry = h_int32();
    HParser *e_phoff = h_int32();
    HParser *e_shoff = h_int32();
    HParser *e_flags = h_int32();
    HParser *e_ehsize = h_int16();
    HParser *e_phentsize = h_int16();
    HParser *e_phnum = h_int16();
    HParser *e_shentsize = h_int16();
    HParser *e_shnum = h_int16();
    HParser *e_shstrndx = h_int16();

    elf_header_parser = h_sequence(e_ident, e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags,
                                   e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx, NULL);

    return elf_header_parser;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ELF file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        fprintf(stderr, "Failed to read file\n");
        free(buffer);
        fclose(fp);
        return 1;
    }

    fclose(fp);

    HParser *elf_parser = init_elf_parser();
    HParseResult *result = h_parse(elf_parser, buffer, size);
    if (result) {
        printf("ELF parsed successfully\n");
    } else {
        printf("Failed to parse ELF\n");
    }

    free(buffer);
    h_parse_result_free(result);

    return 0;
}