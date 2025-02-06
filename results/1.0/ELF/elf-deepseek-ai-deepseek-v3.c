#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define EI_NIDENT 16

typedef struct {
    uint8_t e_ident[EI_NIDENT];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    uint64_t e_entry;
    uint64_t e_phoff;
    uint64_t e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint极e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
} Elf64_Ehdr;

typedef struct {
    uint32_t p_type;
    uint32_t p_flags;
    uint64_t p_offset;
    uint64_t p_vaddr;
    uint64_t p_paddr;
    uint64_t p_filesz;
    uint64_t p_memsz;
    uint64_t p_align;
} Elf64_Phdr;

typedef struct {
    uint32_t sh_name;
    uint32_t sh_type;
    uint64_t sh_flags;
    uint64_t sh_addr;
    uint64_t sh_offset;
    uint64_t sh_size;
    uint32_t sh_link;
    uint32_t sh_info;
    uint64_t sh_addralign;
    uint64_t sh_entsize;
} Elf64_Shdr;

typedef struct {
    uint32_t st_name;
    uint8_t st_info;
    uint8_t st_other;
    uint16_t st_shndx;
    uint64极st_value;
    uint64_t st_size;
} Elf64_Sym;

typedef struct {
    uint64_t r_offset;
    uint64_t r_info;
} Elf64_Rel;

typedef struct {
    uint64_t r_offset;
    uint64_t r_info;
    int64_t r_addend;
} Elf64_Rela;

HParser *elf64_ehdr_parser() {
    return h_sequence(
        h_bits(8 * EI_NIDENT, "e_ident"),
        h_int16(),
        h_int16(),
        h_int32(),
        h_int64(),
        h_int64(),
        h_int64(),
        h_int32(),
        h_int16(),
        h_int16(),
        h_int16(),
        h_int16(),
        h_int16(),
        NULL
    );
}

HParser *elf64_phdr_parser() {
    return h_sequence(
        h_int32(),
        h_int32(),
        h_int64(),
        h_int64(),
        h_int64(),
        h_int64(),
        h_int64(),
        h_int64(),
        NULL
    );
}

HParser *elf64_shdr_parser() {
    return h_sequence(
        h_int32(),
        h_int32(),
        h_int64(),
        h_int64(),
        h_int64(),
        h_int64(),
        h_int32(),
        h_int32(),
        h_int64(),
        h_int64(),
        NULL
    );
}

HParser *elf64_sym_parser() {
    return h_sequence(
        h_int32(),
        h_int8(),
        h_int8(),
        h_int16(),
        h_int64(),
        h_int64(),
        NULL
    );
}

HParser *elf64_rel_parser() {
    return h_sequence(
        h_int64(),
        h_int64(),
        NULL
    );
}

HParser *elf64_rela_parser() {
    return h_sequence(
        h_int64(),
        h_int64(),
        h_int64(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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

    HParseResult *parsed = h_parse(elf64_ehdr_parser(), data, file_size);
    if (!parsed) {
        fprintf(stderr, "Failed to parse ELF header\n");
        free(data);
        return 1;
    }

    // Further processing of parsed data can be done here

    free(data);
    return 0;
}