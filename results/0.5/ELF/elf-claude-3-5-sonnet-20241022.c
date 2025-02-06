#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static const uint8_t ELF_MAGIC[] = {0x7f, 'E', 'L', 'F'};

HParser* elf_ident_parser() {
    return h_sequence(
        h_token((const uint8_t*)ELF_MAGIC, 4),
        h_choice(h_int_range(h_uint8(), 0, 2), NULL),  // EI_CLASS
        h_choice(h_int_range(h_uint8(), 0, 2), NULL),  // EI_DATA
        h_int_range(h_uint8(), 0, 1),                  // EI_VERSION
        h_int_range(h_uint8(), 0, 255),                // EI_OSABI
        h_uint8(),                                     // EI_ABIVERSION
        h_repeat_n(h_uint8(), 7),                      // EI_PAD
        NULL);
}

HParser* elf_header_parser() {
    return h_sequence(
        elf_ident_parser(),
        h_int_range(h_uint16(), 0, 0xffff),           // e_type
        h_int_range(h_uint16(), 0, 0xffff),           // e_machine
        h_int_range(h_uint32(), 0, 1),                // e_version
        h_uint64(),                                    // e_entry
        h_uint64(),                                    // e_phoff
        h_uint64(),                                    // e_shoff
        h_uint32(),                                    // e_flags
        h_uint16(),                                    // e_ehsize
        h_uint16(),                                    // e_phentsize
        h_uint16(),                                    // e_phnum
        h_uint16(),                                    // e_shentsize
        h_uint16(),                                    // e_shnum
        h_uint16(),                                    // e_shstrndx
        NULL);
}

HParser* program_header_parser() {
    return h_sequence(
        h_uint32(),                                    // p_type
        h_uint32(),                                    // p_flags
        h_uint64(),                                    // p_offset
        h_uint64(),                                    // p_vaddr
        h_uint64(),                                    // p_paddr
        h_uint64(),                                    // p_filesz
        h_uint64(),                                    // p_memsz
        h_uint64(),                                    // p_align
        NULL);
}

HParser* section_header_parser() {
    return h_sequence(
        h_uint32(),                                    // sh_name
        h_uint32(),                                    // sh_type
        h_uint64(),                                    // sh_flags
        h_uint64(),                                    // sh_addr
        h_uint64(),                                    // sh_offset
        h_uint64(),                                    // sh_size
        h_uint32(),                                    // sh_link
        h_uint32(),                                    // sh_info
        h_uint64(),                                    // sh_addralign
        h_uint64(),                                    // sh_entsize
        NULL);
}

HParser* symbol_table_entry_parser() {
    return h_sequence(
        h_uint32(),                                    // st_name
        h_uint8(),                                     // st_info
        h_uint8(),                                     // st_other
        h_uint16(),                                    // st_shndx
        h_uint64(),                                    // st_value
        h_uint64(),                                    // st_size
        NULL);
}

HParser* elf_parser() {
    return h_sequence(
        elf_header_parser(),
        h_many(program_header_parser()),
        h_many(section_header_parser()),
        h_many(symbol_table_entry_parser()),
        NULL);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, size, file) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* parser = elf_parser();
    HParseResult* result = h_parse(parser, buffer, size);

    if (result) {
        printf("Successfully parsed ELF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ELF file\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}