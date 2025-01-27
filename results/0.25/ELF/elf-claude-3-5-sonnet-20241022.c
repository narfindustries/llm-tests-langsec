#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ELF Header parsers
static HParser* e_ident_parser() {
    return h_sequence(
        h_ch('\x7f'),
        h_token((uint8_t*)"ELF", 3),
        h_uint8(),  // EI_CLASS
        h_uint8(),  // EI_DATA
        h_uint8(),  // EI_VERSION
        h_uint8(),  // EI_OSABI
        h_uint8(),  // EI_ABIVERSION
        h_repeat_n(h_uint8(), 7)  // EI_PAD
    );
}

static HParser* elf_header_parser() {
    return h_sequence(
        e_ident_parser(),
        h_uint16(),  // e_type
        h_uint16(),  // e_machine
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
        h_uint16()   // e_shstrndx
    );
}

// Program Header parser
static HParser* program_header_parser() {
    return h_sequence(
        h_uint32(),  // p_type
        h_uint32(),  // p_flags
        h_uint64(),  // p_offset
        h_uint64(),  // p_vaddr
        h_uint64(),  // p_paddr
        h_uint64(),  // p_filesz
        h_uint64(),  // p_memsz
        h_uint64()   // p_align
    );
}

// Section Header parser
static HParser* section_header_parser() {
    return h_sequence(
        h_uint32(),  // sh_name
        h_uint32(),  // sh_type
        h_uint64(),  // sh_flags
        h_uint64(),  // sh_addr
        h_uint64(),  // sh_offset
        h_uint64(),  // sh_size
        h_uint32(),  // sh_link
        h_uint32(),  // sh_info
        h_uint64(),  // sh_addralign
        h_uint64()   // sh_entsize
    );
}

// Complete ELF parser
static HParser* elf_parser() {
    return h_sequence(
        elf_header_parser(),
        h_many(program_header_parser()),
        h_many(section_header_parser())
    );
}

int main(int argc, char* argv[]) {
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
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* parser = elf_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ELF file\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    // Clean up
    h_parse_result_free(result);
    free(buffer);
    fclose(file);

    printf("Successfully parsed ELF file\n");
    return 0;
}