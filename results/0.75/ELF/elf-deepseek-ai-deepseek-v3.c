#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ELF header
H_PARSER(e_ident) {
    return h_sequence(h_byte(0x7F), h_char('E'), h_char('L'), h_char('F'), NULL);
}

H_PARSER(ei_class) {
    return h_choice(h_byte(0), h_byte(1), h_byte(2), NULL);
}

H_PARSER(ei_data) {
    return h_choice(h_byte(0), h_byte(1), h_byte(2), NULL);
}

H_PARSER(ei_version) {
    return h_choice(h_byte(0), h_byte(1), NULL);
}

H_PARSER(ei_osabi) {
    return h_byte(0); // Simplified for brevity
}

H_PARSER(ei_abiversion) {
    return h_byte(0); // Simplified for brevity
}

H_PARSER(ei_pad) {
    return h_many(h_byte(0), 7);
}

H_PARSER(e_type) {
    return h_uint16();
}

H_PARSER(e_machine) {
    return h_uint16();
}

H_PARSER(e_version) {
    return h_uint32();
}

H_PARSER(e_entry) {
    return h_uint64();
}

H_PARSER(e_phoff) {
    return h_uint64();
}

H_PARSER(e_shoff) {
    return h_uint64();
}

H_PARSER(e_flags) {
    return h_uint32();
}

H_PARSER(e_ehsize) {
    return h_uint16();
}

H_PARSER(e_phentsize) {
    return h_uint16();
}

H_PARSER(e_phnum) {
    return h_uint16();
}

H_PARSER(e_shentsize) {
    return h_uint16();
}

H_PARSER(e_shnum) {
    return h_uint16();
}

H_PARSER(e_shstrndx) {
    return h_uint16();
}

// Program header
H_PARSER(p_type) {
    return h_uint32();
}

H_PARSER(p_flags) {
    return h_uint32();
}

H_PARSER(p_offset) {
    return h_uint64();
}

H_PARSER(p_vaddr) {
    return h_uint64();
}

H_PARSER(p_paddr) {
    return h_uint64();
}

H_PARSER(p_filesz) {
    return h_uint64();
}

H_PARSER(p_memsz) {
    return h_uint64();
}

H_PARSER(p_align) {
    return h_uint64();
}

// Section header
H_PARSER(sh_name) {
    return h_uint32();
}

H_PARSER(sh_type) {
    return h_uint32();
}

H_PARSER(sh_flags) {
    return h_uint64();
}

H_PARSER(sh_addr) {
    return h_uint64();
}

H_PARSER(sh_offset) {
    return h_uint64();
}

H_PARSER(sh_size) {
    return h_uint64();
}

H_PARSER(sh_link) {
    return h_uint32();
}

H_PARSER(sh_info) {
    return h_uint32();
}

H_PARSER(sh_addralign) {
    return h_uint64();
}

H_PARSER(sh_entssize) {
    return h_uint64();
}

// Main parser
H_PARSER(elf_parser) {
    return h_sequence(
        e_ident(), ei_class(), ei_data(), ei_version(), ei_osabi(), ei_abiversion(), ei_pad(),
        e_type(), e_machine(), e_version(), e_entry(), e_phoff(), e_shoff(), e_flags(),
        e_ehsize(), e_phentsize(), e_phnum(), e_shentsize(), e_shnum(), e_shstrndx(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
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

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = elf_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse ELF file\n");
        free(buffer);
        return 1;
    }

    free(buffer);
    h_parse_result_free(result);
    return 0;
}