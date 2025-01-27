#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the ELF header
static HParser *elf_header() {
    return h_sequence(
        h_bits(8, false), // e_ident[EI_MAG0]
        h_bits(8, false), // e_ident[EI_MAG1]
        h_bits(8, false), // e_ident[EI_MAG2]
        h_bits(8, false), // e_ident[EI_MAG3]
        h_bits(8, false), // e_ident[EI_CLASS]
        h_bits(8, false), // e_ident[EI_DATA]
        h_bits(8, false), // e_ident[EI_VERSION]
        h_bits(8, false), // e_ident[EI_OSABI]
        h_bits(8, false), // e_ident[EI_ABIVERSION]
        h_ignore(7*8),    // e_ident[EI_PAD]
        h_bits(16, false), // e_type
        h_bits(16, false), // e_machine
        h_bits(32, false), // e_version
        h_bits(64, false), // e_entry
        h_bits(64, false), // e_phoff
        h_bits(64, false), // e_shoff
        h_bits(32, false), // e_flags
        h_bits(16, false), // e_ehsize
        h_bits(16, false), // e_phentsize
        h_bits(16, false), // e_phnum
        h_bits(16, false), // e_shentsize
        h_bits(16, false), // e_shnum
        h_bits(16, false), // e_shstrndx
        NULL
    );
}

// Define the ELF program header
static HParser *elf_pheader() {
    return h_sequence(
        h_bits(32, false), // p_type
        h_bits(32, false), // p_flags
        h_bits(64, false), // p_offset
        h_bits(64, false), // p_vaddr
        h_bits(64, false), // p_paddr
        h_bits(64, false), // p_filesz
        h_bits(64, false), // p_memsz
        h_bits(64, false), // p_align
        NULL
    );
}

// Define the ELF section header
static HParser *elf_sheader() {
    return h_sequence(
        h_bits(32, false), // sh_name
        h_bits(32, false), // sh_type
        h_bits(64, false), // sh_flags
        h_bits(64, false), // sh_addr
        h_bits(64, false), // sh_offset
        h_bits(64, false), // sh_size
        h_bits(32, false), // sh_link
        h_bits(32, false), // sh_info
        h_bits(64, false), // sh_addralign
        h_bits(64, false), // sh_entsize
        NULL
    );
}

// Define the ELF file parser
static HParser *elf_file() {
    return h_sequence(
        elf_header(),
        h_many(elf_pheader()),
        h_many(elf_sheader()),
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *elf_parser = elf_file();
    HParseResult *result = h_parse(elf_parser, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse successful!\n");
    } else {
        printf("Parse failed!\n");
    }
    h_parse_result_free(result);
    h_parser_free(elf_parser);
    return 0;
}