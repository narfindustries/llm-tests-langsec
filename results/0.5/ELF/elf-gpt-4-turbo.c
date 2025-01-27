#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the basic types
static HParser *uint8 = h_uint8();
static HParser *uint16 = h_uint16();
static HParser *uint32 = h_uint32();
static HParser *uint64 = h_uint64();

// Define ELF header
static HParser *elf_header = h_sequence(
    h_bits(uint8, 4),     // EI_MAG
    h_bits(uint8, 4),     // EI_CLASS
    h_bits(uint8, 4),     // EI_DATA
    h_bits(uint8, 4),     // EI_VERSION
    h_bits(uint8, 4),     // EI_OSABI
    h_bits(uint8, 4),     // EI_ABIVERSION
    h_ignore(h_bits(uint8, 32)), // EI_PAD
    uint16,               // e_type
    uint16,               // e_machine
    uint32,               // e_version
    h_choice(uint32, uint64, NULL), // e_entry
    h_choice(uint32, uint64, NULL), // e_phoff
    h_choice(uint32, uint64, NULL), // e_shoff
    uint32,               // e_flags
    uint16,               // e_ehsize
    uint16,               // e_phentsize
    uint16,               // e_phnum
    uint16,               // e_shentsize
    uint16,               // e_shnum
    uint16,               // e_shstrndx
    NULL
);

// Define Program Header
static HParser *program_header = h_sequence(
    uint32,               // p_type
    h_optional(h_uint32()), // p_flags (optional, present in newer versions)
    h_choice(uint32, uint64, NULL), // p_offset
    h_choice(uint32, uint64, NULL), // p_vaddr
    h_choice(uint32, uint64, NULL), // p_paddr
    h_choice(uint32, uint64, NULL), // p_filesz
    h_choice(uint32, uint64, NULL), // p_memsz
    h_choice(uint32, uint64, NULL), // p_align
    NULL
);

// Define Section Header
static HParser *section_header = h_sequence(
    uint32,               // sh_name
    uint32,               // sh_type
    h_choice(uint32, uint64, NULL), // sh_flags
    h_choice(uint32, uint64, NULL), // sh_addr
    h_choice(uint32, uint64, NULL), // sh_offset
    h_choice(uint32, uint64, NULL), // sh_size
    uint32,               // sh_link
    uint32,               // sh_info
    h_choice(uint32, uint64, NULL), // sh_addralign
    h_choice(uint32, uint64, NULL), // sh_entsize
    NULL
);

// Define the complete ELF parser
static HParser *elf_parser = h_sequence(
    elf_header,
    h_many(program_header),
    h_many(section_header),
    NULL
);

int main(int argc, char **argv) {
    HParser *parser = elf_parser;
    HParseResult *result = h_parse(parser, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed!\n");
    }
    h_parse_result_free(result);
    return 0;
}