#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <hammer/parser.h>
#include <hammer/alloc.h>
#include <hammer/stream.h>
#include <hammer/error.h>
#include <hammer/compiler.h>
#include <hammer/elf.h>
#include <hammer/elf_parser.h>

// Define the ELF header structure
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
} ElfHeader;

// Define the ELF program header structure
typedef struct {
    uint32_t p_type;
    uint32_t p_flags;
    uint64_t p_offset;
    uint64_t p_vaddr;
    uint64_t p_paddr;
    uint64_t p_filesz;
    uint64_t p_memsz;
    uint64_t p_align;
} ElfProgramHeader;

// Define the ELF section header structure
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
} ElfSectionHeader;

// Define the ELF symbol structure
typedef struct {
    uint32_t st_name;
    uint8_t st_info;
    uint8_t st_other;
    uint16_t st_shndx;
    uint64_t st_value;
    uint64_t st_size;
} ElfSymbol;

// Define the ELF relocation structure
typedef struct {
    uint64_t r_offset;
    uint64_t r_info;
} ElfRelocation;

// Define the ELF parser
HParser *elf_parser() {
    H_RULE(elf_header, h_sequence(
        h_bytes(16), // e_ident
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
        h_uint16(),  // e_shstrndx
        NULL
    ));

    H_RULE(elf_program_header, h_sequence(
        h_uint32(),  // p_type
        h_uint32(),  // p_flags
        h_uint64(),  // p_offset
        h_uint64(),  // p_vaddr
        h_uint64(),  // p_paddr
        h_uint64(),  // p_filesz
        h_uint64(),  // p_memsz
        h_uint64(),  // p_align
        NULL
    ));

    H_RULE(elf_section_header, h_sequence(
        h_uint32(),  // sh_name
        h_uint32(),  // sh_type
        h_uint64(),  // sh_flags
        h_uint64(),  // sh_addr
        h_uint64(),  // sh_offset
        h_uint64(),  // sh_size
        h_uint32(),  // sh_link
        h_uint32(),  // sh_info
        h_uint64(),  // sh_addralign
        h_uint64(),  // sh_entsize
        NULL
    ));

    H_RULE(elf_symbol, h_sequence(
        h_uint32(),  // st_name
        h_uint8(),   // st_info
        h_uint8(),   // st_other
        h_uint16(),  // st_shndx
        h_uint64(),  // st_value
        h_uint64(),  // st_size
        NULL
    ));

    H_RULE(elf_relocation, h_sequence(
        h_uint64(),  // r_offset
        h_uint64(),  // r_info
        NULL
    ));

    H_RULE(elf_file, h_sequence(
        elf_header,
        h_repeat(elf_program_header, h_int_range(0, 65535)),
        h_repeat(elf_section_header, h_int_range(0, 65535)),
        h_repeat(elf_symbol, h_int_range(0, 65535)),
        h_repeat(elf_relocation, h_int_range(0, 65535)),
        NULL
    ));

    return elf_file;
}

// Main function to compile and run the parser
int main(int argc, char **argv) {
    HParser *parser = elf_parser();
    HParseResult *result = h_parse_file(parser, argv[1]);
    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }
    h_parser_free(parser);
    return 0;
}