#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// ELF Header Constants
#define EI_NIDENT 16
#define ELFMAG0 0x7F
#define ELFMAG1 'E'
#define ELFMAG2 'L'
#define ELFMAG3 'F'

// Parser declarations
static HParser *elf_ident_parser;
static HParser *elf_header_parser;
static HParser *program_header_parser;
static HParser *section_header_parser;
static HParser *symbol_table_entry_parser;
static HParser *elf_file_parser;

void init_parsers(void) {
    // e_ident parser
    HParser *magic = h_sequence(
        h_ch(ELFMAG0),
        h_ch(ELFMAG1),
        h_ch(ELFMAG2),
        h_ch(ELFMAG3),
        NULL
    );
    
    HParser *ei_class = h_uint8();
    HParser *ei_data = h_uint8();
    HParser *ei_version = h_uint8();
    HParser *ei_osabi = h_uint8();
    HParser *ei_abiversion = h_uint8();
    HParser *ei_pad = h_repeat_n(h_uint8(), 7);  // 7 padding bytes

    elf_ident_parser = h_sequence(
        magic,
        ei_class,
        ei_data,
        ei_version,
        ei_osabi,
        ei_abiversion,
        ei_pad,
        NULL
    );

    // ELF Header parser
    HParser *e_type = h_uint16();
    HParser *e_machine = h_uint16();
    HParser *e_version = h_uint32();
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

    elf_header_parser = h_sequence(
        elf_ident_parser,
        e_type,
        e_machine,
        e_version,
        e_entry,
        e_phoff,
        e_shoff,
        e_flags,
        e_ehsize,
        e_phentsize,
        e_phnum,
        e_shentsize,
        e_shnum,
        e_shstrndx,
        NULL
    );

    // Program Header parser
    HParser *p_type = h_uint32();
    HParser *p_flags = h_uint32();
    HParser *p_offset = h_uint64();
    HParser *p_vaddr = h_uint64();
    HParser *p_paddr = h_uint64();
    HParser *p_filesz = h_uint64();
    HParser *p_memsz = h_uint64();
    HParser *p_align = h_uint64();

    program_header_parser = h_sequence(
        p_type,
        p_flags,
        p_offset,
        p_vaddr,
        p_paddr,
        p_filesz,
        p_memsz,
        p_align,
        NULL
    );

    // Section Header parser
    HParser *sh_name = h_uint32();
    HParser *sh_type = h_uint32();
    HParser *sh_flags = h_uint64();
    HParser *sh_addr = h_uint64();
    HParser *sh_offset = h_uint64();
    HParser *sh_size = h_uint64();
    HParser *sh_link = h_uint32();
    HParser *sh_info = h_uint32();
    HParser *sh_addralign = h_uint64();
    HParser *sh_entsize = h_uint64();

    section_header_parser = h_sequence(
        sh_name,
        sh_type,
        sh_flags,
        sh_addr,
        sh_offset,
        sh_size,
        sh_link,
        sh_info,
        sh_addralign,
        sh_entsize,
        NULL
    );

    // Symbol Table Entry parser
    HParser *st_name = h_uint32();
    HParser *st_info = h_uint8();
    HParser *st_other = h_uint8();
    HParser *st_shndx = h_uint16();
    HParser *st_value = h_uint64();
    HParser *st_size = h_uint64();

    symbol_table_entry_parser = h_sequence(
        st_name,
        st_info,
        st_other,
        st_shndx,
        st_value,
        st_size,
        NULL
    );

    // Complete ELF file parser
    elf_file_parser = h_sequence(
        elf_header_parser,
        h_many(program_header_parser),
        h_many(section_header_parser),
        h_many(symbol_table_entry_parser),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(buffer, 1, size, f) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(f);
        return 1;
    }

    init_parsers();

    HParseResult *result = h_parse(elf_file_parser, buffer, size);
    if (!result) {
        fprintf(stderr, "Failed to parse ELF file\n");
        free(buffer);
        fclose(f);
        return 1;
    }

    // Here you would process the parse tree in result->ast
    // For now, we just indicate success
    printf("Successfully parsed ELF file\n");

    h_parse_result_free(result);
    free(buffer);
    fclose(f);
    return 0;
}