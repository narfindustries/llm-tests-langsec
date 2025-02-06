#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// ELF Header Constants
#define EI_NIDENT 16
#define ELFMAG0 0x7F
#define ELFMAG1 'E'
#define ELFMAG2 'L'
#define ELFMAG3 'F'

// Parser declarations
static HParser *elf_magic;
static HParser *elf_class;
static HParser *elf_data;
static HParser *elf_version;
static HParser *elf_osabi;
static HParser *elf_abiversion;
static HParser *elf_pad;
static HParser *elf_ident;
static HParser *elf_type;
static HParser *elf_machine;
static HParser *elf_entry;
static HParser *elf_phoff;
static HParser *elf_shoff;
static HParser *elf_flags;
static HParser *elf_ehsize;
static HParser *elf_phentsize;
static HParser *elf_phnum;
static HParser *elf_shentsize;
static HParser *elf_shnum;
static HParser *elf_shstrndx;
static HParser *elf_header;
static HParser *program_header;
static HParser *section_header;
static HParser *elf_file;

void init_parsers(void) {
    // ELF Identification
    elf_magic = h_sequence(h_ch(ELFMAG0), h_ch(ELFMAG1), h_ch(ELFMAG2), h_ch(ELFMAG3), NULL);
    elf_class = h_uint8();  // ELFCLASS32/ELFCLASS64
    elf_data = h_uint8();   // ELFDATA2LSB/ELFDATA2MSB
    elf_version = h_uint8(); // EV_CURRENT
    elf_osabi = h_uint8();
    elf_abiversion = h_uint8();
    elf_pad = h_repeat_n(h_uint8(), 7); // Padding bytes
    
    elf_ident = h_sequence(elf_magic, elf_class, elf_data, elf_version,
                          elf_osabi, elf_abiversion, elf_pad, NULL);

    // ELF Header fields
    elf_type = h_uint16();
    elf_machine = h_uint16();
    elf_version = h_uint32();
    elf_entry = h_uint64();
    elf_phoff = h_uint64();
    elf_shoff = h_uint64();
    elf_flags = h_uint32();
    elf_ehsize = h_uint16();
    elf_phentsize = h_uint16();
    elf_phnum = h_uint16();
    elf_shentsize = h_uint16();
    elf_shnum = h_uint16();
    elf_shstrndx = h_uint16();

    // Complete ELF Header
    elf_header = h_sequence(elf_ident, elf_type, elf_machine, elf_version,
                           elf_entry, elf_phoff, elf_shoff, elf_flags,
                           elf_ehsize, elf_phentsize, elf_phnum,
                           elf_shentsize, elf_shnum, elf_shstrndx, NULL);

    // Program Header
    program_header = h_sequence(
        h_uint32(), // p_type
        h_uint32(), // p_flags
        h_uint64(), // p_offset
        h_uint64(), // p_vaddr
        h_uint64(), // p_paddr
        h_uint64(), // p_filesz
        h_uint64(), // p_memsz
        h_uint64(), // p_align
        NULL
    );

    // Section Header
    section_header = h_sequence(
        h_uint32(), // sh_name
        h_uint32(), // sh_type
        h_uint64(), // sh_flags
        h_uint64(), // sh_addr
        h_uint64(), // sh_offset
        h_uint64(), // sh_size
        h_uint32(), // sh_link
        h_uint32(), // sh_info
        h_uint64(), // sh_addralign
        h_uint64(), // sh_entsize
        NULL
    );

    // Complete ELF File
    elf_file = h_sequence(
        elf_header,
        h_many(program_header),
        h_many(section_header),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
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
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    init_parsers();

    HParseResult *result = h_parse(elf_file, buffer, size);
    if (!result) {
        fprintf(stderr, "Failed to parse ELF file\n");
        free(buffer);
        fclose(fp);
        return 1;
    }

    // Clean up
    h_parse_result_free(result);
    free(buffer);
    fclose(fp);

    printf("Successfully parsed ELF file\n");
    return 0;
}