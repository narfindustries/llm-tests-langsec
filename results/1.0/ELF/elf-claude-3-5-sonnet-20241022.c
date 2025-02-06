#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

// ELF Header field parsers
HParser* elf_magic = h_token((uint8_t*)"\x7F" "ELF", 4);

HParser* elf_class = h_uint8();
HParser* elf_data = h_uint8();
HParser* elf_version = h_uint8();
HParser* elf_osabi = h_uint8();
HParser* elf_abiversion = h_uint8();
HParser* elf_pad = h_repeat_n(h_uint8(), 7);

HParser* e_type = h_uint16();
HParser* e_machine = h_uint16();
HParser* e_version32 = h_uint32();
HParser* e_entry32 = h_uint32();
HParser* e_entry64 = h_uint64();
HParser* e_phoff32 = h_uint32();
HParser* e_phoff64 = h_uint64();
HParser* e_shoff32 = h_uint32();
HParser* e_shoff64 = h_uint64();
HParser* e_flags = h_uint32();
HParser* e_ehsize = h_uint16();
HParser* e_phentsize = h_uint16();
HParser* e_phnum = h_uint16();
HParser* e_shentsize = h_uint16();
HParser* e_shnum = h_uint16();
HParser* e_shstrndx = h_uint16();

// Program Header parsers
HParser* p_type = h_uint32();
HParser* p_flags = h_uint32();
HParser* p_offset32 = h_uint32();
HParser* p_offset64 = h_uint64();
HParser* p_vaddr32 = h_uint32();
HParser* p_vaddr64 = h_uint64();
HParser* p_paddr32 = h_uint32();
HParser* p_paddr64 = h_uint64();
HParser* p_filesz32 = h_uint32();
HParser* p_filesz64 = h_uint64();
HParser* p_memsz32 = h_uint32();
HParser* p_memsz64 = h_uint64();
HParser* p_align32 = h_uint32();
HParser* p_align64 = h_uint64();

// Section Header parsers
HParser* sh_name = h_uint32();
HParser* sh_type = h_uint32();
HParser* sh_flags32 = h_uint32();
HParser* sh_flags64 = h_uint64();
HParser* sh_addr32 = h_uint32();
HParser* sh_addr64 = h_uint64();
HParser* sh_offset32 = h_uint32();
HParser* sh_offset64 = h_uint64();
HParser* sh_size32 = h_uint32();
HParser* sh_size64 = h_uint64();
HParser* sh_link = h_uint32();
HParser* sh_info = h_uint32();
HParser* sh_addralign32 = h_uint32();
HParser* sh_addralign64 = h_uint64();
HParser* sh_entsize32 = h_uint32();
HParser* sh_entsize64 = h_uint64();

// Program Header parser (32-bit)
HParser* program_header_32 = h_sequence(
    p_type,
    p_offset32,
    p_vaddr32,
    p_paddr32,
    p_filesz32,
    p_memsz32,
    p_flags,
    p_align32,
    NULL
);

// Program Header parser (64-bit)
HParser* program_header_64 = h_sequence(
    p_type,
    p_flags,
    p_offset64,
    p_vaddr64,
    p_paddr64,
    p_filesz64,
    p_memsz64,
    p_align64,
    NULL
);

// Section Header parser (32-bit)
HParser* section_header_32 = h_sequence(
    sh_name,
    sh_type,
    sh_flags32,
    sh_addr32,
    sh_offset32,
    sh_size32,
    sh_link,
    sh_info,
    sh_addralign32,
    sh_entsize32,
    NULL
);

// Section Header parser (64-bit)
HParser* section_header_64 = h_sequence(
    sh_name,
    sh_type,
    sh_flags64,
    sh_addr64,
    sh_offset64,
    sh_size64,
    sh_link,
    sh_info,
    sh_addralign64,
    sh_entsize64,
    NULL
);

// ELF Header parser (32-bit)
HParser* elf_header_32 = h_sequence(
    elf_magic,
    elf_class,
    elf_data,
    elf_version,
    elf_osabi,
    elf_abiversion,
    elf_pad,
    e_type,
    e_machine,
    e_version32,
    e_entry32,
    e_phoff32,
    e_shoff32,
    e_flags,
    e_ehsize,
    e_phentsize,
    e_phnum,
    e_shentsize,
    e_shnum,
    e_shstrndx,
    NULL
);

// ELF Header parser (64-bit)
HParser* elf_header_64 = h_sequence(
    elf_magic,
    elf_class,
    elf_data,
    elf_version,
    elf_osabi,
    elf_abiversion,
    elf_pad,
    e_type,
    e_machine,
    e_version32,
    e_entry64,
    e_phoff64,
    e_shoff64,
    e_flags,
    e_ehsize,
    e_phentsize,
    e_phnum,
    e_shentsize,
    e_shnum,
    e_shstrndx,
    NULL
);

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* buffer = malloc(fsize);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(buffer, 1, fsize, f) != fsize) {
        perror("Failed to read file");
        free(buffer);
        fclose(f);
        return 1;
    }

    // Check ELF class (32 or 64 bit)
    uint8_t elf_class_value = buffer[4];
    HParser* elf_parser;
    HParser* ph_parser;
    HParser* sh_parser;

    if (elf_class_value == 1) { // ELFCLASS32
        elf_parser = elf_header_32;
        ph_parser = program_header_32;
        sh_parser = section_header_32;
    } else if (elf_class_value == 2) { // ELFCLASS64
        elf_parser = elf_header_64;
        ph_parser = program_header_64;
        sh_parser = section_header_64;
    } else {
        fprintf(stderr, "Invalid ELF class\n");
        free(buffer);
        fclose(f);
        return 1;
    }

    HParseResult* result = h_parse(elf_parser, buffer, fsize);
    if (!result) {
        fprintf(stderr, "Failed to parse ELF header\n");
        free(buffer);
        fclose(f);
        return 1;
    }

    // Parse program headers and section headers based on header information
    // This would require extracting offsets and counts from the parsed header
    // and parsing the corresponding sections

    h_parse_result_free(result);
    free(buffer);
    fclose(f);
    return 0;
}