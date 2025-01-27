#include <stdio.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint8_t magic[4];
    uint16_t type;
    uint16_t machine;
    uint32_t version;
    uint64_t entry_point;
    uint64_t program_header_offset;
    uint64_t section_header_offset;
    uint32_t flags;
    uint16_t header_size;
    uint16_t program_header_size;
    uint16_t program_header_count;
    uint16_t section_header_size;
    uint16_t section_header_count;
    uint16_t section_string_table_index;
} ElfHeader;

typedef struct {
    uint32_t type;
    uint32_t offset;
    uint32_t virtual_address;
    uint32_t physical_address;
    uint32_t file_size;
    uint32_t memory_size;
    uint32_t flags;
    uint32_t alignment;
} ProgramHeader;

typedef struct {
    uint32_t name;
    uint32_t type;
    uint64_t flags;
    uint64_t address;
    uint64_t offset;
    uint64_t size;
    uint32_t link;
    uint32_t info;
    uint64_t address_alignment;
    uint64_t entry_size;
} SectionHeader;

int main() {
    // Example ELF file data
    uint8_t data[] = {
        0x7F, 'E', 'L', 'F',  // magic
        0x02, 0x01,  // type (ET_EXEC)
        0x00, 0x03,  // machine (EM_386)
        0x01, 0x00, 0x00, 0x00,  // version
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // entry point
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // program header offset
        0xE0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // section header offset
        0x00, 0x00, 0x00, 0x00,  // flags
        0x40, 0x00,  // header size
        0x38, 0x00,  // program header size
        0x02, 0x00,  // program header count
        0x40, 0x00,  // section header size
        0x05, 0x00,  // section header count
        0x01, 0x00   // section string table index
    };

    // Parse ELF header
    ElfHeader* elf_header = (ElfHeader*) data;

    // Check ELF magic
    if (memcmp(elf_header->magic, "\x7F\x45\x4C\x46", 4) != 0) {
        printf("Invalid ELF magic\n");
        return 1;
    }

    // Print ELF information
    printf("ELF type: 0x%X\n", elf_header->type);
    printf("ELF machine: 0x%X\n", elf_header->machine);
    printf("ELF version: 0x%X\n", elf_header->version);
    printf("Entry point: 0x%llX\n", elf_header->entry_point);

    // Parse program headers
    ProgramHeader* program_headers = (ProgramHeader*) (data + elf_header->program_header_offset);
    for (int i = 0; i < elf_header->program_header_count; i++) {
        printf("Program header %d:\n", i);
        printf("  Type: 0x%X\n", program_headers[i].type);
        printf("  Offset: 0x%X\n", program_headers[i].offset);
        printf("  Virtual address: 0x%llX\n", program_headers[i].virtual_address);
        printf("  Physical address: 0x%llX\n", program_headers[i].physical_address);
        printf("  File size: 0x%X\n", program_headers[i].file_size);
        printf("  Memory size: 0x%X\n", program_headers[i].memory_size);
        printf("  Flags: 0x%X\n", program_headers[i].flags);
        printf("  Alignment: 0x%X\n", program_headers[i].alignment);
    }

    // Parse section headers
    SectionHeader* section_headers = (SectionHeader*) (data + elf_header->section_header_offset);
    for (int i = 0; i < elf_header->section_header_count; i++) {
        printf("Section header %d:\n", i);
        printf("  Name: 0x%X\n", section_headers[i].name);
        printf("  Type: 0x%X\n", section_headers[i].type);
        printf("  Flags: 0x%llX\n", section_headers[i].flags);
        printf("  Address: 0x%llX\n", section_headers[i].address);
        printf("  Offset: 0x%llX\n", section_headers[i].offset);
        printf("  Size: 0x%llX\n", section_headers[i].size);
        printf("  Link: 0x%X\n", section_headers[i].link);
        printf("  Info: 0x%X\n", section_headers[i].info);
        printf("  Address alignment: 0x%llX\n", section_headers[i].address_alignment);
        printf("  Entry size: 0x%llX\n", section_headers[i].entry_size);
    }

    return 0;
}