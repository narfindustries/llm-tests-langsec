#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Define the ElfMetaLlamaLlama struct
typedef struct {
    uint32_t magic;
    uint8_t padding[12];
    uint64_t header_size;
    uint64_t program_header_offset;
    uint64_t section_header_offset;
    uint16_t flags;
    uint16_t ehsize;
    uint16_t phentsize;
    uint16_t phnum;
    uint16_t shentsize;
    uint16_t shnum;
    uint16_t shstrndx;
} ElfMetaLlamaLlama;

// Define the ProgramHeader struct
typedef struct {
    uint32_t p_type;
    uint32_t p_offset;
    uint32_t p_vaddr;
    uint32_t p_paddr;
    uint32_t p_filesz;
    uint32_t p_memsz;
    uint32_t p_flags;
    uint32_t p_align;
} ProgramHeader;

// Define the SectionHeader struct
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
} SectionHeader;

// Define the parse_elf_meta_llama_llama function
void parse_elf_meta_llama_llama(const char* data, uint64_t size) {
    // Check if the data is large enough to contain the ElfMetaLlamaLlama header
    if (size < sizeof(ElfMetaLlamaLlama)) {
        printf("Data too small to contain ElfMetaLlamaLlama header\n");
        return;
    }

    // Parse the ElfMetaLlamaLlama header
    ElfMetaLlamaLlama* elf_header = (ElfMetaLlamaLlama*) data;

    // Check the magic number
    if (elf_header->magic != 0x7F454C46) {
        printf("Invalid magic number\n");
        return;
    }

    // Check if the data is large enough to contain the program headers
    if (size < sizeof(ElfMetaLlamaLlama) + elf_header->phnum * elf_header->phentsize) {
        printf("Data too small to contain program headers\n");
        return;
    }

    // Parse the program headers
    ProgramHeader* program_headers = (ProgramHeader*) (data + sizeof(ElfMetaLlamaLlama));
    for (uint16_t i = 0; i < elf_header->phnum; i++) {
        ProgramHeader* program_header = &program_headers[i];
        printf("Program Header %d:\n", i);
        printf("  p_type: 0x%x\n", program_header->p_type);
        printf("  p_offset: 0x%x\n", program_header->p_offset);
        printf("  p_vaddr: 0x%x\n", program_header->p_vaddr);
        printf("  p_paddr: 0x%x\n", program_header->p_paddr);
        printf("  p_filesz: 0x%x\n", program_header->p_filesz);
        printf("  p_memsz: 0x%x\n", program_header->p_memsz);
        printf("  p_flags: 0x%x\n", program_header->p_flags);
        printf("  p_align: 0x%x\n", program_header->p_align);
    }

    // Check if the data is large enough to contain the section headers
    if (size < elf_header->section_header_offset + elf_header->shnum * elf_header->shentsize) {
        printf("Data too small to contain section headers\n");
        return;
    }

    // Parse the section headers
    SectionHeader* section_headers = (SectionHeader*) (data + elf_header->section_header_offset);
    for (uint16_t i = 0; i < elf_header->shnum; i++) {
        SectionHeader* section_header = &section_headers[i];
        printf("Section Header %d:\n", i);
        printf("  sh_name: 0x%x\n", section_header->sh_name);
        printf("  sh_type: 0x%x\n", section_header->sh_type);
        printf("  sh_flags: 0x%lx\n", section_header->sh_flags);
        printf("  sh_addr: 0x%lx\n", section_header->sh_addr);
        printf("  sh_offset: 0x%lx\n", section_header->sh_offset);
        printf("  sh_size: 0x%lx\n", section_header->sh_size);
        printf("  sh_link: 0x%x\n", section_header->sh_link);
        printf("  sh_info: 0x%x\n", section_header->sh_info);
        printf("  sh_addralign: 0x%lx\n", section_header->sh_addralign);
        printf("  sh_entsize: 0x%lx\n", section_header->sh_entsize);
    }
}

int main() {
    // Example usage:
    const char data[] = {
        0x7F, 0x45, 0x4C, 0x46, // Magic number
        0x02, 0x00, 0x00, 0x00, // Class
        0x01, 0x00, 0x00, 0x00, // Data
        0x01, 0x00, 0x00, 0x00, // Version
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Padding
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Header size
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Program header offset
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Section header offset
        0x00, 0x00, // Flags
        0x40, 0x00, // EH size
        0x38, 0x00, // PH entry size
        0x01, 0x00, // PH num
        0x40, 0x00, // SH entry size
        0x02, 0x00, // SH num
        0x01, 0x00, // SH string table index
        // Program header
        0x01, 0x00, 0x00, 0x00, // p_type
        0x00, 0x00, 0x00, 0x00, // p_offset
        0x00, 0x00, 0x00, 0x00, // p_vaddr
        0x00, 0x00, 0x00, 0x00, // p_paddr
        0x10, 0x00, 0x00, 0x00, // p_filesz
        0x10, 0x00, 0x00, 0x00, // p_memsz
        0x05, 0x00, 0x00, 0x00, // p_flags
        0x10, 0x00, 0x00, 0x00, // p_align
        // Section header
        0x01, 0x00, 0x00, 0x00, // sh_name
        0x01, 0x00, 0x00, 0x00, // sh_type
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // sh_flags
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // sh_addr
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // sh_offset
        0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // sh_size
        0x00, 0x00, 0x00, 0x00, // sh_link
        0x00, 0x00, 0x00, 0x00, // sh_info
        0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // sh_addralign
        0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // sh_entsize
    };
    parse_elf_meta_llama_llama(data, sizeof(data));
    return 0;
}