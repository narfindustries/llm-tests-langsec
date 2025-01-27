#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <hammer/parser.h>
#include <hammer/alloc.h>
#include <hammer/stream.h>
#include <hammer/error.h>
#include <hammer/compiler.h>
#include <hammer/elf.h>
#include <hammer/elf_parser.h>
#include <hammer/elf_utils.h>

// Define the ELF header parser
HParser *elf_header_parser() {
    return h_sequence(
        h_magic("\x7F""ELF", 4), // ELF magic number
        h_bits(8, false),        // EI_CLASS (32-bit or 64-bit)
        h_bits(8, false),        // EI_DATA (endianness)
        h_bits(8, false),        // EI_VERSION (ELF version)
        h_bits(8, false),        // EI_OSABI (OS/ABI identification)
        h_bits(8, false),        // EI_ABIVERSION (ABI version)
        h_bits(48, false),       // EI_PAD (padding)
        h_bits(16, false),       // e_type (object file type)
        h_bits(16, false),       // e_machine (architecture)
        h_bits(32, false),       // e_version (ELF version)
        h_bits(32, false),       // e_entry (entry point)
        h_bits(32, false),       // e_phoff (program header table offset)
        h_bits(32, false),       // e_shoff (section header table offset)
        h_bits(32, false),       // e_flags (processor-specific flags)
        h_bits(16, false),       // e_ehsize (ELF header size)
        h_bits(16, false),       // e_phentsize (program header entry size)
        h_bits(16, false),       // e_phnum (number of program headers)
        h_bits(16, false),       // e_shentsize (section header entry size)
        h_bits(16, false),       // e_shnum (number of section headers)
        h_bits(16, false),       // e_shstrndx (section header string table index)
        NULL
    );
}

// Define the ELF section header parser
HParser *elf_section_header_parser() {
    return h_sequence(
        h_bits(32, false),       // sh_name (section name)
        h_bits(32, false),       // sh_type (section type)
        h_bits(32, false),       // sh_flags (section flags)
        h_bits(32, false),       // sh_addr (section address)
        h_bits(32, false),       // sh_offset (section offset)
        h_bits(32, false),       // sh_size (section size)
        h_bits(32, false),       // sh_link (section link)
        h_bits(32, false),       // sh_info (section info)
        h_bits(32, false),       // sh_addralign (section alignment)
        h_bits(32, false),       // sh_entsize (section entry size)
        NULL
    );
}

// Define the ELF program header parser
HParser *elf_program_header_parser() {
    return h_sequence(
        h_bits(32, false),       // p_type (segment type)
        h_bits(32, false),       // p_offset (segment offset)
        h_bits(32, false),       // p_vaddr (virtual address)
        h_bits(32, false),       // p_paddr (physical address)
        h_bits(32, false),       // p_filesz (file size)
        h_bits(32, false),       // p_memsz (memory size)
        h_bits(32, false),       // p_flags (segment flags)
        h_bits(32, false),       // p_align (segment alignment)
        NULL
    );
}

// Define the ELF parser
HParser *elf_parser() {
    return h_sequence(
        elf_header_parser(),
        h_repeat(elf_program_header_parser(), h_int_range(0, 65535)),
        h_repeat(elf_section_header_parser(), h_int_range(0, 65535)),
        NULL
    );
}

// Main function to compile and run the parser
int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    const char *filename = argv[1];
    HInputStream *input_stream = h_input_stream_from_file(filename);
    if (!input_stream) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        return 1;
    }

    HParser *parser = elf_parser();
    HParseResult *result = h_parse(parser, input_stream);
    if (!result) {
        fprintf(stderr, "Failed to parse ELF file: %s\n", filename);
        h_input_stream_free(input_stream);
        return 1;
    }

    printf("Successfully parsed ELF file: %s\n", filename);
    h_parse_result_free(result);
    h_input_stream_free(input_stream);
    return 0;
}