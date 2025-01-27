#include <hammer/hammer.h>

HParser *create_elf_parser() {
    // Define ELF header fields
    HParser *e_ident_magic = h_sequence(
        h_uint8_val(0x7f),
        h_ch('E'),
        h_ch('L'),
        h_ch('F'),
        NULL
    );

    HParser *e_ident_class = h_choice(
        h_uint8_val(1), // ELFCLASS32
        h_uint8_val(2), // ELFCLASS64
        NULL
    );

    HParser *e_ident_data = h_choice(
        h_uint8_val(1), // ELFDATA2LSB
        h_uint8_val(2), // ELFDATA2MSB
        NULL
    );

    HParser *e_ident_version = h_uint8_val(1); // EV_CURRENT

    HParser *e_ident_osabi = h_uint8(); // ELFOSABI

    HParser *e_ident_abiversion = h_uint8(); // ABI version

    HParser *e_ident_pad = h_repeat_n(h_uint8(), 7);

    HParser *e_ident = h_sequence(
        e_ident_magic,
        e_ident_class,
        e_ident_data,
        e_ident_version,
        e_ident_osabi,
        e_ident_abiversion,
        e_ident_pad,
        NULL
    );

    // Define ELF header
    HParser *e_type = h_uint16();
    HParser *e_machine = h_uint16();
    HParser *e_version = h_uint32();
    HParser *e_entry = h_uint32(); // Entry point address
    HParser *e_phoff = h_uint32(); // Program header table file offset
    HParser *e_shoff = h_uint32(); // Section header table file offset
    HParser *e_flags = h_uint32();
    HParser *e_ehsize = h_uint16(); // ELF header size
    HParser *e_phentsize = h_uint16(); // Program header table entry size
    HParser *e_phnum = h_uint16(); // Program header table entry count
    HParser *e_shentsize = h_uint16(); // Section header table entry size
    HParser *e_shnum = h_uint16(); // Section header table entry count
    HParser *e_shstrndx = h_uint16(); // Section header string table index

    HParser *elf_header = h_sequence(
        e_ident,
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

    // Complete ELF parser
    return elf_header;
}

int main(int argc, char **argv) {
    // Create the ELF parser
    HParser *elf_parser = create_elf_parser();

    // Load the input file
    const char *filename = "input.elf";
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    // Parse the ELF file
    HParseResult *result = h_parse(elf_parser, (const uint8_t *)buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse ELF file\n");
        free(buffer);
        return 1;
    }

    // Successfully parsed
    printf("Successfully parsed ELF file\n");

    // Clean up
    h_parse_result_free(result);
    free(buffer);
    return 0;
}