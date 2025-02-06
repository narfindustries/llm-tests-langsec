#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define sizes and magic numbers
#define EI_NIDENT 16
#define EI_MAG0 0x7F
#define EI_MAG1 'E'
#define EI_MAG2 'L'
#define EI_MAG3 'F'

// ELF identification indexes
enum {
    EI_CLASS = 4,
    EI_DATA = 5,
    EI_VERSION = 6,
    EI_OSABI = 7,
    EI_ABIVERSION = 8
};

// e_type possible values
enum {
    ET_NONE = 0,
    ET_REL = 1,
    ET_EXEC = 2,
    ET_DYN = 3,
    ET_CORE = 4
};

// e_machine possible values
enum {
    EM_NONE = 0,
    EM_X86_64 = 62
};

// Function declarations
static void parse_elf(const char *filename);

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ELF file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_elf(argv[1]);
    return 0;
}

static void parse_elf(const char *filename) {
    // Open the ELF file
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    // Move file pointer to the end and get size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    // Read the entire file into memory
    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        fclose(file);
        fprintf(stderr, "Failed to allocate memory\n");
        exit(EXIT_FAILURE);
    }
    fread(buffer, 1, file_size, file);
    fclose(file);

    // Define ELF header parsers
    HParser *magic = h_sequence(h_int8(), h_ch(EI_MAG1), h_ch(EI_MAG2), h_ch(EI_MAG3), NULL);
    HParser *ident = h_sequence(magic,
                                h_int8(),  // EI_CLASS
                                h_int8(),  // EI_DATA
                                h_int8(),  // EI_VERSION
                                h_int8(),  // EI_OSABI
                                h_int8(),  // EI_ABIVERSION
                                h_ignore(h_repeat_n(h_int8(), 7)), // EI_PAD
                                NULL);
    HParser *e_type = h_int16();
    HParser *e_machine = h_int16();
    HParser *e_version = h_int32();
    HParser *e_entry = h_int64();
    HParser *e_phoff = h_int64();
    HParser *e_shoff = h_int64();
    HParser *e_flags = h_int32();
    HParser *e_ehsize = h_int16();
    HParser *e_phentsize = h_int16();
    HParser *e_phnum = h_int16();
    HParser *e_shentsize = h_int16();
    HParser *e_shnum = h_int16();
    HParser *e_shstrndx = h_int16();

    HParser *elf_header = h_sequence(ident, e_type, e_machine, e_version,
                                     e_entry, e_phoff, e_shoff, e_flags,
                                     e_ehsize, e_phentsize, e_phnum, 
                                     e_shentsize, e_shnum, e_shstrndx, NULL);

    // Parse the ELF header
    HParseResult *result = h_parse(elf_header, buffer, file_size);
    if (result) {
        printf("ELF parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 3);
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ELF.\n");
    }

    // Cleanup
    free(buffer);
}