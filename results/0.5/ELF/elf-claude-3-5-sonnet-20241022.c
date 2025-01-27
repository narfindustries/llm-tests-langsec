#include <hammer/hammer.h>
#include <stdio.h>

HParser* init_elf_parser() {
    // Basic components
    HParser* whitespace = h_whitespace(h_ch(' '));
    HParser* newline = h_ch('\n');
    
    // Header components
    HParser* magic = h_token((uint8_t*)"\x7f""ELF", 4);
    HParser* class_field = h_uint8();
    HParser* data_field = h_uint8();
    HParser* version_field = h_uint8();
    HParser* osabi_field = h_uint8();
    HParser* abiversion_field = h_uint8();
    HParser* padding = h_repeat_n(h_uint8(), 7);
    
    // Type and machine
    HParser* type_field = h_uint16();
    HParser* machine_field = h_uint16();
    
    // Version and entry point
    HParser* e_version = h_uint32();
    HParser* entry_point = h_uint32();
    
    // Program and section header offsets
    HParser* phoff = h_uint32();
    HParser* shoff = h_uint32();
    
    // Flags and header sizes
    HParser* flags = h_uint32();
    HParser* ehsize = h_uint16();
    HParser* phentsize = h_uint16();
    HParser* phnum = h_uint16();
    HParser* shentsize = h_uint16();
    HParser* shnum = h_uint16();
    HParser* shstrndx = h_uint16();
    
    // Combine all components
    return h_sequence(
        magic,
        class_field,
        data_field,
        version_field,
        osabi_field,
        abiversion_field,
        padding,
        type_field,
        machine_field,
        e_version,
        entry_point,
        phoff,
        shoff,
        flags,
        ehsize,
        phentsize,
        phnum,
        shentsize,
        shnum,
        shstrndx,
        NULL
    );
}

HParsedToken* parse_elf(const uint8_t* input, size_t length) {
    HParser* parser = init_elf_parser();
    if (!parser) return NULL;
    
    HParseResult* result = h_parse(parser, input, length);
    if (!result) return NULL;
    
    return result->ast;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }
    
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }
    
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        fclose(file);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }
    
    if (fread(buffer, 1, file_size, file) != file_size) {
        free(buffer);
        fclose(file);
        fprintf(stderr, "File read failed\n");
        return 1;
    }
    
    HParsedToken* result = parse_elf(buffer, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        fclose(file);
        return 1;
    }
    
    // Clean up
    free(buffer);
    fclose(file);
    return 0;
}