#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>
#include <hammer/allocator.h>

typedef struct {
    uint8_t e_ident[16];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    void* e_entry;
    void* e_phoff;
    void* e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint16_t e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
} ElfHeader;

HParser* elf_magic_number() {
    uint8_t magic[] = {0x7F, 'E', 'L', 'F'};
    return h_token(magic, sizeof(magic));
}

HParser* elf_class() {
    uint8_t classes[] = {0x00, 0x01, 0x02};
    return h_choice(
        h_token(classes, sizeof(classes)),
        NULL
    );
}

HParser* elf_data_encoding() {
    uint8_t encodings[] = {0x00, 0x01, 0x02};
    return h_choice(
        h_token(encodings, sizeof(encodings)),
        NULL
    );
}

HParser* elf_version() {
    uint8_t version = 0x01;
    return h_token(&version, 1);
}

HParser* elf_osabi() {
    uint8_t osabis[] = {0x00, 0x01, 0x02, 0x03, 0x06, 0x09, 0x0C};
    return h_choice(
        h_token(osabis, sizeof(osabis)),
        NULL
    );
}

static HParsedToken* identity_action(const HParseResult* p, void* user_data) {
    return p->ast;
}

HParser* elf_header_parser() {
    return h_sequence(
        elf_magic_number(),
        elf_class(),
        elf_data_encoding(),
        elf_version(),
        elf_osabi(),
        h_repeat_n(h_token((uint8_t[]){0x00}, 1), 8),
        h_uint16(),
        h_uint16(),
        h_uint32(),
        h_action(h_int64(), identity_action, NULL),
        h_action(h_int64(), identity_action, NULL),
        h_action(h_int64(), identity_action, NULL),
        h_uint32(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* parser = elf_header_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("ELF file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("ELF file parsing failed\n");
    }

    free(buffer);
    fclose(file);
    h_parser_free(parser);
    return 0;
}