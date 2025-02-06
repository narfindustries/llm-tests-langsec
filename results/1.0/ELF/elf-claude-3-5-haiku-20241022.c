#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t e_ident[16];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    uint64_t e_entry;
    uint64_t e_phoff;
    uint64_t e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint16_t e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
} __attribute__((packed)) ElfHeader;

HParser* create_elf_parser() {
    uint8_t magic_bytes[] = {0x7F, 'E', 'L', 'F'};
    HParser* magic_number = h_token(magic_bytes, sizeof(magic_bytes));

    HParser* elf_class = h_choice(
        h_ch(0),
        h_ch(1),
        h_ch(2),
        NULL
    );

    HParser* elf_data_encoding = h_choice(
        h_ch(0),
        h_ch(1),
        h_ch(2),
        NULL
    );

    HParser* elf_version = h_choice(
        h_ch(0),
        h_ch(1),
        NULL
    );

    HParser* elf_osabi = h_choice(
        h_ch(0),
        h_ch(3),
        h_ch(9),
        h_uint8(),
        NULL
    );

    HParser* elf_type = h_choice(
        h_ch(0),
        h_ch(1),
        h_ch(2),
        h_ch(3),
        h_ch(4),
        h_uint16(),
        NULL
    );

    HParser* elf_machine = h_choice(
        h_ch(0),
        h_ch(3),
        h_ch(62),
        h_ch(40),
        h_uint16(),
        NULL
    );

    HParser* elf_header = h_sequence(
        magic_number,
        elf_class,
        elf_data_encoding,
        elf_version,
        elf_osabi,
        h_uint8(),
        h_repeat_n(h_ch(0), 7),
        elf_type,
        elf_machine,
        h_uint32(),
        h_uint64(),
        h_uint64(),
        h_uint64(),
        h_uint32(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        NULL
    );

    return elf_header;
}

int main(int argc, char** argv) {
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
    fseek(file, 0, SEEK_SET);

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
    fclose(file);

    HParser* elf_parser = create_elf_parser();
    HParseResult* result = h_parse(elf_parser, buffer, file_size);

    if (result && result->ast) {
        printf("ELF file successfully parsed!\n");
    } else {
        printf("ELF file parsing failed.\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(elf_parser);
    free(buffer);

    return 0;
}