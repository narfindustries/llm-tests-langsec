#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

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

HParsedToken* parse_elf_header(void* data, size_t len) {
    HParser* magic = h_token("\x7FELF", 4);
    HParser* class = h_choice(
        h_token("\x00", 1),
        h_token("\x01", 1),
        h_token("\x02", 1),
        NULL
    );
    HParser* encoding = h_choice(
        h_token("\x00", 1),
        h_token("\x01", 1),
        h_token("\x02", 1),
        NULL
    );
    HParser* version = h_token("\x01", 1);
    HParser* osabi = h_choice(
        h_token("\x00", 1),
        h_token("\x01", 1),
        h_token("\x02", 1),
        h_token("\x03", 1),
        h_token("\x06", 1),
        h_token("\x09", 1),
        h_token("\x0C", 1),
        NULL
    );
    HParser* abi_version = h_uint8();
    HParser* padding = h_repeat_n(h_uint8(), 7);

    HParser* e_ident = h_sequence(
        magic, class, encoding, version, 
        osabi, abi_version, padding, NULL
    );

    HParser* e_type = h_uint16();
    HParser* e_machine = h_uint16();
    HParser* e_version_parser = h_uint32();
    HParser* e_entry = h_uint64();
    HParser* e_phoff = h_uint64();
    HParser* e_shoff = h_uint64();
    HParser* e_flags = h_uint32();
    HParser* e_ehsize = h_uint16();
    HParser* e_phentsize = h_uint16();
    HParser* e_phnum = h_uint16();
    HParser* e_shentsize = h_uint16();
    HParser* e_shnum = h_uint16();
    HParser* e_shstrndx = h_uint16();

    HParser* elf_header = h_sequence(
        e_ident, e_type, e_machine, e_version_parser,
        e_entry, e_phoff, e_shoff, e_flags,
        e_ehsize, e_phentsize, e_phnum,
        e_shentsize, e_shnum, e_shstrndx, NULL
    );

    return h_parse(elf_header, data, len);
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

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParsedToken* result = parse_elf_header(buffer, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }

    free(buffer);
    h_parse_result_free(result);
    return 0;
}