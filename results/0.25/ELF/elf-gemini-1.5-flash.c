#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

typedef struct {
    unsigned char e_ident[16];
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
} elf_header_t;

typedef struct {
    uint32_t p_type;
    uint32_t p_flags;
    uint64_t p_offset;
    uint64_t p_vaddr;
    uint64_t p_paddr;
    uint64_t p_filesz;
    uint64_t p_memsz;
    uint64_t p_align;
} elf_program_header_t;

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
} elf_section_header_t;

HParser elf_uint16_parser() {
    return h_map(h_consume(2), (HMapFunc)HBytesToUint16);
}

HParser elf_uint32_parser() {
    return h_map(h_consume(4), (HMapFunc)HBytesToUint32);
}

HParser elf_uint64_parser() {
    return h_map(h_consume(8), (HMapFunc)HBytesToUint64);
}

HParser elf_header_parser() {
    return h_map(h_sequence(
                    h_consume(16),
                    elf_uint16_parser(),
                    elf_uint16_parser(),
                    elf_uint32_parser(),
                    elf_uint64_parser(),
                    elf_uint64_parser(),
                    elf_uint64_parser(),
                    elf_uint32_parser(),
                    elf_uint16_parser(),
                    elf_uint16_parser(),
                    elf_uint16_parser(),
                    elf_uint16_parser(),
                    elf_uint16_parser(),
                    elf_uint16_parser()),
                (HMapFunc)HBytesToElfHeader);
}


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    int fd = open(argv[1], O_RDONLY);
    if (fd == -1) {
        perror("open");
        return 1;
    }

    off_t fileSize = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    void* fileBuffer = malloc(fileSize);
    if (read(fd, fileBuffer, fileSize) != fileSize) {
        perror("read");
        free(fileBuffer);
        close(fd);
        return 1;
    }
    close(fd);

    HParseResult* result = h_parse(elf_header_parser(), fileBuffer, fileSize);

    if (h_result_is_success(result)) {
        elf_header_t* header = (elf_header_t*)h_result_get_value(result);
        printf("ELF header parsed successfully!\n");
        free(header);
        h_result_destroy(result);
    } else {
        fprintf(stderr, "ELF header parsing failed: %s\n", h_result_get_error(result));
        h_result_destroy(result);
    }

    free(fileBuffer);
    return 0;
}

uint16_t HBytesToUint16(const void* bytes) {
    return *(uint16_t*)bytes;
}

uint32_t HBytesToUint32(const void* bytes) {
    return *(uint32_t*)bytes;
}

uint64_t HBytesToUint64(const void* bytes) {
    return *(uint64_t*)bytes;
}

elf_header_t* HBytesToElfHeader(const void* bytes) {
    elf_header_t* header = malloc(sizeof(elf_header_t));
    memcpy(header, bytes, sizeof(elf_header_t));
    return header;
}
