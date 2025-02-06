#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>

typedef struct {
    unsigned char e_ident[16];
    unsigned short e_type;
    unsigned short e_machine;
    unsigned int e_version;
    unsigned long e_entry;
    unsigned long e_phoff;
    unsigned long e_shoff;
    unsigned int e_flags;
    unsigned short e_ehsize;
    unsigned short e_phentsize;
    unsigned short e_phnum;
    unsigned short e_shentsize;
    unsigned short e_shnum;
    unsigned short e_shstrndx;
} elf_header_t;

hammer_parser_t* parse_uint8(void) {
    return hammer_uint8();
}

hammer_parser_t* parse_uint16_be(void) {
    return hammer_uint16_be();
}

hammer_parser_t* parse_uint32_be(void) {
    return hammer_uint32_be();
}

hammer_parser_t* parse_uint64_be(void) {
    return hammer_uint64_be();
}

hammer_parser_t* parse_elf_ident(void) {
    return hammer_array(parse_uint8(), 16);
}

hammer_parser_t* parse_elf_header(void) {
    return hammer_sequence(
        parse_elf_ident(),
        parse_uint16_be(),
        parse_uint16_be(),
        parse_uint32_be(),
        parse_uint64_be(),
        parse_uint64_be(),
        parse_uint64_be(),
        parse_uint32_be(),
        parse_uint16_be(),
        parse_uint16_be(),
        parse_uint16_be(),
        parse_uint16_be(),
        parse_uint16_be(),
        parse_uint16_be(),
        hammer_end()
    );
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

    struct stat sb;
    if (fstat(fd, &sb) == -1) {
        perror("fstat");
        close(fd);
        return 1;
    }

    void* addr = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (addr == MAP_FAILED) {
        perror("mmap");
        close(fd);
        return 1;
    }

    close(fd);

    hammer_parser_t* parser = parse_elf_header();
    hammer_result_t result = hammer_parse(parser, addr, sb.st_size);

    if (result.success) {
        elf_header_t* header = (elf_header_t*)result.value;
        printf("ELF Header parsed successfully!\n");
        printf("e_type: 0x%x\n", header->e_type);
        free(result.value);
    } else {
        fprintf(stderr, "ELF header parsing failed: %s\n", result.error);
    }

    munmap(addr, sb.st_size);

    return 0;
}
