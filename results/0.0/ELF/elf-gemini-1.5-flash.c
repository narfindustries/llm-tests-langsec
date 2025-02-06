#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <elf.h>

static hammer_parser_t* elf_ident_parser() {
    return hammer_seq(
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        hammer_uint8(),
        NULL
    );
}

static hammer_parser_t* elf_header_parser() {
    return hammer_seq(
        elf_ident_parser(),
        hammer_uint16(), 
        hammer_uint16(), 
        hammer_uint32(), 
        hammer_uint64(), 
        hammer_uint64(), 
        hammer_uint64(), 
        hammer_uint32(), 
        hammer_uint16(), 
        hammer_uint16(), 
        hammer_uint16(), 
        hammer_uint16(), 
        hammer_uint16(), 
        hammer_uint16(),  
        NULL
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

    char* file_buffer = (char*)malloc(sb.st_size);
    if (file_buffer == NULL) {
        perror("malloc");
        close(fd);
        return 1;
    }

    ssize_t bytes_read = read(fd, file_buffer, sb.st_size);
    if (bytes_read == -1 || bytes_read != sb.st_size) {
        perror("read");
        free(file_buffer);
        close(fd);
        return 1;
    }
    close(fd);


    hammer_parser_t* parser = elf_header_parser();
    hammer_result_t result = hammer_parse(parser, file_buffer, sb.st_size);

    if (result.success) {
        printf("ELF header parsed successfully!\n");
    } else {
        fprintf(stderr, "ELF header parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    hammer_free(parser);
    free(file_buffer);
    return 0;
}
