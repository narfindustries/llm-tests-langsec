#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <elf.h>


// Helper function to read a file into a buffer
uint8_t* readFile(const char* filename, size_t* size) {
    struct stat sb;
    if (stat(filename, &sb) == -1) {
        perror("stat");
        return NULL;
    }

    int fd = open(filename, O_RDONLY);
    if (fd == -1) {
        perror("open");
        return NULL;
    }

    *size = sb.st_size;
    uint8_t* buffer = (uint8_t*)malloc(*size);
    if (buffer == NULL) {
        perror("malloc");
        close(fd);
        return NULL;
    }

    ssize_t bytesRead = read(fd, buffer, *size);
    if (bytesRead == -1) {
        perror("read");
        free(buffer);
        close(fd);
        return NULL;
    }
    close(fd);
    return buffer;
}


int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    size_t fileSize;
    uint8_t* fileBuffer = readFile(argv[1], &fileSize);
    if (fileBuffer == NULL) {
        return 1;
    }

    //Basic ELF header parser (replace with full Hammer parser)

    Elf64_Ehdr* header = (Elf64_Ehdr*)fileBuffer;

    if(memcmp(header->e_ident, ELFMAG, SELFMAG) != 0){
        fprintf(stderr, "Not a valid ELF file\n");
        free(fileBuffer);
        return 1;
    }

    printf("ELF Header:\n");
    printf("  Magic: %x %x %x %x\n", header->e_ident[EI_MAG0], header->e_ident[EI_MAG1], header->e_ident[EI_MAG2], header->e_ident[EI_MAG3]);
    printf("  Data encoding: %d\n", header->e_ident[EI_DATA]);
    printf("  Version: %d\n", header->e_ident[EI_VERSION]);
    printf("  OS ABI: %d\n", header->e_ident[EI_OSABI]);
    printf("  ABI version: %d\n", header->e_ident[EI_ABIVERSION]);
    printf("  Type: %d\n", header->e_type);
    printf("  Machine: %d\n", header->e_machine);
    printf("  Version: %d\n", header->e_version);
    printf("  Entry point: 0x%lx\n", header->e_entry);
    printf("  Program header offset: %d\n", header->e_phoff);
    printf("  Section header offset: %d\n", header->e_shoff);
    printf("  Flags: %d\n", header->e_flags);
    printf("  Size of this header: %d\n", header->e_ehsize);
    printf("  Size of program header entry: %d\n", header->e_phentsize);
    printf("  Number of program header entries: %d\n", header->e_phnum);
    printf("  Size of section header entry: %d\n", header->e_shentsize);
    printf("  Number of section header entries: %d\n", header->e_shnum);
    printf("  Section header string table index: %d\n", header->e_shstrndx);


    free(fileBuffer);
    return 0;
}
