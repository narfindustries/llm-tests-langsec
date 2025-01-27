#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#define HAMMER_BUFFER_SIZE 4096

// Structure to represent a Hammer file
typedef struct {
    char *filename;
    char *content;
    size_t size;
} HammerFile;


// Function to create a HammerFile
HammerFile* createHammerFile(const char* filename, const char* content, size_t size) {
    HammerFile* file = (HammerFile*)malloc(sizeof(HammerFile));
    if (file == NULL) {
        perror("malloc");
        exit(1);
    }
    file->filename = (char*)malloc(strlen(filename) + 1);
    if (file->filename == NULL) {
        perror("malloc");
        free(file);
        exit(1);
    }
    strcpy(file->filename, filename);
    file->content = (char*)malloc(size + 1);
    if (file->content == NULL) {
        perror("malloc");
        free(file->filename);
        free(file);
        exit(1);
    }
    strncpy(file->content, content, size);
    file->content[size] = '\0';
    file->size = size;
    return file;
}


// Function to write a HammerFile to disk
void writeHammerFile(const HammerFile* file) {
    int fd = open(file->filename, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd == -1) {
        perror("open");
        exit(1);
    }
    ssize_t bytes_written = write(fd, file->content, file->size);
    if (bytes_written == -1) {
        perror("write");
        exit(1);
    }
    if (bytes_written != file->size) {
        fprintf(stderr, "Error: Only wrote %zd of %zu bytes\n", bytes_written, file->size);
        exit(1);

    }
    close(fd);
}

// Function to free a HammerFile
void freeHammerFile(HammerFile* file) {
    free(file->filename);
    free(file->content);
    free(file);
}


int main() {
    // Example usage:
    char* content = "This is a test Hammer file.";
    size_t content_size = strlen(content);
    HammerFile* hammerFile = createHammerFile("output.txt", content, content_size);
    writeHammerFile(hammerFile);
    freeHammerFile(hammerFile);
    return 0;
}
