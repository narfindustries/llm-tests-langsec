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


// Function to create a Hammer file
HammerFile* createHammerFile(const char *filename, const char *content, size_t size) {
    HammerFile *hammerFile = (HammerFile *)malloc(sizeof(HammerFile));
    if (hammerFile == NULL) {
        perror("malloc failed");
        exit(1);
    }
    hammerFile->filename = strdup(filename);
    if (hammerFile->filename == NULL) {
        perror("strdup failed");
        free(hammerFile);
        exit(1);
    }
    hammerFile->content = (char *)malloc(size + 1);
    if (hammerFile->content == NULL) {
        perror("malloc failed");
        free(hammerFile->filename);
        free(hammerFile);
        exit(1);
    }
    memcpy(hammerFile->content, content, size);
    hammerFile->content[size] = '\0';
    hammerFile->size = size;
    return hammerFile;
}


// Function to write a Hammer file to disk
void writeHammerFile(const HammerFile *hammerFile) {
    int fd = open(hammerFile->filename, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd == -1) {
        perror("open failed");
        exit(1);
    }
    ssize_t bytesWritten = write(fd, hammerFile->content, hammerFile->size);
    if (bytesWritten == -1) {
        perror("write failed");
        exit(1);
    }
    close(fd);
}


// Function to free a Hammer file
void freeHammerFile(HammerFile *hammerFile) {
    free(hammerFile->filename);
    free(hammerFile->content);
    free(hammerFile);
}


int main() {
    // Example usage: Create and write a Hammer file.  Replace with your actual data.
    char *content = "This is a test Hammer file.";
    size_t contentSize = strlen(content);
    HammerFile *hammerFile = createHammerFile("output.txt", content, contentSize);
    writeHammerFile(hammerFile);
    freeHammerFile(hammerFile);
    return 0;
}
