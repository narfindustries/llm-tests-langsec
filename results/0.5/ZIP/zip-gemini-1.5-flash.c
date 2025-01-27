#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Structure to represent a zip entry
typedef struct {
    char* filename;
    uint32_t size;
    uint8_t* data;
} ZipEntry;


// Function to simulate zip file processing (replace with actual zip library)
ZipEntry* processZipEntry(const char* filename) {
    // Replace this with actual zip file processing logic using a library like miniz
    ZipEntry* entry = (ZipEntry*)malloc(sizeof(ZipEntry));
    if (entry == NULL) {
        perror("Memory allocation failed");
        exit(1);
    }
    entry->filename = strdup(filename); //Important: strdup allocates memory, needs free() later.
    if (entry->filename == NULL) {
        perror("Memory allocation failed");
        free(entry);
        exit(1);
    }

    //Simulate data size and content
    entry->size = strlen(filename) * 2; //Example size
    entry->data = (uint8_t*)malloc(entry->size);
    if (entry->data == NULL) {
        perror("Memory allocation failed");
        free(entry->filename);
        free(entry);
        exit(1);
    }
    for (uint32_t i = 0; i < entry->size; ++i) {
        entry->data[i] = (uint8_t)(i % 256); //Example data
    }
    return entry;
}


int main() {
    //Simulate zip file entries
    const char* zipEntries[] = {"file1.txt", "file2.bin", "file3.jpg"};
    int numEntries = sizeof(zipEntries) / sizeof(zipEntries[0]);

    ZipEntry** entries = (ZipEntry**)malloc(numEntries * sizeof(ZipEntry*));
    if (entries == NULL) {
        perror("Memory allocation failed");
        exit(1);
    }

    for (int i = 0; i < numEntries; ++i) {
        entries[i] = processZipEntry(zipEntries[i]);
    }

    //Simulate Hammer processing (replace with actual Hammer library calls)
    for (int i = 0; i < numEntries; ++i) {
        printf("Processing entry: %s (size: %u)\n", entries[i]->filename, entries[i]->size);
        //Here you would integrate Hammer library calls to process entries[i]->data
        //Example:  hammer_process(entries[i]->data, entries[i]->size);

        free(entries[i]->data);
        free(entries[i]->filename);
        free(entries[i]);
    }

    free(entries);
    return 0;
}
