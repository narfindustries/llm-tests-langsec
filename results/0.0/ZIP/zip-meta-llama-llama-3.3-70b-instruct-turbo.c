#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the ZIP file format structure
typedef struct {
    unsigned char signature[4];  // ZIP file signature (0x04034b50)
    unsigned short version;      // Version of the ZIP file format
    unsigned short flags;        // Flags (e.g., encryption, compression)
    unsigned short compression;  // Compression method
    unsigned short last_mod_time; // Last modification time
    unsigned short last_mod_date; // Last modification date
    unsigned int crc;            // CRC-32 of the uncompressed data
    unsigned int compressed_size; // Compressed size of the data
    unsigned int uncompressed_size; // Uncompressed size of the data
    unsigned short filename_length; // Length of the filename
    unsigned short extra_field_length; // Length of the extra field
} zip_local_file_header_t;

// Define the ZIP file format structure for the central directory
typedef struct {
    unsigned char signature[4];  // ZIP file signature (0x02014b50)
    unsigned short version;      // Version of the ZIP file format
    unsigned short version_needed; // Version needed to extract
    unsigned short flags;        // Flags (e.g., encryption, compression)
    unsigned short compression;  // Compression method
    unsigned short last_mod_time; // Last modification time
    unsigned short last_mod_date; // Last modification date
    unsigned int crc;            // CRC-32 of the uncompressed data
    unsigned int compressed_size; // Compressed size of the data
    unsigned int uncompressed_size; // Uncompressed size of the data
    unsigned short filename_length; // Length of the filename
    unsigned short extra_field_length; // Length of the extra field
    unsigned short file_comment_length; // Length of the file comment
    unsigned short disk_number;   // Disk number where the file starts
    unsigned short internal_attributes; // Internal attributes
    unsigned int external_attributes; // External attributes
    unsigned int local_header_offset; // Offset of the local header
} zip_central_directory_t;

// Define the ZIP file format structure for the end of central directory
typedef struct {
    unsigned char signature[4];  // ZIP file signature (0x06054b50)
    unsigned short disk_number;   // Disk number where the central directory starts
    unsigned short central_directory_disk; // Disk number where the central directory starts
    unsigned short central_directory_entries; // Number of entries in the central directory
    unsigned short total_central_directory_entries; // Total number of entries in the central directory
    unsigned int central_directory_size; // Size of the central directory
    unsigned int central_directory_offset; // Offset of the central directory
    unsigned short comment_length; // Length of the ZIP file comment
} zip_end_of_central_directory_t;

int main() {
    // Create a sample ZIP file
    FILE *zip_file = fopen("example.zip", "wb");
    if (zip_file == NULL) {
        printf("Error creating ZIP file\n");
        return 1;
    }

    // Write the local file header
    zip_local_file_header_t local_file_header;
    local_file_header.signature[0] = 0x50;
    local_file_header.signature[1] = 0x4b;
    local_file_header.signature[2] = 0x03;
    local_file_header.signature[3] = 0x04;
    local_file_header.version = 0x14;
    local_file_header.flags = 0x00;
    local_file_header.compression = 0x00;
    local_file_header.last_mod_time = 0x00;
    local_file_header.last_mod_date = 0x00;
    local_file_header.crc = 0x00;
    local_file_header.compressed_size = 0x00;
    local_file_header.uncompressed_size = 0x00;
    local_file_header.filename_length = 0x08;
    local_file_header.extra_field_length = 0x00;
    fwrite(&local_file_header, sizeof(zip_local_file_header_t), 1, zip_file);

    // Write the filename
    char filename[] = "example.txt";
    fwrite(filename, strlen(filename), 1, zip_file);

    // Write the file data
    char file_data[] = "Hello, World!";
    fwrite(file_data, strlen(file_data), 1, zip_file);

    // Write the central directory
    zip_central_directory_t central_directory;
    central_directory.signature[0] = 0x50;
    central_directory.signature[1] = 0x4b;
    central_directory.signature[2] = 0x01;
    central_directory.signature[3] = 0x02;
    central_directory.version = 0x14;
    central_directory.version_needed = 0x14;
    central_directory.flags = 0x00;
    central_directory.compression = 0x00;
    central_directory.last_mod_time = 0x00;
    central_directory.last_mod_date = 0x00;
    central_directory.crc = 0x00;
    central_directory.compressed_size = 0x00;
    central_directory.uncompressed_size = 0x00;
    central_directory.filename_length = 0x08;
    central_directory.extra_field_length = 0x00;
    central_directory.file_comment_length = 0x00;
    central_directory.disk_number = 0x00;
    central_directory.internal_attributes = 0x00;
    central_directory.external_attributes = 0x00;
    central_directory.local_header_offset = 0x00;
    fwrite(&central_directory, sizeof(zip_central_directory_t), 1, zip_file);

    // Write the end of central directory
    zip_end_of_central_directory_t end_of_central_directory;
    end_of_central_directory.signature[0] = 0x50;
    end_of_central_directory.signature[1] = 0x4b;
    end_of_central_directory.signature[2] = 0x05;
    end_of_central_directory.signature[3] = 0x06;
    end_of_central_directory.disk_number = 0x00;
    end_of_central_directory.central_directory_disk = 0x00;
    end_of_central_directory.central_directory_entries = 0x01;
    end_of_central_directory.total_central_directory_entries = 0x01;
    end_of_central_directory.central_directory_size = 0x00;
    end_of_central_directory.central_directory_offset = 0x00;
    end_of_central_directory.comment_length = 0x00;
    fwrite(&end_of_central_directory, sizeof(zip_end_of_central_directory_t), 1, zip_file);

    fclose(zip_file);
    return 0;
}