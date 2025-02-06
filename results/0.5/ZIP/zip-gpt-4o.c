#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define the structures of the ZIP format using Hammer
HParser *local_file_header, *central_directory_file_header, *end_of_central_directory_record, *data_descriptor, *zip64_end_of_central_directory_record, *zip64_end_of_central_directory_locator;

// Define the parsers
void define_parsers() {
    local_file_header = h_sequence(
        h_bits(32, h_uint32()), // Signature
        h_uint16(), // Version Needed to Extract
        h_uint16(), // General Purpose Bit Flag
        h_uint16(), // Compression Method
        h_uint16(), // Last Mod File Time
        h_uint16(), // Last Mod File Date
        h_uint32(), // CRC-32
        h_uint32(), // Compressed Size
        h_uint32(), // Uncompressed Size
        h_uint16(), // File Name Length
        h_uint16(), // Extra Field Length
        h_length_value(h_uint16(), h_uint8()), // File Name
        h_length_value(h_uint16(), h_uint8()), // Extra Field
        NULL
    );

    central_directory_file_header = h_sequence(
        h_bits(32, h_uint32()), // Signature
        h_uint16(), // Version Made By
        h_uint16(), // Version Needed to Extract
        h_uint16(), // General Purpose Bit Flag
        h_uint16(), // Compression Method
        h_uint16(), // Last Mod File Time
        h_uint16(), // Last Mod File Date
        h_uint32(), // CRC-32
        h_uint32(), // Compressed Size
        h_uint32(), // Uncompressed Size
        h_uint16(), // File Name Length
        h_uint16(), // Extra Field Length
        h_uint16(), // File Comment Length
        h_uint16(), // Disk Number Start
        h_uint16(), // Internal File Attributes
        h_uint32(), // External File Attributes
        h_uint32(), // Relative Offset of Local Header
        h_length_value(h_uint16(), h_uint8()), // File Name
        h_length_value(h_uint16(), h_uint8()), // Extra Field
        h_length_value(h_uint16(), h_uint8()), // File Comment
        NULL
    );

    end_of_central_directory_record = h_sequence(
        h_bits(32, h_uint32()), // Signature
        h_uint16(), // Number of This Disk
        h_uint16(), // Disk Where Central Directory Starts
        h_uint16(), // Number of Central Directory Records on This Disk
        h_uint16(), // Total Number of Central Directory Records
        h_uint32(), // Size of Central Directory
        h_uint32(), // Offset of Start of Central Directory
        h_uint16(), // ZIP File Comment Length
        h_length_value(h_uint16(), h_uint8()), // ZIP File Comment
        NULL
    );

    data_descriptor = h_sequence(
        h_uint32(), // CRC-32
        h_uint32(), // Compressed Size
        h_uint32(), // Uncompressed Size
        NULL
    );

    zip64_end_of_central_directory_record = h_sequence(
        h_bits(32, h_uint32()), // Signature
        h_uint64(), // Size of ZIP64 End of Central Directory Record
        h_uint16(), // Version Made By
        h_uint16(), // Version Needed to Extract
        h_uint32(), // Number of This Disk
        h_uint32(), // Disk Where Central Directory Starts
        h_uint64(), // Number of Central Directory Records on This Disk
        h_uint64(), // Total Number of Central Directory Records
        h_uint64(), // Size of Central Directory
        h_uint64(), // Offset of Start of Central Directory
        h_length_value(h_uint64(), h_uint8()), // ZIP64 Extensible Data Sector
        NULL
    );

    zip64_end_of_central_directory_locator = h_sequence(
        h_bits(32, h_uint32()), // Signature
        h_uint32(), // Number of the Disk with the Start of the ZIP64 End of Central Directory
        h_uint64(), // Relative Offset of the ZIP64 End of Central Directory Record
        h_uint32(), // Total Number of Disks
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zipfile>\n", argv[0]);
        return EXIT_FAILURE;
    }

    // Open the input binary file
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    // Determine the file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Allocate memory for the file data
    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Error allocating memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    // Read the file into memory
    fread(data, 1, file_size, file);
    fclose(file);

    // Define the parsers
    define_parsers();

    // Parse the data
    HParseResult *result = h_parse(local_file_header, data, file_size);
    if (result) {
        printf("Parsed Local File Header successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse Local File Header.\n");
    }

    // Clean up
    free(data);
    h_parser_free(local_file_header);
    h_parser_free(central_directory_file_header);
    h_parser_free(end_of_central_directory_record);
    h_parser_free(data_descriptor);
    h_parser_free(zip64_end_of_central_directory_record);
    h_parser_free(zip64_end_of_central_directory_locator);

    return EXIT_SUCCESS;
}