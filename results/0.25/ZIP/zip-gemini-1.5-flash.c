#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <arpa/inet.h>

// Helper function to read a specific number of bytes
static HParser* read_bytes(size_t count) {
    return h_map(h_read_bytes(count), h_identity);
}

// Helper function to read a uint16_t
static HParser* read_uint16(void) {
    return h_map(read_bytes(sizeof(uint16_t)), [](HParser* p, void* data){
        uint16_t* value = (uint16_t*)malloc(sizeof(uint16_t));
        if (value == NULL) return h_fail(p, "Memory allocation failed");
        *value = ntohs(*(uint16_t*)p->buffer);
        p->buffer += sizeof(uint16_t);
        return h_success(p, value);
    });
}

// Helper function to read a uint32_t
static HParser* read_uint32(void) {
    return h_map(read_bytes(sizeof(uint32_t)), [](HParser* p, void* data){
        uint32_t* value = (uint32_t*)malloc(sizeof(uint32_t));
        if (value == NULL) return h_fail(p, "Memory allocation failed");
        *value = ntohl(*(uint32_t*)p->buffer);
        p->buffer += sizeof(uint32_t);
        return h_success(p, value);
    });
}

// Helper function to read a string of specified length
static HParser* read_string(size_t length) {
    return h_map(read_bytes(length), [](HParser* p, void* data){
        char* str = (char*)malloc(length + 1);
        if (str == NULL) return h_fail(p, "Memory allocation failed");
        memcpy(str, p->buffer, length);
        str[length] = '\0';
        p->buffer += length;
        return h_success(p, str);
    });
}

//Helper to get length from uint16
static HParser* get_length_uint16(HParser* parser, size_t* length){
    uint16_t len;
    HResult res = h_parse(parser, read_uint16());
    if(res.status != HSuccessStatus) return h_fail(parser, res.message);
    len = *(uint16_t*)res.value;
    *length = len;
    free(res.value);
    return h_success(parser, NULL);
}

// Parser for Local File Header
static HParser* parse_local_file_header(void) {
    return h_sequenceN(
        h_map(h_const(0x04034b50), h_identity), //Signature
        read_uint16(), //Version Needed to Extract
        read_uint16(), //General Purpose Bit Flag
        read_uint16(), //Compression Method
        read_uint16(), //Last Modified Time
        read_uint16(), //Last Modified Date
        read_uint32(), //CRC-32
        read_uint32(), //Compressed Size
        read_uint32(), //Uncompressed Size
        get_length_uint16, //Filename Length
        get_length_uint16, //Extra Field Length
        read_string(0), //Filename - length is determined by previous field
        read_bytes(0) //Extra Field (handle variable length) - length is determined by previous field
    );
}


// Parser for Central Directory Header (simplified - many fields omitted for brevity)
static HParser* parse_central_directory_header(void) {
    return h_sequenceN(
        h_map(h_const(0x02014b50), h_identity), //Signature
        read_uint16(), //Version Made By
        read_uint16(), //Version Needed to Extract
        read_uint16(), //General Purpose Bit Flag
        read_uint16(), //Compression Method
        read_uint16(), //Last Modified Time
        read_uint16(), //Last Modified Date
        read_uint32(), //CRC-32
        read_uint32(), //Compressed Size
        read_uint32(), //Uncompressed Size
        get_length_uint16, //Filename Length
        get_length_uint16, //Extra Field Length
        get_length_uint16, //File Comment Length
        read_uint16(), //Disk Number Start
        read_uint16(), //Internal File Attributes
        read_uint32(), //External File Attributes
        read_uint32(), //Relative Offset of Local Header
        read_string(0), //Filename - length is determined by previous field
        read_bytes(0), //Extra Field - length is determined by previous field
        read_string(0)  //File Comment - length is determined by previous field
    );
}

// Parser for End of Central Directory Record (simplified)
static HParser* parse_end_of_central_directory(void) {
    return h_sequenceN(
        h_map(h_const(0x06054b50), h_identity), //Signature
        read_uint16(), //Number of this disk
        read_uint16(), //Number of the disk with the start of the central directory
        read_uint16(), //Total number of entries in the central directory on this disk
        read_uint16(), //Total number of entries in the central directory
        read_uint32(), //Size of the central directory
        read_uint32(), //Offset of start of central directory with respect to the starting disk number
        get_length_uint16, //ZIP file comment length
        read_string(0)  //ZIP file comment - length is determined by previous field
    );
}


int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = (char*)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, fileSize, file);
    fclose(file);

    HParser* parser = h_buffer(buffer, fileSize);

    HResult result = h_parse(parser, parse_end_of_central_directory());
    if (result.status != HSuccessStatus) {
        fprintf(stderr, "Error parsing End of Central Directory: %s\n", result.message);
    }

    free(buffer);
    h_free(parser);
    return 0;
}