#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>


// Helper function to read a specific number of bytes from a file
static int read_bytes(int fd, void *buf, size_t count) {
    ssize_t bytes_read = read(fd, buf, count);
    if (bytes_read != count) {
        perror("read");
        return -1;
    }
    return 0;
}


// Helper function to convert a 4-byte array to an unsigned integer
static uint32_t bytes_to_uint32(const unsigned char *bytes) {
    return (uint32_t)bytes[0] << 24 | (uint32_t)bytes[1] << 16 | (uint32_t)bytes[2] << 8 | (uint32_t)bytes[3];
}


// Helper function to convert a 2-byte array to an unsigned short
static uint16_t bytes_to_uint16(const unsigned char *bytes) {
    return (uint16_t)bytes[0] << 8 | (uint16_t)bytes[1];
}


int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gzip_file>\n", argv[0]);
        return 1;
    }

    int fd = open(argv[1], O_RDONLY);
    if (fd == -1) {
        perror("open");
        return 1;
    }

    unsigned char header[10];
    if (read_bytes(fd, header, 10) == -1) return 1;

    if (header[0] != 0x1f || header[1] != 0x8b) {
        fprintf(stderr, "Invalid GZIP magic number\n");
        return 1;
    }

    uint8_t cm = header[2];
    uint8_t flg = header[3];
    uint32_t mtime = bytes_to_uint32(header + 4);
    uint8_t xfl = header[8];
    uint8_t os = header[9];


    if (flg & 0x04) { // Extra fields
        unsigned char extra_header[4];
        if (read_bytes(fd, extra_header, 4) == -1) return 1;
        uint16_t subfield_id = bytes_to_uint16(extra_header);
        uint16_t subfield_len = bytes_to_uint16(extra_header + 2);
        unsigned char *extra_data = malloc(subfield_len);
        if (read_bytes(fd, extra_data, subfield_len) == -1) return 1;
        free(extra_data);
    }

    if (flg & 0x08) { // Filename
        unsigned char filename[256]; // Arbitrary buffer size
        ssize_t bytes_read = read(fd, filename, 255);
        if (bytes_read == -1) return 1;
        filename[bytes_read] = '\0';
        // Process filename
    }

    if (flg & 0x10) { // Comment
        unsigned char comment[256]; // Arbitrary buffer size
        ssize_t bytes_read = read(fd, comment, 255);
        if (bytes_read == -1) return 1;
        comment[bytes_read] = '\0';
        // Process comment
    }

    if (flg & 0x02) { // Header CRC16
        unsigned char crc16_bytes[2];
        if (read_bytes(fd, crc16_bytes, 2) == -1) return 1;
        uint16_t header_crc16 = bytes_to_uint16(crc16_bytes);
        // Verify header CRC16 (Implementation needed)
    }

    // ... (Deflate decompression using a suitable library would go here) ...

    unsigned char crc32_bytes[4];
    if (read_bytes(fd, crc32_bytes, 4) == -1) return 1;
    uint32_t crc32 = bytes_to_uint32(crc32_bytes);

    unsigned char isize_bytes[4];
    if (read_bytes(fd, isize_bytes, 4) == -1) return 1;
    uint32_t isize = bytes_to_uint32(isize_bytes);

    close(fd);
    return 0;
}
