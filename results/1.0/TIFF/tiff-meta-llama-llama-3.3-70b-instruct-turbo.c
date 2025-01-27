#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the structure for TIFF image file directory
typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t offset;
} TiffIFDEntry;

// Define the structure for TIFF image file header
typedef struct {
    uint16_t byteOrder;
    uint16_t magic;
    uint32_t offset;
} TiffIFH;

// Function to parse TIFF image file
void parseTiff(const char* data, size_t size) {
    // Check if the data is valid
    if (size < sizeof(TiffIFH)) {
        printf("Invalid TIFF file\n");
        return;
    }

    // Parse the TIFF image file header
    TiffIFH ifh;
    memcpy(&ifh, data, sizeof(TiffIFH));

    // Check the byte order
    if (ifh.byteOrder != 0x4949 && ifh.byteOrder != 0x4d4d) {
        printf("Unsupported byte order\n");
        return;
    }

    // Check the magic number
    if (ifh.magic != 0x002a) {
        printf("Invalid TIFF file\n");
        return;
    }

    // Parse the TIFF image file directory
    TiffIFDEntry ifd;
    uint32_t ifdOffset = ifh.offset;
    while (ifdOffset < size) {
        // Read the IFD entry
        memcpy(&ifd, data + ifdOffset, sizeof(TiffIFDEntry));

        // Handle the IFD entry
        switch (ifd.tag) {
            case 0x0100: // ImageWidth
                printf("Image width: %u\n", *(uint16_t*)(data + ifdOffset + sizeof(TiffIFDEntry)));
                break;
            case 0x0101: // ImageLength
                printf("Image length: %u\n", *(uint16_t*)(data + ifdOffset + sizeof(TiffIFDEntry)));
                break;
            case 0x0102: // BitsPerSample
                printf("Bits per sample: %u\n", *(uint16_t*)(data + ifdOffset + sizeof(TiffIFDEntry)));
                break;
            default:
                printf("Unknown tag: %u\n", ifd.tag);
                break;
        }

        // Move to the next IFD entry
        ifdOffset += sizeof(TiffIFDEntry);
    }
}

int main() {
    const char data[] = {
        0x49, 0x49, 0x2a, 0x00, 0x08, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x03, 0x00, 0x01, 0x00, 0x01, 0x00,
        0x00, 0x02, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00,
        0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    parseTiff(data, sizeof(data));

    return 0;
}