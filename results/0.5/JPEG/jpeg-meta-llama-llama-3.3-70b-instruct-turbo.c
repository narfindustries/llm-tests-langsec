#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Define the structure for JPEG file format
typedef struct {
    uint8_t soi; // Start of Image marker
    uint8_t app0[2]; // APP0 marker
    uint8_t app0_length[2]; // Length of APP0 segment
    uint8_t app0_data[6]; // APP0 data (e.g., JPEG identifier)
    uint8_t dht[2]; // DHT marker
    uint8_t dht_length[2]; // Length of DHT segment
    uint8_t dht_data[2]; // DHT data (e.g., Huffman table)
    uint8_t sos[2]; // SOS marker
    uint8_t sos_length[2]; // Length of SOS segment
    uint8_t sos_data[3]; // SOS data (e.g., scan header)
    uint8_t eoi; // End of Image marker
} __attribute__((packed)) JpegFile;

// Define the function to parse JPEG file format
void parse_jpeg(const uint8_t* data, size_t size) {
    JpegFile* jpeg = (JpegFile*)data;
    
    // Check if the data is a valid JPEG file
    if (size < sizeof(JpegFile) || jpeg->soi != 0xFF || jpeg->app0[0] != 0xFF || jpeg->app0[1] != 0xE0) {
        printf("Invalid JPEG file\n");
        return;
    }
    
    // Print the JPEG file format information
    printf("JPEG file format:\n");
    printf("  SOI: 0x%02X\n", jpeg->soi);
    printf("  APP0 marker: 0x%02X%02X\n", jpeg->app0[0], jpeg->app0[1]);
    printf("  APP0 length: %u\n", (jpeg->app0_length[0] << 8) | jpeg->app0_length[1]);
    printf("  APP0 data: ");
    for (int i = 0; i < 6; i++) {
        printf("0x%02X ", jpeg->app0_data[i]);
    }
    printf("\n");
    printf("  DHT marker: 0x%02X%02X\n", jpeg->dht[0], jpeg->dht[1]);
    printf("  DHT length: %u\n", (jpeg->dht_length[0] << 8) | jpeg->dht_length[1]);
    printf("  DHT data: 0x%02X%02X\n", jpeg->dht_data[0], jpeg->dht_data[1]);
    printf("  SOS marker: 0x%02X%02X\n", jpeg->sos[0], jpeg->sos[1]);
    printf("  SOS length: %u\n", (jpeg->sos_length[0] << 8) | jpeg->sos_length[1]);
    printf("  SOS data: ");
    for (int i = 0; i < 3; i++) {
        printf("0x%02X ", jpeg->sos_data[i]);
    }
    printf("\n");
    printf("  EOI: 0x%02X\n", jpeg->eoi);
}

int main() {
    // Example usage:
    uint8_t data[] = {
        0xFF, 0xD8, 0xFF, 0xE0, 0x00, 0x10, 0x4A, 0x46, 0x49, 0x46, 0x00, 0x01,
        0xFF, 0xC4, 0x00, 0x1A, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xDA, 0x00, 0x0C,
        0x00, 0x03, 0x00, 0x00, 0x00, 0x01, 0xFF, 0xD9
    };
    size_t size = sizeof(data);
    
    parse_jpeg(data, size);
    
    return 0;
}