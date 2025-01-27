#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define constants
#define MAX_DELAY 1000 // Maximum delay in milliseconds

// Define data structures
typedef struct {
    uint8_t opcode;
    uint16_t len;
    uint8_t data[255];
} frame_t;

typedef struct {
    uint8_t type;
    uint16_t seq;
    uint8_t flags;
} meta_t;

// Define functions
void process_frame(frame_t* frame) {
    if (frame->opcode == 0x01) {
        meta_t* meta = (meta_t*)frame->data;
        if (meta->type == 0x02) {
            printf("Received METADATA frame\n");
        }
    }
}

// Main program
int main() {
    // Initialize variables
    frame_t frame;

    // Receive and process frame
    while (1) {
        // Receive data (simulated)
        uint8_t data[] = {0x01, 0x02, 0x00, 0x03};
        frame.opcode = data[0];
        frame.len = (data[1] << 8) | data[2];
        memcpy(frame.data, data + 3, frame.len);

        // Process frame
        process_frame(&frame);

        // Introduce a small delay to avoid overwhelming the system
        usleep(1000);
    }

    return 0;
}