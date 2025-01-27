#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// PNG Image Generation Parser and Generator

// Define color structure
typedef struct {
    uint8_t red;
    uint8_t green; 
    uint8_t blue;
    uint8_t alpha;
} RGBAColor;

// Define PNG Image structure
typedef struct {
    int width;
    int height;
    RGBAColor* pixels;
} PNGImage;

// Parser for width
static HParser* width_parser() {
    return h_int_range(1, 4096);
}

// Parser for height 
static HParser* height_parser() {
    return h_int_range(1, 4096);
}

// Parser for color component
static HParser* color_component_parser() {
    return h_int_range(0, 255);
}

// Color parser
static HParser* color_parser() {
    return h_sequence(
        color_component_parser(), // Red
        color_component_parser(), // Green
        color_component_parser(), // Blue
        color_component_parser(), // Alpha
        NULL
    );
}

// Full PNG image parser
static HParser* png_image_parser() {
    return h_sequence(
        width_parser(),   // Width
        height_parser(),  // Height
        h_many(color_parser()), // Pixel array
        NULL
    );
}

// Generator for PNG image
static HResult* png_image_generator(HAllocator* allocator, void* context) {
    PNGImage* image = (PNGImage*)context;
    
    // Allocate buffer for generation
    HBuf* buf = h_buf_new(allocator);
    
    // Write width and height
    h_buf_append_int(buf, image->width);
    h_buf_append_int(buf, image->height);
    
    // Write pixel data
    for (int i = 0; i < image->width * image->height; i++) {
        h_buf_append_byte(buf, image->pixels[i].red);
        h_buf_append_byte(buf, image->pixels[i].green);
        h_buf_append_byte(buf, image->pixels[i].blue);
        h_buf_append_byte(buf, image->pixels[i].alpha);
    }
    
    return h_make_result(allocator, buf);
}

// Main generation function
PNGImage* generate_png_image(int width, int height) {
    PNGImage* image = malloc(sizeof(PNGImage));
    image->width = width;
    image->height = height;
    image->pixels = malloc(width * height * sizeof(RGBAColor));
    
    // Generate random pixel data
    for (int i = 0; i < width * height; i++) {
        image->pixels[i].red = rand() % 256;
        image->pixels[i].green = rand() % 256;
        image->pixels[i].blue = rand() % 256;
        image->pixels[i].alpha = rand() % 256;
    }
    
    return image;
}

int main() {
    // Initialize random seed
    srand(time(NULL));
    
    // Create a sample PNG image
    PNGImage* image = generate_png_image(100, 100);
    
    // Create Hammer parser
    HParser* parser = png_image_parser();
    
    // Create Hammer generator
    HGenerator* generator = h_generator_new(png_image_generator);
    
    // Free resources
    free(image->pixels);
    free(image);
    
    return 0;
}