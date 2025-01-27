#include <hammer/hammer.h>

int main() {
    HMR_INIT();

    HMR<?>> image = (HMR?>){
        .type = HMR_TYPE_PNG_IMAGE,
        .data = {
            .png_image = {
                .width = 70,
                .height = 70,
                .bit_depth = 8,
                .color_type = 2,
                .compression_method = 0,
                .filter_method = 0,
                .interlace_method = 0,
                .meta = {
                    .author = "Llama 3",
                    .description = "A hammer-shaped PNG image",
                    .keywords = "hammer, llama, image"
                },
                .pixels = {
                    /* generated pixel data */
                }
            }
        }
    };

    HMR_OUTPUT declaración PNG image;
    HMR_OUTPUT PNG image;
    HMR_OUTPUT declaración output;

    declaración output = (HMR?>){
        .type = HMR_TYPE_BUFFER,
        .data = {
            .buffer = {
                .data = image.data.png_image.pixels,
                .size = image.data.png_image.width * image.data.png_image.height * 4
            }
        }
    };

    HMR_WRITE(output, NULL);
    HMR Fin

    HMR秋;
}