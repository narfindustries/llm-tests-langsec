type Pixel = struct {
    r: u8,
    g: u8, 
    b: u8,
    a: u8
}

type GIFImage = struct {
    width: u16,
    height: u16, 
    pixels: [Pixel]
}

func create_gif_image(width: u16, height: u16) -> GIFImage {
    let pixel_count = width * height;
    let pixels = make([Pixel], pixel_count);
    
    for (let i = 0; i < pixel_count; i = i + 1) {
        pixels[i] = Pixel {
            r: 0,
            g: 0,
            b: 0, 
            a: 255
        };
    }

    return GIFImage {
        width: width,
        height: height,
        pixels: pixels
    }
}

func draw_pixel(image: &mut GIFImage, x: u16, y: u16, color: Pixel) {
    if x < image.width && y < image.height {
        let index = y * image.width + x;
        image.pixels[index] = color;
    }
}

func main() {
    let width = 100;
    let height = 100;
    let mut gif_image = create_gif_image(width, height);

    let red = Pixel { r: 255, g: 0, b: 0, a: 255 };
    let blue = Pixel { r: 0, g: 0, b: 255, a: 255 };

    draw_pixel(&mut gif_image, 50, 50, red);
    draw_pixel(&mut gif_image, 51, 51, blue);
}