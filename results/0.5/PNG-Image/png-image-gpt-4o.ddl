// PNG Image Specification in Daedalus

namespace png_image {
    // PNG signature
    struct PngSignature {
        magic: u64; // Should be 0x89504E470D0A1A0A
    }

    // PNG chunk structure
    struct PngChunk {
        length: u32;
        type: string[4];
        data: u8[length];
        crc: u32;
    }

    // PNG file structure
    struct PngFile {
        signature: PngSignature;
        chunks: [PngChunk] (until: _.type == "IEND");
    }
}
