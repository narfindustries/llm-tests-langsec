// Daedalus specification for GIF file format
namespace gif

// GIF Header
struct Header {
    signature: string(3); // "GIF"
    version: string(3);   // "87a" or "89a"
}

// Logical Screen Descriptor
struct LogicalScreenDescriptor {
    width: u2;
    height: u2;
    packedFields: u1;
    backgroundColorIndex: u1;
    pixelAspectRatio: u1;
}

// Global Color Table
struct Color {
    red: u1;
    green: u1;
    blue: u1;
}

struct GlobalColorTable {
    colors: Color[];
}

// Image Descriptor
struct ImageDescriptor {
    imageSeparator: u1; // 0x2C
    imageLeftPosition: u2;
    imageTopPosition: u2;
    imageWidth: u2;
    imageHeight: u2;
    packedFields: u1;
}

// Local Color Table
struct LocalColorTable {
    colors: Color[];
}

// Image Data
struct ImageData {
    lzwMinimumCodeSize: u1;
    dataBlocks: DataBlock[];
}

struct DataBlock {
    size: u1;
    data: u1[size];
}

// Graphic Control Extension
struct GraphicControlExtension {
    extensionIntroducer: u1; // 0x21
    graphicControlLabel: u1; // 0xF9
    blockSize: u1; // 0x04
    packedFields: u1;
    delayTime: u2;
    transparentColorIndex: u1;
    blockTerminator: u1; // 0x00
}

// Comment Extension
struct CommentExtension {
    extensionIntroducer: u1; // 0x21
    commentLabel: u1; // 0xFE
    commentData: DataBlock[];
}

// Plain Text Extension
struct PlainTextExtension {
    extensionIntroducer: u1; // 0x21
    plainTextLabel: u1; // 0x01
    blockSize: u1; // 0x0C
    textGridLeftPosition: u2;
    textGridTopPosition: u2;
    textGridWidth: u2;
    textGridHeight: u2;
    characterCellWidth: u1;
    characterCellHeight: u1;
    textForegroundColorIndex: u1;
    textBackgroundColorIndex: u1;
    plainTextData: DataBlock[];
}

// Application Extension
struct ApplicationExtension {
    extensionIntroducer: u1; // 0x21
    applicationLabel: u1; // 0xFF
    blockSize: u1; // 0x0B
    applicationIdentifier: string(8);
    applicationAuthenticationCode: string(3);
    applicationData: DataBlock[];
}

// Trailer
struct Trailer {
    trailer: u1; // 0x3B
}

// GIF File
struct GifFile {
    header: Header;
    logicalScreenDescriptor: LogicalScreenDescriptor;
    globalColorTable: GlobalColorTable if logicalScreenDescriptor.packedFields & 0x80 != 0;
    blocks: Block[];
    trailer: Trailer;
}

union Block {
    imageDescriptor: ImageDescriptor;
    graphicControlExtension: GraphicControlExtension;
    commentExtension: CommentExtension;
    plainTextExtension: PlainTextExtension;
    applicationExtension: ApplicationExtension;
    imageData: ImageData;
}