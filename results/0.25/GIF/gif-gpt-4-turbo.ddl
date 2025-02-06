grammar GIF89a;

import std.ascii;
import std.bin;

type Color = struct {
    red: u8,
    green: u8,
    blue: u8
};

type LogicalScreenDescriptor = struct {
    screenWidth: u16,
    screenHeight: u16,
    packedFields: u8,
    backgroundColorIndex: u8,
    pixelAspectRatio: u8
};

type ImageDescriptor = struct {
    imageLeftPosition: u16,
    imageTopPosition: u16,
    imageWidth: u16,
    imageHeight: u16,
    packedFields: u8
};

type GraphicControlExtension = struct {
    blockSize: u8,
    packedFields: u8,
    delayTime: u16,
    transparentColorIndex: u8,
    terminator: u8
};

type ApplicationExtension = struct {
    blockSize: u8,
    applicationIdentifier: ascii.String<8>,
    applicationAuthCode: bin.bytes<3>,
    applicationData: bin.bytes
};

type CommentExtension = struct {
    commentData: ascii.String
};

type PlainTextExtension = struct {
    blockSize: u8,
    textGridLeftPosition: u16,
    textGridTopPosition: u16,
    textGridWidth: u16,
    textGridHeight: u16,
    characterWidth: u8,
    characterHeight: u8,
    textForegroundColorIndex: u8,
    textBackgroundColorIndex: u8,
    plainTextData: ascii.String
};

type Extension = union {
    graphicControl: GraphicControlExtension,
    application: ApplicationExtension,
    comment: CommentExtension,
    plainText: PlainTextExtension
};

type Block = union {
    imageDescriptor: ImageDescriptor,
    extension: Extension
};

type GIFFile = struct {
    header: ascii.String<6>,
    logicalScreenDescriptor: LogicalScreenDescriptor,
    globalColorTable: [Color]?,
    blocks: [Block],
    trailer: u8
};

def parseGIF(input: bin.Stream) -> GIFFile {
    let header = input.read(ascii.String<6>);
    let logicalScreenDescriptor = input.read(LogicalScreenDescriptor);
    let globalColorTable = if (logicalScreenDescriptor.packedFields & 0x80) != 0 {
        input.read([Color]((logicalScreenDescriptor.packedFields & 0x07) + 1))
    } else {
        None
    };
    let blocks = [];
    loop {
        let nextByte = input.peek(u8);
        if nextByte == 0x3B { // Trailer
            break;
        } else if nextByte == 0x21 { // Extension Introducer
            input.skip(1); // Skip the introducer
            let label = input.read(u8);
            match label {
                0xF9 => blocks.push(Block::graphicControl(input.read(GraphicControlExtension))),
                0xFF => blocks.push(Block::application(input.read(ApplicationExtension))),
                0xFE => blocks.push(Block::comment(input.read(CommentExtension))),
                0x01 => blocks.push(Block::plainText(input.read(PlainTextExtension))),
                _ => {}
            }
        } else if nextByte == 0x2C { // Image Separator
            blocks.push(Block::imageDescriptor(input.read(ImageDescriptor)));
        }
    }
    let trailer = input.read(u8);
    return GIFFile { header, logicalScreenDescriptor, globalColorTable, blocks, trailer };
}