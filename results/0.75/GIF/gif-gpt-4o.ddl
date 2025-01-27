Format gif {
    Const HEADER: Bytes(6) = "GIF89a";
    
    Struct LogicalScreenDescriptor {
        Width: UInt16;
        Height: UInt16;
        PackedFields: UInt8;
        BackgroundColorIndex: UInt8;
        PixelAspectRatio: UInt8;
    }
    
    Struct GlobalColorTable {
        Colors: Array(UInt8, context.logicalScreenDescriptor.colorTableSize);
    }
    
    Struct ImageDescriptor {
        ImageSeparator: Const(Bytes(1)) = 0x2C;
        LeftPosition: UInt16;
        TopPosition: UInt16;
        Width: UInt16;
        Height: UInt16;
        PackedFields: UInt8;
    }
    
    Struct LocalColorTable {
        Colors: Array(UInt8, context.imageDescriptor.colorTableSize);
    }
    
    Struct ImageData {
        LzwMinimumCodeSize: UInt8;
        Blocks: Array(SubBlock);
    }

    Struct SubBlock {
        Size: UInt8;
        Data: Bytes(context.subBlock.size);
    }

    Struct Trailer {
        TrailerByte: Const(Bytes(1)) = 0x3B;
    }

    LogicalScreenDescriptor: LogicalScreenDescriptor;
    GlobalColorTable: Optional(GlobalColorTable) = (logicalScreenDescriptor.PackedFields & 0x80) != 0;
    Images: Array(Image) = until(trailer.TrailerByte == 0x3B);
    Trailer: Trailer;
}