struct GIF {
    Signature: string = "GIF";
    Version: string in {"87a", "89a"};

    struct PackedScreenDescriptor {
        GlobalColorTableFlag: bits(1);
        ColorResolution: bits(3);
        SortFlag: bits(1);
        GlobalColorTableSize: bits(3);
    }

    struct LogicalScreenDescriptor {
        Width: u16;
        Height: u16;
        PackedField: PackedScreenDescriptor;
        BackgroundColorIndex: u8;
        PixelAspectRatio: u8;
    }

    struct ColorTableEntry {
        R: u8;
        G: u8;
        B: u8;
    }

    struct GlobalColorTable {
        if (LogicalScreenDescriptor.PackedField.GlobalColorTableFlag == 1)
            Entries: ColorTableEntry[3 * 2^(LogicalScreenDescriptor.PackedField.GlobalColorTableSize + 1)];
    }

    struct PackedImageDescriptor {
        LocalColorTableFlag: bits(1);
        InterlaceFlag: bits(1);
        SortFlag: bits(1);
        Reserved: bits(2);
        LocalColorTableSize: bits(3);
    }

    struct ImageDescriptor {
        Separator: u8 = 0x2C;
        Left: u16;
        Top: u16;
        Width: u16;
        Height: u16;
        PackedField: PackedImageDescriptor;
    }

    struct LocalColorTable {
        if (ImageDescriptor.PackedField.LocalColorTableFlag == 1)
            Entries: ColorTableEntry[3 * 2^(ImageDescriptor.PackedField.LocalColorTableSize + 1)];
    }

    struct DataSubBlock {
        Size: u8;
        if (Size > 0)
            Data: u8[Size];
    }

    struct ImageData {
        LZWMinimumCodeSize: u8;
        Blocks: DataSubBlock[];
        while (peek(u8) != 0x00)
            Block: DataSubBlock;
        BlockTerminator: u8;
    }

    struct GraphicsControlExtension {
        BlockSize: u8;
        struct PackedField {
            Reserved: bits(3);
            DisposalMethod: bits(3);
            UserInputFlag: bits(1);
            TransparentColorFlag: bits(1);
        }
        Packed: PackedField;
        DelayTime: u16;
        TransparentColorIndex: u8;
        Terminator: u8 = 0x00;
    }

    struct CommentExtension {
        Blocks: DataSubBlock[];
        while (peek(u8) != 0x00)
            Block: DataSubBlock;
        Terminator: u8 = 0x00;
    }

    struct PlainTextExtension {
        BlockSize: u8;
        TextGridLeft: u16;
        TextGridTop: u16;
        TextGridWidth: u16;
        TextGridHeight: u16;
        CellWidth: u8;
        CellHeight: u8;
        ForegroundColorIndex: u8;
        BackgroundColorIndex: u8;
        Blocks: DataSubBlock[];
        while (peek(u8) != 0x00)
            Block: DataSubBlock;
        Terminator: u8 = 0x00;
    }

    struct ApplicationExtension {
        BlockSize: u8;
        ApplicationIdentifier: u8[8];
        AuthenticationCode: u8[3];
        Blocks: DataSubBlock[];
        while (peek(u8) != 0x00)
            Block: DataSubBlock;
        Terminator: u8 = 0x00;
    }

    struct Extension {
        Introducer: u8 = 0x21;
        Label: u8;
        switch (Label) {
            case 0xF9: GraphicsControl: GraphicsControlExtension;
            case 0xFE: Comment: CommentExtension;
            case 0x01: PlainText: PlainTextExtension;
            case 0xFF: Application: ApplicationExtension;
        }
    }

    struct Block {
        if (peek(u8) == 0x21)
            Extension: Extension;
        else if (peek(u8) == 0x2C) {
            ImageDesc: ImageDescriptor;
            LocalColors: LocalColorTable;
            Data: ImageData;
        }
    }

    Header: Signature;
    Ver: Version;
    LogicalScreen: LogicalScreenDescriptor;
    GlobalColors: GlobalColorTable;
    Blocks: Block[];
    while (peek(u8) != 0x3B)
        Block: Block;
    Trailer: u8 = 0x3B;
}