GIF := Header
        LogicalScreenDescriptor
        GlobalColorTable?
        ImageDescriptor
        LocalColorTable?
        ImageData
        GraphicControlExtension*
        PlainTextExtension*
        ApplicationExtension*
        CommentExtension*
        Trailer;

Header := Signature Version;

Signature := 'G' 'I' 'F';

Version := '87' 'a' | '89' 'a';

LogicalScreenDescriptor := LogicalScreenWidth
                            LogicalScreenHeight
                            PackedFields
                            BackgroundColorIndex
                            PixelAspectRatio;

LogicalScreenWidth := UInt16;

LogicalScreenHeight := UInt16;

PackedFields := GlobalColorTableFlag
                ColorResolution
                SortFlag
                SizeOfGlobalColorTable;

GlobalColorTableFlag := Bit;

ColorResolution := Bits 3;

SortFlag := Bit;

SizeOfGlobalColorTable := Bits 3;

BackgroundColorIndex := UInt8;

PixelAspectRatio := UInt8;

GlobalColorTable := Color[2^(SizeOfGlobalColorTable + 1)];

Color := Red Green Blue;

Red := UInt8;

Green := UInt8;

Blue := UInt8;

ImageDescriptor := ImageSeparator
                   ImageLeftPosition
                   ImageTopPosition
                   ImageWidth
                   ImageHeight
                   PackedFieldsImage;

ImageSeparator := 0x2C;

ImageLeftPosition := UInt16;

ImageTopPosition := UInt16;

ImageWidth := UInt16;

ImageHeight := UInt16;

PackedFieldsImage := LocalColorTableFlag
                      InterlaceFlag
                      SortFlagImage
                      Reserved
                      SizeOfLocalColorTable;

LocalColorTableFlag := Bit;

InterlaceFlag := Bit;

SortFlagImage := Bit;

Reserved := Bits 2;

SizeOfLocalColorTable := Bits 3;

LocalColorTable := Color[2^(SizeOfLocalColorTable + 1)];

ImageData := LZWMinimumCodeSize
              CompressedDataBlocks;

LZWMinimumCodeSize := UInt8;

CompressedDataBlocks := Block*;

Block := BlockSize Data;

BlockSize := UInt8;

Data := UInt8[BlockSize];

GraphicControlExtension := ExtensionIntroducer
                            GraphicControlLabel
                            BlockSizeGraphic
                            PackedFieldsGraphic
                            DelayTime
                            TransparentColorIndex
                            BlockTerminator;

ExtensionIntroducer := 0x21;

GraphicControlLabel := 0xF9;

BlockSizeGraphic := 0x04;

PackedFieldsGraphic := ReservedGraphic
                         DisposalMethod
                         UserInputFlag
                         TransparencyFlag;

ReservedGraphic := Bits 3;

DisposalMethod := Bits 3;

UserInputFlag := Bit;

TransparencyFlag := Bit;

DelayTime := UInt16;

TransparentColorIndex := UInt8;

BlockTerminator := 0x00;

PlainTextExtension := ExtensionIntroducer
                       PlainTextLabel
                       BlockSizePlainText
                       TextGridLeftPosition
                       TextGridTopPosition
                       TextGridWidth
                       TextGridHeight
                       CharacterCellWidth
                       CharacterCellHeight
                       TextForegroundColorIndex
                       TextBackgroundColorIndex
                       PlainTextData
                       BlockTerminator;

PlainTextLabel := 0x01;

BlockSizePlainText := 0x0C;

TextGridLeftPosition := UInt16;

TextGridTopPosition := UInt16;

TextGridWidth := UInt16;

TextGridHeight := UInt16;

CharacterCellWidth := UInt8;

CharacterCellHeight := UInt8;

TextForegroundColorIndex := UInt8;

TextBackgroundColorIndex := UInt8;

PlainTextData := Block*;

ApplicationExtension := ExtensionIntroducer
                         ApplicationExtensionLabel
                         BlockSizeApplication
                         ApplicationIdentifier
                         ApplicationAuthenticationCode
                         ApplicationData
                         BlockTerminator;

ApplicationExtensionLabel := 0xFF;

BlockSizeApplication := 0x0B;

ApplicationIdentifier := UInt8[8];

ApplicationAuthenticationCode := UInt8[3];

ApplicationData := Block*;

CommentExtension := ExtensionIntroducer
                    CommentLabel
                    CommentData
                    BlockTerminator;

CommentLabel := 0xFE;

CommentData := Block*;

Trailer := 0x3B;