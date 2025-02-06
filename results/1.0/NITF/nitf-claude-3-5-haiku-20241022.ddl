module NITF {
    endian big;

    struct NITFFile {
        file_header: FileHeader;
        image_segments: ImageSegment[];
        graphic_segments: GraphicSegment[];
        text_segments: TextSegment[];
        data_extension_segments: DataExtensionSegment[];
    }

    struct FileHeader {
        fver: char[5];
        clevel: char[1];
        stype: char[10];
        orig_security: SecurityBlock;
        deriv_security: SecurityBlock;
        clsy: enum { TOP_SECRET, SECRET, CONFIDENTIAL, RESTRICTED, UNCLASSIFIED };
        code: char[40];
        fscop: char[5];
        fscpys: char[5];
        encryption: char[1];
        background_color: BackgroundColor;
        originator: char[24];
        file_datetime: char[14];
        file_title: char[80];
        reserved: char[3];
    }

    struct SecurityBlock {
        country_code: char[2];
        authority: char[11];
        reason: char[2];
        source: char[6];
        date: char[8];
    }

    struct BackgroundColor {
        r: u8;
        g: u8;
        b: u8;
    }

    struct ImageSegment {
        image_subheader: ImageSubheader;
        image_data: bytes;
    }

    struct ImageSubheader {
        icat: char[3];
        isorce: char[42];
        nrows: u32;
        ncols: u32;
        pvtype: char[3];
        irep: char[8];
        igeolo: char[60];
        nicom: u8;
        comments: char[80][] if nicom > 0;
        ic: char[2];
        comrat: char[4];
        security: SecurityBlock;
    }

    struct GraphicSegment {
        graphic_subheader: GraphicSubheader;
        graphic_data: bytes;
    }

    struct GraphicSubheader {
        sy: char[10];
        security: SecurityBlock;
    }

    struct TextSegment {
        text_subheader: TextSubheader;
        text_data: bytes;
    }

    struct TextSubheader {
        txt: char[3];
        security: SecurityBlock;
    }

    struct DataExtensionSegment {
        data_extension_subheader: DataExtensionSubheader;
        extension_data: bytes;
    }

    struct DataExtensionSubheader {
        desid: char[25];
        security: SecurityBlock;
    }
}