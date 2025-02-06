parser PNG_Image {
    unit: File;

    grammar {
        file = signature chunks end_chunk;

        signature = magic_bytes(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A);

        chunks = chunk*;

        chunk = {
            length: u32,
            type: chunk_type,
            data: chunk_data(type),
            crc: u32
        };

        chunk_type = enum {
            IHDR = 0x49484452,
            PLTE = 0x504C5445,
            tRNS = 0x74524E53,
            gAMA = 0x67414D41,
            cHRM = 0x6348524D,
            sRGB = 0x73524742,
            iCCP = 0x69434350,
            tEXt = 0x74455874,
            zTXt = 0x7A545874,
            iTXt = 0x69545874,
            bKGD = 0x624B4744,
            pHYs = 0x70485973,
            sBIT = 0x73424954,
            tIME = 0x74494D45,
            IDAT = 0x49444154,
            IEND = 0x49454E44
        };

        chunk_data = match chunk_type {
            IHDR => {
                width: u32 where width > 0 && width < 2147483648,
                height: u32 where height > 0 && height < 2147483648,
                bit_depth: u8 where [1, 2, 4, 8, 16].contains(bit_depth),
                color_type: u8 where [0, 2, 3, 4, 6].contains(color_type),
                compression: u8 = 0,
                filter: u8 = 0,
                interlace: u8 where [0, 1].contains(interlace)
            },
            PLTE => {
                palette: list<{
                    red: u8,
                    green: u8,
                    blue: u8
                }> where palette.length >= 1 && palette.length <= 256
            },
            tRNS => bytes,
            gAMA => u32,
            cHRM => {
                white_x: u32,
                white_y: u32,
                red_x: u32,
                red_y: u32,
                green_x: u32,
                green_y: u32,
                blue_x: u32,
                blue_y: u32
            },
            sRGB => u8 where value <= 3,
            iCCP => {
                profile_name: string,
                compression: u8,
                profile: bytes
            },
            tEXt => {
                keyword: string,
                text: string
            },
            zTXt => {
                keyword: string,
                compression: u8,
                compressed_text: bytes
            },
            iTXt => {
                keyword: string,
                compression_flag: u8,
                compression: u8,
                language: string,
                translated_keyword: string,
                text: string
            },
            bKGD => bytes,
            pHYs => {
                pixels_x: u32,
                pixels_y: u32,
                unit: u8 where unit <= 1
            },
            sBIT => bytes,
            tIME => {
                year: u16,
                month: u8 where month >= 1 && month <= 12,
                day: u8 where day >= 1 && day <= 31,
                hour: u8 where hour <= 23,
                minute: u8 where minute <= 59,
                second: u8 where second <= 60
            },
            IDAT => bytes,
            _ => bytes
        };

        end_chunk = chunk_type.IEND;
    }
}