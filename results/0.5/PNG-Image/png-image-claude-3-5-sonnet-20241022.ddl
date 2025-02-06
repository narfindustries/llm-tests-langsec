def PNG_SIGNATURE = x"89504E470D0A1A0A"

format png {
    bitfield ChunkType {
        bit[8] byte1;
        bit[8] byte2;
        bit[8] byte3;
        bit[8] byte4;
    }

    section Chunk {
        u32 length;
        ChunkType type;
        byte[length] data;
        u32 crc;
    }

    section IHDR {
        u32 width;
        u32 height;
        u8 bit_depth;
        u8 color_type;
        u8 compression_method;
        u8 filter_method;
        u8 interlace_method;
    }

    section RGB {
        u8 r;
        u8 g;
        u8 b;
    }

    section PLTE {
        RGB[..] palette;
    }

    section tRNS {
        if (parent.ihdr.color_type == 0) {
            u16 gray_alpha;
        } else if (parent.ihdr.color_type == 2) {
            u16 red_alpha;
            u16 green_alpha;
            u16 blue_alpha;
        } else if (parent.ihdr.color_type == 3) {
            u8[..] palette_alpha;
        }
    }

    section cHRM {
        u32 white_point_x;
        u32 white_point_y;
        u32 red_x;
        u32 red_y;
        u32 green_x;
        u32 green_y;
        u32 blue_x;
        u32 blue_y;
    }

    section gAMA {
        u32 gamma;
    }

    section iCCP {
        string profile_name;
        u8 compression_method;
        byte[..] compressed_profile;
    }

    section sBIT {
        if (parent.ihdr.color_type == 0 || parent.ihdr.color_type == 4) {
            u8 significant_grayscale_bits;
        } else if (parent.ihdr.color_type == 2 || parent.ihdr.color_type == 6) {
            u8 significant_red_bits;
            u8 significant_green_bits;
            u8 significant_blue_bits;
        } else if (parent.ihdr.color_type == 3) {
            u8 significant_red_bits;
            u8 significant_green_bits;
            u8 significant_blue_bits;
        }
    }

    section sRGB {
        u8 rendering_intent;
    }

    section tEXt {
        string keyword;
        string text;
    }

    section zTXt {
        string keyword;
        u8 compression_method;
        byte[..] compressed_text;
    }

    section iTXt {
        string keyword;
        u8 compression_flag;
        u8 compression_method;
        string language_tag;
        string translated_keyword;
        string text;
    }

    section bKGD {
        if (parent.ihdr.color_type == 0 || parent.ihdr.color_type == 4) {
            u16 gray_background;
        } else if (parent.ihdr.color_type == 2 || parent.ihdr.color_type == 6) {
            u16 red_background;
            u16 green_background;
            u16 blue_background;
        } else if (parent.ihdr.color_type == 3) {
            u8 palette_index;
        }
    }

    section pHYs {
        u32 pixels_per_unit_x;
        u32 pixels_per_unit_y;
        u8 unit_specifier;
    }

    section tIME {
        u16 year;
        u8 month;
        u8 day;
        u8 hour;
        u8 minute;
        u8 second;
    }

    section PNGImage {
        byte[8] signature == PNG_SIGNATURE;
        Chunk ihdr_chunk;
        IHDR ihdr = ihdr_chunk.data;
        
        optional Chunk plte_chunk if ihdr.color_type == 3;
        optional PLTE plte = plte_chunk.data if ihdr.color_type == 3;
        
        Chunk[] idat_chunks;
        
        optional Chunk trns_chunk;
        optional tRNS trns = trns_chunk.data;
        
        optional Chunk chrm_chunk;
        optional cHRM chrm = chrm_chunk.data;
        
        optional Chunk gama_chunk;
        optional gAMA gama = gama_chunk.data;
        
        optional Chunk iccp_chunk;
        optional iCCP iccp = iccp_chunk.data;
        
        optional Chunk sbit_chunk;
        optional sBIT sbit = sbit_chunk.data;
        
        optional Chunk srgb_chunk;
        optional sRGB srgb = srgb_chunk.data;
        
        optional Chunk text_chunk;
        optional tEXt text = text_chunk.data;
        
        optional Chunk ztxt_chunk;
        optional zTXt ztxt = ztxt_chunk.data;
        
        optional Chunk itxt_chunk;
        optional iTXt itxt = itxt_chunk.data;
        
        optional Chunk bkgd_chunk;
        optional bKGD bkgd = bkgd_chunk.data;
        
        optional Chunk phys_chunk;
        optional pHYs phys = phys_chunk.data;
        
        optional Chunk time_chunk;
        optional tIME time = time_chunk.data;
        
        Chunk iend_chunk;
    }
}