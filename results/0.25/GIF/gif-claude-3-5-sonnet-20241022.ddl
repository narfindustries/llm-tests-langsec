def GIF = {
    let signature = "GIF";
    let version = "87a" | "89a";
    let screen_width = UINT16;
    let screen_height = UINT16;
    let packed = UINT8;
    let has_global_color_table = packed >= 128;
    let color_resolution = packed / 16;
    let sort_flag = packed >= 8;
    let global_color_table_size = 2 * (packed + 1);
    let background_color_index = UINT8;
    let pixel_aspect_ratio = UINT8;
    let global_color_table = if has_global_color_table {
        let colors = UINT8[3 * global_color_table_size]
    };
    let blocks = Block*;
    let trailer = 0x3B;
}

def Block = ImageBlock | Extension

def ImageBlock = {
    let separator = 0x2C;
    let left = UINT16;
    let top = UINT16;
    let width = UINT16;
    let height = UINT16;
    let packed = UINT8;
    let has_local_color_table = packed >= 128;
    let interlace = packed >= 64;
    let sort = packed >= 32;
    let local_color_table_size = 2 * (packed + 1);
    let local_color_table = if has_local_color_table {
        let colors = UINT8[3 * local_color_table_size]
    };
    let lzw_min_code_size = UINT8;
    let data_sub_blocks = DataSubBlocks;
}

def Extension = GraphicControlExt | CommentExt | PlainTextExt | ApplicationExt

def GraphicControlExt = {
    let introducer = 0x21;
    let label = 0xF9;
    let block_size = 0x04;
    let packed = UINT8;
    let disposal_method = packed / 4;
    let user_input = packed >= 2;
    let transparent_color_flag = packed >= 1;
    let delay_time = UINT16;
    let transparent_color_index = UINT8;
    let terminator = 0x00;
}

def CommentExt = {
    let introducer = 0x21;
    let label = 0xFE;
    let data_sub_blocks = DataSubBlocks;
}

def PlainTextExt = {
    let introducer = 0x21;
    let label = 0x01;
    let block_size = 0x0C;
    let text_grid_left = UINT16;
    let text_grid_top = UINT16;
    let text_grid_width = UINT16;
    let text_grid_height = UINT16;
    let cell_width = UINT8;
    let cell_height = UINT8;
    let fg_color_index = UINT8;
    let bg_color_index = UINT8;
    let data_sub_blocks = DataSubBlocks;
}

def ApplicationExt = {
    let introducer = 0x21;
    let label = 0xFF;
    let block_size = 0x0B;
    let application_identifier = UINT8[8];
    let authentication_code = UINT8[3];
    let data_sub_blocks = DataSubBlocks;
}

def DataSubBlocks = {
    let blocks = SubBlock*;
    let terminator = 0x00;
}

def SubBlock = {
    let size = UINT8;
    let data = UINT8[size];
}