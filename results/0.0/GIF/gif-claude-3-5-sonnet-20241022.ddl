def GIF = struct {
    signature : BYTE[3];
    version : BYTE[3];
    screen_width : UINT16;
    screen_height : UINT16;
    packed : BYTE;
    global_color_table_flag : (packed >> 7);
    color_resolution : (packed >> 4);
    sort_flag : (packed >> 3);
    global_color_table_size : packed;
    background_color_index : BYTE;
    pixel_aspect_ratio : BYTE;
    
    global_color_table : ColorTable[2 ** (global_color_table_size + 1)] if global_color_table_flag == 1;
    
    blocks : Block[];
    trailer : BYTE;
};

def ColorTable[size: UINT] = struct {
    entries : RGB[size];
};

def RGB = struct {
    r : BYTE;
    g : BYTE;
    b : BYTE;
};

def Block = struct {
    block_type : BYTE;
    ImageDescriptor if block_type == 0x2C;
    ExtensionBlock if block_type == 0x21;
};

def ExtensionBlock = struct {
    label : BYTE;
    GraphicControlExtension if label == 0xF9;
    CommentExtension if label == 0xFE;
    PlainTextExtension if label == 0x01;
    ApplicationExtension if label == 0xFF;
};

def ImageDescriptor = struct {
    left : UINT16;
    top : UINT16;
    width : UINT16;
    height : UINT16;
    packed : BYTE;
    local_color_table_flag : (packed >> 7);
    interlace_flag : (packed >> 6);
    sort_flag : (packed >> 5);
    local_color_table_size : packed;
    
    local_color_table : ColorTable[2 ** (local_color_table_size + 1)] if local_color_table_flag == 1;
    
    lzw_minimum_code_size : BYTE;
    data_sub_blocks : DataSubBlocks;
};

def GraphicControlExtension = struct {
    block_size : BYTE;
    packed : BYTE;
    disposal_method : (packed >> 2);
    user_input_flag : (packed >> 1);
    transparent_color_flag : packed;
    delay_time : UINT16;
    transparent_color_index : BYTE;
    terminator : BYTE;
};

def CommentExtension = struct {
    data_sub_blocks : DataSubBlocks;
};

def PlainTextExtension = struct {
    block_size : BYTE;
    text_grid_left : UINT16;
    text_grid_top : UINT16;
    text_grid_width : UINT16;
    text_grid_height : UINT16;
    cell_width : BYTE;
    cell_height : BYTE;
    foreground_color : BYTE;
    background_color : BYTE;
    data_sub_blocks : DataSubBlocks;
};

def ApplicationExtension = struct {
    block_size : BYTE;
    application_identifier : BYTE[8];
    authentication_code : BYTE[3];
    data_sub_blocks : DataSubBlocks;
};

def DataSubBlocks = struct {
    blocks : DataSubBlock[];
    terminator : BYTE;
};

def DataSubBlock = struct {
    size : BYTE;
    data : BYTE[size] if size > 0;
};