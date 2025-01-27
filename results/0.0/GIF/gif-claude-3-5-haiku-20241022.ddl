type GIF {
    header: Header,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<ColorTable>,
    blocks: List<Block>
}

type Header {
    signature: [Char; 3],
    version: [Char; 3]
}

type LogicalScreenDescriptor {
    width: U16,
    height: U16,
    packed_fields: PackedFields,
    background_color_index: U8,
    pixel_aspect_ratio: U8
}

type PackedFields {
    global_color_table_flag: Bool,
    color_resolution: U3,
    sort_flag: Bool,
    global_color_table_size: U3
}

type ColorTable {
    colors: List<Color>
}

type Color {
    red: U8,
    green: U8,
    blue: U8
}

type Block {
    content: BlockContent
}

type BlockContent {
    | ImageDescriptor(ImageDescriptorBlock)
    | GraphicControlExtension(GraphicControlExtensionBlock)
    | Trailer
}

type ImageDescriptorBlock {
    separator: U8,
    left: U16,
    top: U16,
    width: U16,
    height: U16,
    local_color_table_flag: Bool,
    interlace_flag: Bool,
    sort_flag: Bool,
    local_color_table_size: U3,
    local_color_table: Option<ColorTable>,
    image_data: ImageData
}

type ImageData {
    lzw_minimum_code_size: U8,
    data_blocks: List<DataBlock>
}

type DataBlock {
    size: U8,
    data: [U8]
}

type GraphicControlExtensionBlock {
    extension_introducer: U8,
    graphic_control_label: U8,
    block_size: U8,
    packed_fields: GraphicControlPackedFields,
    delay_time: U16,
    transparent_color_index: U8
}

type GraphicControlPackedFields {
    reserved: U3,
    disposal_method: U3,
    user_input_flag: Bool,
    transparent_color_flag: Bool
}

let parse_gif = {
    header: {
        signature: ['G', 'I', 'F'],
        version: ['8', '9', 'a']
    },
    logical_screen_descriptor: {
        width: 100,
        height: 100,
        packed_fields: {
            global_color_table_flag: true,
            color_resolution: 7,
            sort_flag: false,
            global_color_table_size: 7
        },
        background_color_index: 0,
        pixel_aspect_ratio: 0
    },
    global_color_table: Some({
        colors: [
            { red: 255, green: 0, blue: 0 },
            { red: 0, green: 255, blue: 0 },
            { red: 0, green: 0, blue: 255 }
        ]
    }),
    blocks: [
        {
            content: ImageDescriptor({
                separator: 0x2C,
                left: 0,
                top: 0,
                width: 100,
                height: 100,
                local_color_table_flag: false,
                interlace_flag: false,
                sort_flag: false,
                local_color_table_size: 0,
                local_color_table: None,
                image_data: {
                    lzw_minimum_code_size: 8,
                    data_blocks: [
                        { size: 10, data: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] }
                    ]
                }
            })
        },
        {
            content: Trailer
        }
    ]
}