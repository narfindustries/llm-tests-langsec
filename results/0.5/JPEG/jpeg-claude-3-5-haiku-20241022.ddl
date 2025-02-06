def JPEG = {
    start_of_image: magic(0xFFD8),
    segments: list(Segment),
    end_of_image: magic(0xFFD9)
}

def Segment = 
    | APP0
    | APP1
    | SOF
    | DHT
    | DQT
    | SOS
    | Restart
    | Comment

def APP0 = {
    marker: u16be = 0xFFE0,
    length: u16be,
    identifier: string(5),
    version: u16be,
    units: u8,
    x_density: u16be,
    y_density: u16be,
    thumbnail_width: u8,
    thumbnail_height: u8,
    thumbnail_data: bytes(thumbnail_width * thumbnail_height * 3)
}

def APP1 = {
    marker: u16be = 0xFFE1,
    length: u16be,
    exif_data: bytes(length - 2)
}

def SOF = {
    marker: u16be = choice(0xFFC0, 0xFFC1, 0xFFC2, 0xFFC3),
    length: u16be,
    precision: u8,
    height: u16be,
    width: u16be,
    components_count: u8,
    components: list(Component, components_count)
}

def DHT = {
    marker: u16be = 0xFFC4,
    length: u16be,
    tables: list(HuffmanTable)
}

def DQT = {
    marker: u16be = 0xFFDB,
    length: u16be,
    tables: list(QuantizationTable)
}

def SOS = {
    marker: u16be = 0xFFC0,
    length: u16be,
    components_count: u8,
    components: list(ScanComponent, components_count),
    spectral_start: u8,
    spectral_end: u8,
    approximation: u8,
    compressed_data: bytes
}

def Restart = {
    marker: u16be = choice(0xFFD0, 0xFFD1, 0xFFD2, 0xFFD3, 0xFFD4, 0xFFD5, 0xFFD6, 0xFFD7)
}

def Comment = {
    marker: u16be = 0xFFFE,
    length: u16be,
    comment: string(length - 2)
}

def Component = {
    id: u8,
    sampling_factors: u8,
    quantization_table_id: u8
}

def HuffmanTable = {
    class: u8,
    destination: u8,
    code_lengths: array(16, u8),
    values: bytes(sum(code_lengths))
}

def QuantizationTable = {
    precision: u8,
    elements: array(64, u16be)
}

def ScanComponent = {
    selector: u8,
    huffman_tables: u8
}