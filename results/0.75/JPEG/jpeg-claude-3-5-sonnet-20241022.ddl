def Main = JPEG

def JPEG = {
    soi: SOI;
    segments: many Segment;
    eoi: EOI
}

def SOI = {
    magic: [0xFF, 0xD8]
}

def EOI = {
    magic: [0xFF, 0xD9]
}

def Segment = {
    marker: 0xFF;
    type: u8;
    data: case type of {
        0xC0..0xC3 -> FrameHeader,     
        0xC4 -> HuffmanTable,          
        0xC5..0xC7 -> FrameHeader,     
        0xC8..0xCB -> FrameHeader,     
        0xCC..0xCF -> FrameHeader,     
        0xDA -> ScanHeader,            
        0xDB -> QuantizationTable,     
        0xDD -> RestartInterval,       
        0xE0..0xEF -> APPn,            
        0xFE -> Comment,               
        0xD0..0xD7 -> RestartMarker    
    }
}

def FrameHeader = {
    length: u16;
    precision: u8;
    height: u16;
    width: u16;
    num_components: u8;
    components: Component[num_components]
}

def Component = {
    id: u8;
    sampling_factors: u8;
    quantization_table_selector: u8
}

def ScanHeader = {
    length: u16;
    num_components: u8;
    components: ScanComponent[num_components];
    start_spectral: u8;
    end_spectral: u8;
    approx_bit_pos: u8;
    entropy_coded_data: many u8
}

def ScanComponent = {
    component_selector: u8;
    entropy_table_selectors: u8
}

def HuffmanTable = {
    length: u16;
    tables: many HuffmanTableData
}

def HuffmanTableData = {
    table_class_and_id: u8;
    num_codes: u8[16];
    values: u8[Sum(num_codes)]
}

def QuantizationTable = {
    length: u16;
    tables: many QuantizationTableData
}

def QuantizationTableData = {
    precision_and_id: u8;
    values: case (precision_and_id >> 4) of {
        0 -> u8[64],
        1 -> u16[64]
    }
}

def RestartInterval = {
    length: u16;
    interval: u16
}

def APPn = {
    length: u16;
    identifier: u8[5];
    data: u8[length - 7]
}

def Comment = {
    length: u16;
    data: u8[length - 2]
}

def RestartMarker = {
}

def Sum = |xs: u8[]| {
    result: u64 = 0;
    for x in xs {
        result = result + x
    };
    result
}