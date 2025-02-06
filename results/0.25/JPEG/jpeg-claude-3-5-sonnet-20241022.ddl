def SOI = 0xFFD8
def EOI = 0xFFD9

def APP0 = 0xFFE0
def APP1 = 0xFFE1
def APP2 = 0xFFE2
def APP3 = 0xFFE3
def APP4 = 0xFFE4
def APP5 = 0xFFE5
def APP6 = 0xFFE6
def APP7 = 0xFFE7
def APP8 = 0xFFE8
def APP9 = 0xFFE9
def APP10 = 0xFFEA
def APP11 = 0xFFEB
def APP12 = 0xFFEC
def APP13 = 0xFFED
def APP14 = 0xFFEE
def APP15 = 0xFFEF

def COM = 0xFFFE
def DQT = 0xFFDB
def SOF0 = 0xFFC0
def SOF1 = 0xFFC1
def SOF2 = 0xFFC2
def SOF3 = 0xFFC3
def SOF5 = 0xFFC5
def SOF6 = 0xFFC6
def SOF7 = 0xFFC7
def SOF9 = 0xFFC9
def SOF10 = 0xFFCA
def SOF11 = 0xFFCB
def SOF13 = 0xFFCD
def SOF14 = 0xFFCE
def SOF15 = 0xFFCF
def DHT = 0xFFC4
def DRI = 0xFFDD
def SOS = 0xFFDA

def JPEG = {
    magic: u16
    segments: Segment*
    end: u16
}

def Segment = {
    marker: u16
    case marker {
        SOI -> ()
        EOI -> ()
        _ -> {
            length: u16
            data: u8[length-2]
        }
    }
}

def FrameHeader = {
    length: u16
    precision: u8
    height: u16
    width: u16
    components: u8
    component_data: ComponentData[components]
}

def ComponentData = {
    id: u8
    sampling_factors: u8
    qtable_selector: u8
}

def ScanHeader = {
    length: u16
    components: u8
    component_specs: ComponentSpec[components]
    start_spectral: u8
    end_spectral: u8
    approx: u8
}

def ComponentSpec = {
    id: u8
    table_selector: u8
}

def QuantizationTable = {
    length: u16
    table_info: u8
    precision: (table_info >> 4) & 0x0F
    table_id: table_info & 0x0F
    values: if precision == 0 { u8[64] } else { u16[64] }
}

def HuffmanTable = {
    length: u16
    table_info: u8
    table_class: (table_info >> 4) & 0x0F
    table_id: table_info & 0x0F
    num_codes: u8[16]
    values: u8[sum(num_codes)]
}

def RestartInterval = {
    length: u16
    interval: u16
}

def ApplicationData = {
    length: u16
    identifier: u8[5]
    version: u16
    units: u8
    x_density: u16
    y_density: u16
    x_thumbnail: u8
    y_thumbnail: u8
    thumbnail_data: u8[x_thumbnail * y_thumbnail * 3]
}

def Comment = {
    length: u16
    data: u8[length-2]
}

def ImageData = u8*