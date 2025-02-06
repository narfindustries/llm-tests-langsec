def SOI = 0xFFD8
def EOI = 0xFFD9

def APP0 = 0xFFE0
def APP1_15 = 0xFFE1..0xFFEF
def COM = 0xFFFE
def DQT = 0xFFDB
def DHT = 0xFFC4
def DRI = 0xFFDD
def SOF0 = 0xFFC0
def SOF1 = 0xFFC1
def SOF2 = 0xFFC2
def SOF3 = 0xFFC3
def SOF5_15 = 0xFFC5..0xFFCF
def SOS = 0xFFDA
def RST0_7 = 0xFFD0..0xFFD7

def JPEG = {
    magic: SOI
    segments: Array<Segment>
    end: EOI
}

def Segment = {
    marker: APP0 | APP1_15 | COM | DQT | DHT | DRI | SOF0 | SOF1 | SOF2 | SOF3 | SOF5_15 | SOS | RST0_7
    if marker != SOS {
        length: u16
        data: Array<u8>(length-2)
    } else {
        length: u16
        scan_components: u8
        component_data: Array<ComponentSpec>(scan_components)
        spectral_start: u8
        spectral_end: u8
        approx: u8
        entropy_data: Array<u8>
    }
}

def ComponentSpec = {
    id: u8
    dc_ac_tables: u8
}

def QuantizationTable = {
    precision_and_id: u8
    values: Array<u8>(64)
}

def HuffmanTable = {
    class_and_id: u8
    counts: Array<u8>(16)
    values: Array<u8>(sum(counts))
}

def FrameHeader = {
    length: u16
    precision: u8
    height: u16
    width: u16
    components: u8
    component_specs: Array<ComponentInfo>(components)
}

def ComponentInfo = {
    id: u8
    sampling_factors: u8
    quant_table_num: u8
}

def JFIFHeader = {
    length: u16
    identifier: Array<u8>(5)
    version: u16
    units: u8
    x_density: u16
    y_density: u16
    thumb_width: u8
    thumb_height: u8
    thumb_data: Array<u8>(thumb_width * thumb_height * 3)
}

def RestartInterval = {
    length: u16
    interval: u16
}

def Comment = {
    length: u16
    data: Array<u8>(length-2)
}

def ApplicationData = {
    length: u16
    data: Array<u8>(length-2)
}

def EntropyCodedData = {
    data: Array<u8>
}