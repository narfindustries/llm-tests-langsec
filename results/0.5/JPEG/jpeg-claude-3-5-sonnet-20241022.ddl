def Jpeg {
    soi:SOI
    segments:Segments*
    eoi:EOI
}

def SOI {
    marker1:u8[1]
    marker2:u8[1]
}

def EOI {
    marker1:u8[1]
    marker2:u8[1]
}

def Segments {
    | APP0 
    | APP1_15 
    | DQT 
    | SOF0_15 
    | DHT 
    | SOS 
    | DRI 
    | COM 
    | RST0_7
}

def APP0 {
    marker1:u8[1]
    marker2:u8[1]
    length:u16[1]
    identifier:JFIF_Identifier
    version:u16[1]
    units:u8[1]
    x_density:u16[1]
    y_density:u16[1]
    thumb_width:u8[1]
    thumb_height:u8[1]
    thumb_data:u8[]
}

def JFIF_Identifier {
    magic:u8[5]
}

def APP1_15 {
    marker1:u8[1]
    marker2:u8[1]
    length:u16[1]
    data:u8[]
}

def DQT {
    marker1:u8[1]
    marker2:u8[1]
    length:u16[1]
    tables:DQT_Table[]
}

def DQT_Table {
    precision_and_id:u8[1]
    qtable:u8[64]
}

def SOF0_15 {
    marker1:u8[1]
    marker2:u8[1]
    length:u16[1]
    precision:u8[1]
    height:u16[1]
    width:u16[1]
    ncomponents:u8[1]
    components:SOF_Component[]
}

def SOF_Component {
    id:u8[1]
    sampling_factors:u8[1]
    qtable_id:u8[1]
}

def DHT {
    marker1:u8[1]
    marker2:u8[1]
    length:u16[1]
    tables:DHT_Table[]
}

def DHT_Table {
    class_and_id:u8[1]
    counts:u8[16]
    values:u8[]
}

def SOS {
    marker1:u8[1]
    marker2:u8[1]
    length:u16[1]
    ncomponents:u8[1]
    components:SOS_Component[]
    start_spectral:u8[1]
    end_spectral:u8[1]
    approx:u8[1]
    entropy_data:EntropyData
}

def SOS_Component {
    id:u8[1]
    tables:u8[1]
}

def DRI {
    marker1:u8[1]
    marker2:u8[1]
    length:u16[1]
    restart_interval:u16[1]
}

def COM {
    marker1:u8[1]
    marker2:u8[1]
    length:u16[1]
    comment:u8[]
}

def RST0_7 {
    marker1:u8[1]
    marker2:u8[1]
}

def EntropyData {
    scan:u8[]
}