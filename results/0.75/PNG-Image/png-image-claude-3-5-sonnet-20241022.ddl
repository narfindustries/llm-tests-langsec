def Main = PNG_Signature PNG_Chunks

def PNG_Signature = $[137'u8] $[80'u8] $[78'u8] $[71'u8] $[13'u8] $[10'u8] $[26'u8] $[10'u8]

def PNG_Chunks = many PNG_Chunk

def PNG_Chunk = {
    length: UInt32BE
    type: FourCC
    data: Take length
    crc: UInt32BE
}

def UInt32BE = BE32

def FourCC = {
    first_char: $[(65..90)'u8, (97..122)'u8]
    second_char: $[(65..90)'u8, (97..122)'u8]
    third_char: $[(65..90)'u8, (97..122)'u8]
    fourth_char: $[(65..90)'u8, (97..122)'u8]
}

def Take n = seq uint8 n