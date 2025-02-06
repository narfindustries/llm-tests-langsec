def PNG = {
    let SIGNATURE = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

    def ChunkType = {
        ancillary: bool     
        private: bool       
        reserved: bool      
        safe_to_copy: bool  
        name: uint[32]      
    }

    def Chunk = {
        length: uint32
        type: ChunkType
        data: uint8[length]
        crc: uint32
    }

    def IHDR = {
        width: uint32
        height: uint32
        bit_depth: uint8 in [1,2,4,8,16]
        color_type: uint8 in [0,2,3,4,6]
        compression: uint8 == 0
        filter: uint8 == 0
        interlace: uint8 in [0,1]
    }

    def PLTE = {
        entries: (uint8,uint8,uint8)[]
    }

    def IDAT = {
        compressed_data: uint8[]
    }

    def tRNS = {
        data: uint8[]
    }

    def cHRM = {
        white_point_x: uint32
        white_point_y: uint32
        red_x: uint32
        red_y: uint32
        green_x: uint32
        green_y: uint32
        blue_x: uint32
        blue_y: uint32
    }

    def gAMA = {
        gamma: uint32
    }

    def iCCP = {
        name: uint8[] until 0x00
        compression: uint8
        profile: uint8[]
    }

    def sBIT = {
        significant_bits: uint8[]
    }

    def sRGB = {
        intent: uint8 in [0,1,2,3]
    }

    def tEXt = {
        keyword: uint8[] until 0x00
        text: uint8[]
    }

    def zTXt = {
        keyword: uint8[] until 0x00
        compression: uint8
        text: uint8[]
    }

    def iTXt = {
        keyword: uint8[] until 0x00
        compression_flag: uint8
        compression_method: uint8
        language_tag: uint8[] until 0x00
        translated_keyword: uint8[] until 0x00
        text: uint8[]
    }

    def bKGD = {
        data: uint8[]
    }

    def pHYs = {
        pixels_per_unit_x: uint32
        pixels_per_unit_y: uint32
        unit: uint8 in [0,1]
    }

    def tIME = {
        year: uint16
        month: uint8 in 1..12
        day: uint8 in 1..31
        hour: uint8 in 0..23
        minute: uint8 in 0..59
        second: uint8 in 0..60
    }

    def sPLT = {
        name: uint8[] until 0x00
        sample_depth: uint8 in [8,16]
        entries: uint8[]
    }

    def IEND = {}

    sig: SIGNATURE
    ihdr_chunk: Chunk where type.name == "IHDR"
    plte_chunk: optional Chunk where type.name == "PLTE"
    other_chunks: Chunk[] where type.name != "IEND"
    iend_chunk: Chunk where type.name == "IEND"
}