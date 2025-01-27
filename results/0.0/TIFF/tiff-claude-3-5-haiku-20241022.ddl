format TIFF {
    byteOrder: choice {
        LittleEndian: 0x49 0x49,
        BigEndian: 0x4D 0x4D
    }

    magicNumber: 0x2A 0x00 if byteOrder == LittleEndian
               | 0x00 0x2A if byteOrder == BigEndian

    numDirEntries: uint16 

    dirEntries: dirEntry[numDirEntries]

    dirEntry {
        tag: uint16
        fieldType: uint16
        count: uint32
        valueOrOffset: uint32
    }

    imageWidth: uint32 
    imageHeight: uint32
    bitsPerSample: uint16
    compression: uint16
    photometricInterpretation: uint16
    stripOffsets: uint32[]
    rowsPerStrip: uint32
    stripByteCounts: uint32[]

    imageData: byte[totalStripBytes]

    constraints {
        numDirEntries > 0
        numDirEntries <= 256
        bitsPerSample in [1, 4, 8, 16]
        compression in [1, 2, 3, 4, 5]
        photometricInterpretation in [0, 1, 2, 3]
    }

    computed {
        totalStripBytes = sum(stripByteCounts)
    }
}