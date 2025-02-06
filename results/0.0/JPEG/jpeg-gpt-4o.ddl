Parser JPEG

// Start of Image Marker
SOI = %xFF %xD8

// Application-specific Markers
APP0 = %xFF %xE0
APP1 = %xFF %xE1
APP2 = %xFF %xE2
APPn = %xFF %xE0-FFEF

// Comment Marker
COM = %xFF %xFE

// Define Quantization Table Marker
DQT = %xFF %xDB

// Start of Frame Markers
SOF0 = %xFF %xC0
SOF2 = %xFF %xC2
SOFn = %xFF %xC0-FFCF

// Define Huffman Table Marker
DHT = %xFF %xC4

// Start of Scan Marker
SOS = %xFF %xDA

// End of Image Marker
EOI = %xFF %xD9

// Define Restart Interval Marker
DRI = %xFF %xDD

// Restart Markers
RST0 = %xFF %xD0
RST1 = %xFF %xD1
RST2 = %xFF %xD2
RST3 = %xFF %xD3
RST4 = %xFF %xD4
RST5 = %xFF %xD5
RST6 = %xFF %xD6
RST7 = %xFF %xD7

// JPEG File Structure
JPEGFile = SOI (Segment)* EOI

Segment = APPn SegmentData
        / COM SegmentData
        / DQT SegmentData
        / SOFn FrameHeader
        / DHT HuffmanTable
        / SOS ScanHeader
        / DRI RestartInterval
        / RSTn

SegmentData = Length Data

Length = UINT16

Data = *BYTE

FrameHeader = SOF0FrameHeader / SOF2FrameHeader

SOF0FrameHeader = UINT8 // Sample precision
                UINT16 // Number of lines
                UINT16 // Number of samples per line
                UINT8  // Number of components
                ComponentSpec*

SOF2FrameHeader = SOF0FrameHeader

ComponentSpec = UINT8 // Component ID
              UINT8 // Sampling factors
              UINT8 // Quantization table number

HuffmanTable = Length Data

ScanHeader = UINT8 // Number of components
           ScanComponentSpec*
           UINT8 // Start of spectral selection
           UINT8 // End of spectral selection
           UINT8 // Successive approximation

ScanComponentSpec = UINT8 // Component ID
                  UINT8 // Huffman table selectors

RestartInterval = Length UINT16

BYTE = %x00-FF
UINT8 = BYTE
UINT16 = BYTE BYTE