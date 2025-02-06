JPEG
  = SOI Frame* EOI

SOI
  = Marker 0xFFD8

EOI
  = Marker 0xFFD9

Frame
  = SOF | DHT | DAC | DQT | DRI | SOS | RST | APP | COM | TEM

SOF
  = SOF0 | SOF1 | SOF2 | SOF3 | SOF5 | SOF6 | SOF7 | SOF9 | SOF10 | SOF11 | SOF13 | SOF14 | SOF15

SOF0
  = Marker 0xFFC0 FrameHeader

SOF1
  = Marker 0xFFC1 FrameHeader

SOF2
  = Marker 0xFFC2 FrameHeader

SOF3
  = Marker 0xFFC3 FrameHeader

SOF5
  = Marker 0xFFC5 FrameHeader

SOF6
  = Marker 0xFFC6 FrameHeader

SOF7
  = Marker 0xFFC7 FrameHeader

SOF9
  = Marker 0xFFC9 FrameHeader

SOF10
  = Marker 0xFFCA FrameHeader

SOF11
  = Marker 0xFFCB FrameHeader

SOF13
  = Marker 0xFFCD FrameHeader

SOF14
  = Marker 0xFFCE FrameHeader

SOF15
  = Marker 0xFFCF FrameHeader

FrameHeader
  = Precision: U8 Height: U16 Width: U16 NumComponents: U8 Components: Component[NumComponents]

Component
  = ComponentID: U8 SamplingFactors: U8 QuantizationTable: U8

DHT
  = Marker 0xFFC4 HuffmanTable

HuffmanTable
  = TableInfo: U8 CodeLengths: U8[16] CodeValues: U8[Sum(CodeLengths)]

DAC
  = Marker 0xFFCC ArithmeticCodingTable

ArithmeticCodingTable
  = TableInfo: U8 ArithmeticParameters: U8[]

DQT
  = Marker 0xFFDB QuantizationTable

QuantizationTable
  = PrecisionTableID: U8 QuantizationValues: U8[]

DRI
  = Marker 0xFFDD RestartInterval: U16

SOS
  = Marker 0xFFDA ScanHeader

ScanHeader
  = NumComponents: U8 Components: ScanComponent[NumComponents] SpectralSelection: U16 SuccessiveApprox: U16

ScanComponent
  = ComponentSelector: U8 DCTableSelector: U8 ACTableSelector: U8

RST
  = Marker 0xFFD0 | Marker 0xFFD1 | Marker 0xFFD2 | Marker 0xFFD3 | Marker 0xFFD4 | Marker 0xFFD5 | Marker 0xFFD6 | Marker 0xFFD7

APP
  = Marker 0xFFE0 ApplicationData | Marker 0xFFE1 ApplicationData | Marker 0xFFE2 ApplicationData | Marker 0xFFE3 ApplicationData | Marker 0xFFE4 ApplicationData | Marker 0xFFE5 ApplicationData | Marker 0xFFE6 ApplicationData | Marker 0xFFE7 ApplicationData | Marker 0xFFE8 ApplicationData | Marker 0xFFE9 ApplicationData | Marker 0xFFEA ApplicationData | Marker 0xFFEB ApplicationData | Marker 0xFFEC ApplicationData | Marker 0xFFED ApplicationData | Marker 0xFFEE ApplicationData | Marker 0xFFEF ApplicationData

ApplicationData
  = Identifier: U8[] Data: U8[]

COM
  = Marker 0xFFFE CommentData

CommentData
  = Text: U8[]

TEM
  = Marker 0xFF01

Marker
  = U8(0xFF) U8

U8
  = { UInt8 }

U16
  = { UInt16 }