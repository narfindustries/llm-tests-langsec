data JPEG = SOI
          | APPn { marker : Marker; data : Bytes }
          | DQT { tables : [QuantizationTable] }
          | DHT { tables : [HuffmanTable] }
          | SOF { baseline : BaselineDCT; progressive : ProgressiveDCT; lossless : Lossless }
          | SOS { components : [ScanComponent]; startOfSpectralSelection : Integer; endOfSpectralSelection : Integer; successiveApproximation : Integer }
          | DRI { restartInterval : Integer }
          | COM { comment : String }
          | EOI

data QuantizationTable = QT { precision : Integer; tableId : Integer; coefficients : [Integer] }

data HuffmanTable = HT { tableClass : Integer; tableId : Integer; lengths : [Integer]; symbols : [Integer] }

data BaselineDCT = Baseline { precision : Integer; height : Integer; width : Integer; components : [Component] }

data ProgressiveDCT = Progressive { precision : Integer; height : Integer; width : Integer; components : [Component] }

data Lossless = Lossless { precision : Integer; height : Integer; width : Integer; components : [Component] }

data Component = Component { componentId : Integer; horizontalSamplingFactor : Integer; verticalSamplingFactor : Integer; quantizationTableId : Integer }

data ScanComponent = ScanComponent { componentId : Integer; dcHuffmanTableId : Integer; acHuffmanTableId : Integer }

data Marker = Marker { markerCode : Integer; length : Integer }

data Bytes = Bytes { bytes : [Byte] }

data Integer = Integer { value : Int }

data Byte = Byte { value : Int }

data String = String { value : Text }

data Int = Int { value : Integer }

data Text = Text { value : String }
