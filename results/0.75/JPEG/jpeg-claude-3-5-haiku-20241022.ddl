def JPEG = {
  type Marker = enum {
    SOI = 0xD8,
    EOI = 0xD9,
    APP0 = 0xE0,
    APP1 = 0xE1,
    DQT = 0xDB,
    DHT = 0xC4,
    SOF0 = 0xC0,
    SOF2 = 0xC2,
    SOS = 0xDA,
    DRI = 0xDD
  }

  type DensityUnit = enum {
    NoUnits = 0,
    PixelsPerInch = 1,
    PixelsPerCm = 2
  }

  type ComponentID = enum {
    Grayscale = 1,
    RGB = 3
  }

  type HuffmanTableClass = enum {
    DC = 0,
    AC = 1
  }

  type Precision = enum {
    EightBit = 8,
    SixteenBit = 16
  }

  def APP0Segment = {
    marker: 0xFF + Marker.APP0,
    length: u16,
    jfif_identifier: string(5),
    version_major: u8,
    version_minor: u8,
    density_units: DensityUnit,
    x_density: u16,
    y_density: u16,
    thumbnail_width: u8,
    thumbnail_height: u8,
    thumbnail_data?: bytes(thumbnail_width * thumbnail_height * 3)
  }

  def APP1Segment = {
    marker: 0xFF + Marker.APP1,
    length: u16,
    exif_identifier: string,
    exif_data?: bytes
  }

  def QuantizationTable = {
    marker: 0xFF + Marker.DQT,
    length: u16,
    precision: Precision,
    table_destination_id: u8[0-3],
    table_values: match precision {
      Precision.EightBit => u8[64],
      Precision.SixteenBit => u16[64]
    }
  }

  def HuffmanTable = {
    marker: 0xFF + Marker.DHT,
    length: u16,
    table_class: HuffmanTableClass,
    table_destination_id: u8,
    bit_lengths: u8[16],
    huffman_values: u8[]
  }

  def StartOfFrame = {
    marker: 0xFF + (Marker.SOF0 | Marker.SOF2),
    length: u16,
    precision: Precision,
    height: u16,
    width: u16,
    num_components: u8,
    components: {
      id: ComponentID,
      sampling_factors: u8,
      quantization_table_id: u8
    }[num_components]
  }

  def StartOfScan = {
    marker: 0xFF + Marker.SOS,
    length: u16,
    num_scan_components: u8,
    scan_components: {
      selector: ComponentID,
      dc_huffman_table: u8,
      ac_huffman_table: u8
    }[num_scan_components],
    spectral_selection_start: u8,
    spectral_selection_end: u8,
    approximation_high: u4,
    approximation_low: u4
  }

  def RestartInterval = {
    marker: 0xFF + Marker.DRI,
    length: u16,
    restart_interval: u16
  }

  def JPEGFile = {
    start_of_image: 0xFF + Marker.SOI,
    app0_segment?: APP0Segment,
    app1_segment?: APP1Segment,
    quantization_tables: QuantizationTable[],
    huffman_tables: HuffmanTable[],
    start_of_frame: StartOfFrame,
    start_of_scan: StartOfScan,
    restart_interval?: RestartInterval,
    compressed_image_data: bytes,
    end_of_image: 0xFF + Marker.EOI
  }
}