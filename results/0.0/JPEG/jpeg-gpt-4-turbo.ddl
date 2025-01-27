module JPEG.GPT4Turbo {
  import JPEG.Common;

  type JPEGFile = struct {
    segments: Segment*;
  }

  type Segment = struct {
    marker: Marker;
    data: switch (marker) {
      case SOI, EOI -> void;
      case SOS -> ScanData;
      default -> SegmentData;
    }
  }

  type Marker = enum u16 {
    SOI = 0xFFD8, // Start of Image
    EOI = 0xFFD9, // End of Image
    SOS = 0xFFDA, // Start of Scan
    APP0 = 0xFFE0, // Application Segment 0 (JFIF)
    APP1 = 0xFFE1, // Application Segment 1 (EXIF)
    DQT = 0xFFDB, // Define Quantization Table
    SOF0 = 0xFFC0, // Start of Frame (Baseline DCT)
    DHT = 0xFFC4, // Define Huffman Table
    DRI = 0xFFDD  // Define Restart Interval
  }

  type SegmentData = struct {
    length: u16;
    data: bytes(length - 2);
  }

  type ScanData = struct {
    length: u16;
    components: u8;
    componentDetails: ComponentDetail[components];
    spectralSelectionStart: u8;
    spectralSelectionEnd: u8;
    successiveApproximation: u8;
    imageData: bytes;
  }

  type ComponentDetail = struct {
    componentId: u8;
    huffmanTable: u8;
  }

  type Common = struct {
    // Common JPEG structures and utility functions
    // Placeholder for common definitions
  }
}