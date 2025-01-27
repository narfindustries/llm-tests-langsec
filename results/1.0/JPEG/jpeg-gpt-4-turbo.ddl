module JPEG {
  import types {
    type u8  = UInt : 8;
    type u16 = UInt : 16;
  }

  type ImageFile = struct {
    soi : Marker_SOI;
    app  : Marker_APP?;
    frames : Frame+;
    eoi : Marker_EOI;
  }

  type Marker_SOI = struct {
    marker: 0xFFD8 : u16;
  }

  type Marker_APP = struct {
     marker : 0xFFE0 : u16;
     size : u16;
     data : u8[size-2];
  }

  type Marker_EOI = struct {
    marker : 0xFFD9 : u16;
  }

  type Frame = struct {
    frameHeader: FrameHeader;
    scans: Scan+;
  }

  type FrameHeader = struct {
    marker : 0xFFC0 : u16;
    length : u16;
    precision : u8;
    height : u16;
    width : u16;
    numComponents : u8;
    components : Component[numComponents];
  }

  type Component = struct {
    componentId : u8;
    samplingFactors : u8;
    quantizationTableId : u8;
  }

  type Scan = struct {
    scanHeader : ScanHeader;
    huffmanData : u8[];
  }

  type ScanHeader = struct {
    marker : 0xFFDA : u16;
    length : u16;
    numComponents : u8;
    componentSelector : ScanComponent[numComponents];
    spectralStart : u8;
    spectralEnd : u8;
    approxHigh : u8;
    approxLow : u8;
  }

  type ScanComponent = struct {
    componentId : u8;
    huffmanTableDC : u8;
    huffmanTableAC : u8;
  }
}