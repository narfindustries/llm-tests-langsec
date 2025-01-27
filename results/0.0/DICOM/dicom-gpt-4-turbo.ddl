module DICOM {

  type File = struct {
    preamble [128] : u8;
    prefix   [4]   : u8;
    elements       : *Element;
  }

  type Element = struct {
    tag            : Tag;
    vr             : VR;
    length         : u32;
    value          : switch (vr) {
      VR::OB => [length] : u8;
      VR::UI => string(length);
      VR::US => [length / 2] : u16;
      VR::UL => [length / 4] : u32;
      VR::SQ => *Element;
      default => [length] : u8;
    }
  }

  type Tag = struct {
    group   : u16;
    element : u16;
  }

  type VR = enum u16 {
    OB = 0x4F42,
    UI = 0x5549,
    US = 0x5553,
    UL = 0x554C,
    SQ = 0x5351
  }

  type string = struct {
    length : u32;
    data   : [length] : u8;
  }
}