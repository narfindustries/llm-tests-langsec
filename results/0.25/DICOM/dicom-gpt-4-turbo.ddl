module DICOM;

import std::int;
import std::array;

type Preamble = array[u8, 128];

type DICOMPrefix = struct {
  magic: array[u8, 4] = "DICM";
};

type VR = enum[u16] {
  OB = 0x4F42,
  OW = 0x4F57,
  SQ = 0x5351,
  UI = 0x5549,
  UL = 0x554C,
  UN = 0x554E,
  US = 0x5553,
  UT = 0x5554
};

type Length = choice {
  explicit: u32;
  undefined: u32 = 0xFFFFFFFF;
};

type Element = struct {
  tag_group: u16;
  tag_element: u16;
  vr: VR;
  reserved: u16;
  length: Length;
  value: array[u8, self.length.explicit] if self.length.explicit != 0xFFFFFFFF;
};

type SequenceItem = struct {
  item_tag_group: u16 = 0xFFFE;
  item_tag_element: u16 = 0xE000;
  item_length: Length;
  item_content: array[Element, *] if self.item_length.explicit != 0xFFFFFFFF;
};

type Sequence = array[SequenceItem, *];

type DataElement = struct {
  tag_group: u16;
  tag_element: u16;
  vr: VR;
  length: Length;
  value: choice {
    short_value: array[u8, self.length.explicit] if self.vr != VR::SQ;
    sequence_value: Sequence if self.vr == VR::SQ;
  };
};

type DICOMFile = struct {
  preamble: Preamble;
  prefix: DICOMPrefix;
  elements: array[DataElement, *];
};