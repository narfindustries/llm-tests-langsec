module DICOM {
  struct FileHeader {
    magic: bytes(size = 128);
    prefix: string(size = 4, encoding = "ASCII");

    elements: seq[DataElement];
  }

  struct DataElement {
    tagGroup: u16;
    tagElement: u16;
    vr: string(size = 2, encoding = "ASCII");
    length: u32;
    value: switch (self.vr) {
      case "AE": string(size = self.length, encoding = "ASCII");
      case "AS": string(size = self.length, encoding = "ASCII");
      case "AT": u16;
      case "CS": string(size = self.length, encoding = "ASCII");
      case "DA": string(size = self.length, encoding = "ASCII");
      case "DS": string(size = self.length, encoding = "ASCII");
      case "DT": string(size = self.length, encoding = "ASCII");
      case "FL": f32;
      case "FD": f64;
      case "IS": string(size = self.length, encoding = "ASCII");
      case "LO": string(size = self.length, encoding = "ASCII");
      case "LT": string(size = self.length, encoding = "ASCII");
      case "OB": bytes(size = self.length);
      case "OD": bytes(size = self.length);
      case "OF": bytes(size = self.length);
      case "OW": bytes(size = self.length);
      case "PN": string(size = self.length, encoding = "ASCII");
      case "SH": string(size = self.length, encoding = "ASCII");
      case "SL": i32;
      case "SQ": seq[DataElement];
      case "SS": i16;
      case "ST": string(size = self.length, encoding = "ASCII");
      case "TM": string(size = self.length, encoding = "ASCII");
      case "UI": string(size = self.length, encoding = "ASCII");
      case "UL": u32;
      case "UN": bytes(size = self.length);
      case "US": u16;
      case "UT": string(size = self.length, encoding = "ASCII");
      default: bytes(size = self.length);
    };
  }
}