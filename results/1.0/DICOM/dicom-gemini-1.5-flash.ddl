dicom {
  header {
    preamble: bytes:128;
    dicomPrefix: bytes:4;
  }
  metaElement {
    fileMetaInformationGroupLength: uint32_be;
    fileMetaInformationVersion: bytes:2;
    mediaStorageSOPClassUID: string;
    mediaStorageSOPInstanceUID: string;
    transferSyntaxUID: string;
    implementationClassUID: string;
    implementationVersionName: string;
  }
  dataSet {
    elements: seq(element);
  }
  element {
    tag: uint32_be;
    vr: string;
    length: uint32_be;
    value: switch(vr) {
      case "AE": string;
      case "AS": string;
      case "AT": uint32_be;
      case "CS": string;
      case "DA": string;
      case "DT": string;
      case "FD": float64_be;
      case "FL": float32_be;
      case "IS": string;
      case "LO": string;
      case "LT": string;
      case "OB": bytes;
      case "OF": bytes;
      case "OW": bytes;
      case "PN": string;
      case "SH": string;
      case "SL": int32_be;
      case "SQ": seq(element);
      case "SS": int16_be;
      case "ST": string;
      case "TM": string;
      case "UI": string;
      case "UL": uint32_be;
      case "UN": bytes;
      case "US": uint16_be;
      case "UT": string;
      default: bytes;
    }
  }
}
