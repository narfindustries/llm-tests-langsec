JPEG = {
  SOI: 0xFFD8,
  segments: Segment*,
  EOI: 0xFFD9
};

Segment =
  APPnSegment
  | SOFSegment
  | DHTSegment
  | DQTSegment
  | DRISegment
  | SOSSegment
  | RSTnSegment
  | COMSegment;

APPnSegment = {
  marker: 0xFFE0..0xFFEF,
  length: UInt16,
  identifier: Bytes(length - 2)
};

SOFSegment = {
  marker: 0xFFC0..0xFFCF,
  length: UInt16,
  precision: UInt8,
  height: UInt16,
  width: UInt16,
  numComponents: UInt8,
  components: Component[numComponents]
};

Component = {
  id: UInt8,
  samplingFactors: UInt8,
  quantizationTable: UInt8
};

DHTSegment = {
  marker: 0xFFC4,
  length: UInt16,
  tables: HuffmanTable*
};

HuffmanTable = {
  tableClass: UInt4,
  tableId: UInt4,
  codeLengths: UInt8[16],
  codeValues: UInt8[sum(codeLengths)]
};

DQTSegment = {
  marker: 0xFFDB,
  length: UInt16,
  tables: QuantizationTable*
};

QuantizationTable = {
  precision: UInt4,
  tableId: UInt4,
  values: UInt8[64] | UInt16[64]
};

DRISegment = {
  marker: 0xFFDD,
  length: UInt16,
  restartInterval: UInt16
};

SOSSegment = {
  marker: 0xFFDA,
  length: UInt16,
  numComponents: UInt8,
  componentSelectors: ComponentSelector[numComponents],
  spectralSelectionStart: UInt8,
  spectralSelectionEnd: UInt8,
  successiveApproximation: UInt8,
  entropyCodedData: Bytes(length - 6 - numComponents * 2)
};

ComponentSelector = {
  componentId: UInt8,
  dcTable: UInt4,
  acTable: UInt4
};

RSTnSegment = {
  marker: 0xFFD0..0xFFD7
};

COMSegment = {
  marker: 0xFFFE,
  length: UInt16,
  comment: Bytes(length - 2)
};