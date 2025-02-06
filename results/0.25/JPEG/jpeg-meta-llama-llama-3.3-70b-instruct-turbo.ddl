jpeg = Struct(
  soi: Bytes(2) == b"\xFF\xD8",
  app0: app0,
  app1: Array(app1),
  dqt: Array(dqt),
  dht: Array(dht),
  sof0: sof0,
  sof2: Array(sof2),
  dri: Optional(dri),
  sos: Array(sos),
  scanData: Array(scanData),
  rst: Optional(rst),
  eoi: Bytes(2) == b"\xFF\xD9"
)

app0 = Struct(
  marker: Bytes(2) == b"\xFF\xE0",
  length: UInt16,
  identifier: String == "JFIF\0",
  version: UInt16,
  units: UInt8,
  xDensity: UInt16,
  yDensity: UInt16,
  xThumbnail: UInt8,
  yThumbnail: UInt8
)

app1 = Struct(
  marker: Bytes(2) == b"\xFF\xE1",
  length: UInt16,
  identifier: String
)

dqt = Struct(
  marker: Bytes(2) == b"\xFF\xDB",
  length: UInt16,
  tableNumber: UInt8,
  precision: UInt8,
  tableData: Array(UInt8)
)

dht = Struct(
  marker: Bytes(2) == b"\xFF\xC4",
  length: UInt16,
  tableClass: UInt8,
  tableNumber: UInt8,
  tableData: Array(UInt8)
)

sof0 = Struct(
  marker: Bytes(2) == b"\xFF\xC0",
  length: UInt16,
  precision: UInt8,
  imageHeight: UInt16,
  imageWidth: UInt16,
  numberOfComponents: UInt8,
  component: Array(component)
)

sof2 = Struct(
  marker: Bytes(2) == b"\xFF\xC2",
  length: UInt16,
  precision: UInt8,
  imageHeight: UInt16,
  imageWidth: UInt16,
  numberOfComponents: UInt8,
  component: Array(component)
)

component = Struct(
  componentIdentifier: UInt8,
  horizontalSamplingFactor: UInt8,
  verticalSamplingFactor: UInt8,
  quantizationTableNumber: UInt8
)

dri = Struct(
  marker: Bytes(2) == b"\xFF\xDD",
  length: UInt16,
  restartInterval: UInt16
)

sos = Struct(
  marker: Bytes(2) == b"\xFF\xDA",
  length: UInt16,
  numberOfComponents: UInt8,
  component: Array(sosComponent)
)

sosComponent = Struct(
  componentIdentifier: UInt8,
  dcEntropyCodingTableNumber: UInt8,
  acEntropyCodingTableNumber: UInt8
)

scanData = Struct(
  data: Array(UInt8)
)

rst = Struct(
  marker: Bytes(2)
)