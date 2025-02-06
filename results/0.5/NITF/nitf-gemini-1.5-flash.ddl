module NITF {
  type FileHeader = {
    signature: String8,
    version: String4,
    headerLength: UInt32,
    securityMetadata: SecurityMetadata,
    imageHeaderLocation: UInt32,
    logicalFileLength: UInt64,
    optionalFields: OptionalFields,
  }

  type ImageHeader = {
    signature: String8,
    version: String4,
    headerLength: UInt32,
    imageDataLocation: UInt32,
    imageDimensions: { rows: UInt32, cols: UInt32, bands: UInt16 },
    imageDataType: ImageDataType,
    imageCompression: CompressionType,
    geographicInformation: GeographicInfo,
    metadataKeywords: List String,
    optionalFields: OptionalFields,
  }

  type SecurityMetadata = {
    classification: String,
    handlingInstructions: String,
  }

  type GeographicInfo = {
    projection: ProjectionType,
    coordinates: List Coordinate,
  }

  type Coordinate = {
    latitude: Float64,
    longitude: Float64,
    altitude: Float64?,
  }

  type ImageDataType = Enum { UINT8, INT16, UINT16, FLOAT32, FLOAT64 }
  type CompressionType = Enum { NONE, JPEG, JPEG2000 }
  type ProjectionType = Enum { UTM, WGS84 }

  type OptionalFields = {
    fields: Map String Any,
  }

  type NITFFile = {
    fileHeader: FileHeader,
    imageHeaders: List ImageHeader,
    dataSegments: List DataSegment,
  }

  type DataSegment = {
    type: String,
    data: Bytes,
  }

  type String8 = String { length: 8 }
  type String4 = String { length: 4 }
  type List a = [a]
  type Map k v = { entries: [(k, v)] }
  type Any = oneof { UInt8, UInt16, UInt32, UInt64, Int8, Int16, Int32, Int64, Float32, Float64, String, Bytes, Boolean }

}
