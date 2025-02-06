data NITF = NITF { fileHeader: FileHeader, segments: [Segment] }

data FileHeader = FileHeader {
  signature: String,
  version: String,
  uniqueFileIdentifier: String,
  offsetToNextSegment: Integer,
  optionalFields: [(String, FieldValue)]
}

data Segment = ImageSegment { header: ImageSegmentHeader, data: ImageData } |
              TextSegment { header: TextSegmentHeader, data: TextData } |
              GenericSegment { header: GenericSegmentHeader, data: GenericData } |
              UnknownSegment { header: [Byte], data: [Byte] }


data ImageSegmentHeader = ImageSegmentHeader { optionalFields: [(String, FieldValue)] }
data ImageData = ImageData { data: [Byte] }

data TextSegmentHeader = TextSegmentHeader { optionalFields: [(String, FieldValue)] }
data TextData = TextData { data: String }

data GenericSegmentHeader = GenericSegmentHeader { optionalFields: [(String, FieldValue)] }
data GenericData = GenericData { data: [Byte] }

data FieldValue = StringValue String | IntegerValue Integer | ByteValue Byte | BooleanValue Boolean | FloatValue Float | DoubleValue Double
