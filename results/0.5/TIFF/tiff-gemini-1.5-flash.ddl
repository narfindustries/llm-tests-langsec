data TIFF = TIFF {
  ifd : IFD
}

data IFD = IFD {
  entries : [Entry]
  nextIFDOffset : Maybe UInt32
}

data Entry = Entry {
  tag : UInt16
  type : Type
  count : UInt32
  value : Value
}

data Type = BYTE | ASCII | SHORT | LONG | RATIONAL | SBYTE | SSHORT | SLONG | FLOAT | DOUBLE | UNDEFINED

data Value = ByteValue { value : UInt8 } | AsciiValue { value : String } | ShortValue { value : UInt16 } | LongValue { value : UInt32 } | RationalValue { value : Rational } | SByteValue { value : Int8 } | SShortValue { value : Int16 } | SLongValue { value : Int32 } | FloatValue { value : Float } | DoubleValue { value : Double } | UndefinedValue { value : [UInt8] }

data Rational = Rational {
  numerator : UInt32
  denominator : UInt32
}


-- Example tags (incomplete and simplified)  These are placeholders.  A complete specification would be much larger.
--  You'd need to add all TIFF tags with appropriate type and value constraints.  The 'value' field would need to be adjusted to match the actual data type and size for each tag.  This is a highly simplified example and does not reflect the full complexity of the TIFF specification.

-- Example: ImageWidth tag
type ImageWidthEntry = Entry {
  tag : 256
  type : LONG
  count : 1
  value : LongValue { value : UInt32 }
}

-- Example: ImageLength tag
type ImageLengthEntry = Entry {
  tag : 257
  type : LONG
  count : 1
  value : LongValue { value : UInt32 }
}

-- Example: BitsPerSample tag (allowing for multiple samples)
type BitsPerSampleEntry = Entry {
  tag : 258
  type : SHORT
  count : UInt32 -- Variable number of samples
  value : [ShortValue { value : UInt16 }]
}

-- Example: Compression tag
type CompressionEntry = Entry {
  tag : 259
  type : SHORT
  count : 1
  value : ShortValue { value : UInt16 }
}

-- Example: PhotometricInterpretation tag
type PhotometricInterpretationEntry = Entry {
  tag : 262
  type : SHORT
  count : 1
  value : ShortValue { value : UInt16 }
}

-- Example: StripOffsets tag (allowing for multiple strips)
type StripOffsetsEntry = Entry {
  tag : 273
  type : LONG
  count : UInt32 -- Variable number of strips
  value : [LongValue { value : UInt32 }]
}

-- ... many more tags would need to be defined similarly ...
