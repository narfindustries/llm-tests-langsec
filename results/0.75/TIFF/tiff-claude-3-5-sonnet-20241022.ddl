def Main = {
  Tiff;
}

def Tiff = {
  Header ImageFileHeader EndianTag
  IFDList
}

def EndianTag = {
  | LittleEndian = "II"
  | BigEndian = "MM"
}

def Header = {
  byteOrder:EndianTag
  fortyTwo:uint16 = 42
  ifdOffset:uint32
}

def IFDList = {
  IFD*
}

def IFD = {
  entryCount:uint16
  entries:IFDEntry[entryCount]
  nextIFDOffset:uint32
}

def IFDEntry = {
  tag:uint16
  type:uint16
  count:uint32
  valueOffset:uint32
}

def ImageFileHeader = {
  version:uint16
  imageWidth:uint32
  imageHeight:uint32
  bitsPerSample:uint16
  compression:uint16 = 1
  photoMetric:uint16
  stripOffsets:uint32
  rowsPerStrip:uint32
  stripByteCounts:uint32
  xResolution:uint32
  yResolution:uint32
  resolutionUnit:uint16
}

def uint16 = !uint16_le
def uint32 = !uint32_le