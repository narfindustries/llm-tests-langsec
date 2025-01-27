def Main = JPEG

def JPEG = {
  Marker SOI;
  segments*;
  Marker EOI
}

def segments = Select {
  APP0
  APP1 
  DQT
  SOF0
  DHT
  SOS
}

def Marker = {
  @h = $FF;
  @m = $00..$FF
}

def APP0 = {
  Marker where @m == $E0;
  @len = Size16;
  @id = Bytes 5 match "JFIF\0";
  @ver_maj = UInt8;
  @ver_min = UInt8;
  @units = UInt8;
  @xdensity = Size16;
  @ydensity = Size16;
  @xthumb = UInt8;
  @ythumb = UInt8;
  @thumb = Bytes (@xthumb * @ythumb * 3)
}

def APP1 = {
  Marker where @m == $E1;
  @len = Size16;
  @data = Bytes (@len - 2)
}

def DQT = {
  Marker where @m == $DB;
  @len = Size16;
  qtables[@len - 2]
}

def qtables = {
  @prec_id = UInt8;
  @table = Bytes 64
}

def SOF0 = {
  Marker where @m == $C0;
  @len = Size16;
  @precision = UInt8;
  @height = Size16;
  @width = Size16;
  @ncomps = UInt8;
  components[@ncomps]
}

def components = {
  @id = UInt8;
  @samp = UInt8;
  @qtable = UInt8
}

def DHT = {
  Marker where @m == $C4;
  @len = Size16;
  htables[(@len - 2) / 17]
}

def htables = {
  @class_id = UInt8;
  @counts = Bytes 16;
  @symbols = Bytes (Sum @counts)
}

def SOS = {
  Marker where @m == $DA;
  @len = Size16;
  @ncomps = UInt8;
  scanComps[@ncomps];
  @specstart = UInt8;
  @specend = UInt8;
  @approx = UInt8;
  scanData
}

def scanComps = {
  @id = UInt8;
  @tables = UInt8
}

def scanData = Bytes (Remaining)

def Size16 = UInt16BE
def UInt8 = !uint 8

def Sum xs = case xs of {
  [] -> 0;
  x:xs -> x + (Sum xs)
}