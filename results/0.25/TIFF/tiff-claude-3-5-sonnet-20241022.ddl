def Main = {
  header;
  IFDs
}

def header = {
  $$ = "II";
  magic: uint16 = 42;
  offset: uint32
}

def IFDs = {
  repeat1 {
    count: uint16;
    entries: ImageFileEntry[count];
    nextIFD: uint32
  } until nextIFD == 0
}

def ImageFileEntry = {
  tag: uint16;
  type: uint16;
  count: uint32;
  valueOrOffset: uint32;
  
  -- Validate known tags and types
  @assert tag >= 254 && tag <= 34665;
  @assert type >= 1 && type <= 12;
  
  -- Validate count based on type
  switch type {
    case 1: @assert count <= 4294967295  -- BYTE 
    case 2: @assert count <= 4294967295  -- ASCII
    case 3: @assert count <= 2147483647  -- SHORT
    case 4: @assert count <= 1073741823  -- LONG
    case 5: @assert count <= 858993459   -- RATIONAL
    case 6: @assert count <= 4294967295  -- SBYTE
    case 7: @assert count <= 4294967295  -- UNDEFINED
    case 8: @assert count <= 2147483647  -- SSHORT
    case 9: @assert count <= 1073741823  -- SLONG
    case 10: @assert count <= 858993459  -- SRATIONAL
    case 11: @assert count <= 1073741823 -- FLOAT
    case 12: @assert count <= 536870911  -- DOUBLE
  }
}