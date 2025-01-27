def ZIP
  = { magic : Magic
    , version : Version
    , flags : Flags
    , compression : CompressionMethod 
    , last_mod_time : uint16
    , last_mod_date : uint16
    , crc32 : uint32
    , compressed_size : uint32
    , uncompressed_size : uint32
    , filename_length : uint16
    , extra_field_length : uint16
    , filename : $byte * |filename_length as uint64|
    , extra_field : $byte * |extra_field_length as uint64|
    , compressed_data : $byte * |compressed_size as uint64|
    }

def Magic = 0x504B0304 -- 'PK\3\4'

def Version = uint16

def Flags = uint16

def CompressionMethod = uint16

@LittleEndian
@TopLevel
def ZIPFile = ZIP