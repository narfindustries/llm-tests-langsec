module GZIP.GPT4Turbo {

  import DAEDALUS::Std::*;
  import DAEDALUS::Util::*;

  type GzipFile = struct {
    header : GzipHeader,
    data_blocks : [Block] using header.compression_method,
    footer : GzipFooter
  }

  type GzipHeader = struct {
    id1           : uint8  = 0x1f,
    id2           : uint8  = 0x8b,
    compression_method : uint8,
    flags         : uint8,
    mtime         : uint32,
    extra_flags   : uint8,
    os            : uint8,
    extras        : optional[Extras] if (flags & 0x04) != 0,
    original_name : optional[Cstring] if (flags & 0x08) != 0,
    comment       : optional[Cstring] if (flags & 0x10) != 0,
    crc16         : optional[uint16] if (flags & 0x02) != 0
  }

  type Extras = struct {
    xlen : uint16,
    data : [uint8] xlen
  }

  type Block = struct {
    data : [uint8] until endOfBlock(),
    isLastBlock : bool
  }

  type Cstring = struct {
    text : [char] until \0,
    terminator : skip 1
  }

  type GzipFooter = struct {
    crc32        : uint32,
    input_size   : uint32
  }

  native endOfBlock : bool; // Placeholder for the actual end-of-block logic implementation

}