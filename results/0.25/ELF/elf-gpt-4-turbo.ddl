module ELF {

  import DAEDALUS::BitManip;
  import DAEDALUS::Utils;

  type U32 = Bits32;
  type U16 = Bits16;
  type U8  = Bits8;

  type ElfHeader = struct {
    magic       : U32;
    class       : U8;
    data        : U8;
    version     : U8;
    osABI       : U8;
    abiVersion  : U8;
    pad         : U8[7];
    eType       : U16;
    machine     : U16;
    version2    : U32;
    entry       : U32;
    phOff       : U32;
    shOff       : U32;
    flags       : U32;
    ehSize      : U16;
    phEntSize   : U16;
    phNum       : U16;
    shEntSize   : U16;
    shNum       : U16;
    shStrNdx    : U16;
  };

  type ProgramHeader = struct {
    pType   : U32;
    offset  : U32;
    vAddr   : U32;
    pAddr   : U32;
    fileSize: U32;
    memSize : U32;
    flags   : U32;
    align   : U32;
  };

  type SectionHeader = struct {
    name      : U32;
    sType     : U32;
    flags     : U32;
    addr      : U32;
    offset    : U32;
    size      : U32;
    link      : U32;
    info      : U32;
    addrAlign : U32;
    entSize   : U32;
  };

  type ElfFile = struct {
    header        : ElfHeader;
    programHeader : ProgramHeader[header.phNum];
    sectionHeader : SectionHeader[header.shNum];
  };

  let elfParser = parse ElfFile from BitStream;

}