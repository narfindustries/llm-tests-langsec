domain elf {
  type u8 = uint 8;
  type u16 = uint 16;
  type u32 = uint 32;
  type u64 = uint 64;
  type s8 = int 8;
  type s16 = int 16;
  type s32 = int 32;
  type s64 = int 64;

  type ElfIdent = struct {
    magic: u8[4];
    class: u8;
    data: u8;
    version: u8;
    osabi: u8;
    abiversion: u8;
    pad: u8[7];
  };

  type ElfHeader = struct {
    ident: ElfIdent;
    type: u16;
    machine: u16;
    version: u32;
    entry: u64;
    phoff: u64;
    shoff: u64;
    flags: u32;
    ehsize: u16;
    phentsize: u16;
    phnum: u16;
    shentsize: u16;
    shnum: u16;
    shstrndx: u16;
  };

  type SectionHeader = struct {
    name: u32;
    type: u32;
    flags: u64;
    addr: u64;
    offset: u64;
    size: u64;
    link: u32;
    info: u32;
    addralign: u64;
    entsize: u64;
  };

  type ProgramHeader = struct {
    type: u32;
    flags: u32;
    offset: u64;
    vaddr: u64;
    paddr: u64;
    filesz: u64;
    memsz: u64;
    align: u64;
  };

  syntax = {
    start: ElfHeader;
    ElfHeader: {
      phoff: { align: 8 },
      phdr: [ProgramHeader] @phnum;
      shoff: { align: 8 },
      shdr: [SectionHeader] @shnum;
    };
  };

  layout = {
    ElfHeader: {
      ident: { size: 16 },
      type: { size: 2 },
      machine: { size: 2 },
      version: { size: 4 },
      entry: { size: 8 },
      phoff: { size: 8 },
      shoff: { size: 8 },
      flags: { size: 4 },
      ehsize: { size: 2 },
      phentsize: { size: 2 },
      phnum: { size: 2 },
      shentsize: { size: 2 },
      shnum: { size: 2 },
      shstrndx: { size: 2 },
    };
    ProgramHeader: {
      type: { size: 4 },
      flags: { size: 4 },
      offset: { size: 8 },
      vaddr: { size: 8 },
      paddr: { size: 8 },
      filesz: { size: 8 },
      memsz: { size: 8 },
      align: { size: 8 },
    };
    SectionHeader: {
      name: { size: 4 },
      type: { size: 4 },
      flags: { size: 8 },
      addr: { size: 8 },
      offset: { size: 8 },
      size: { size: 8 },
      link: { size: 4 },
      info: { size: 4 },
      addralign: { size: 8 },
      entsize: { size: 8 },
    };
  };
}