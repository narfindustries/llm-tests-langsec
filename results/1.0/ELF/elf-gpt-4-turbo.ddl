module Elf {
  enum EI_CLASS { ELFCLASSNONE = 0, ELFCLASS32 = 1, ELFCLASS64 = 2 }
  enum EI_DATA { ELFDATANONE = 0, ELFDATA2LSB = 1, ELFDATA2MSB = 2 }
  enum E_TYPE { ET_NONE = 0, ET_REL = 1, ET_EXEC = 2, ET_DYN = 3, ET_CORE = 4, ET_LOOS = 0xfe00, ET_HIOS = 0xfeff, ET_LOPROC = 0xff00, ET_HIPROC = 0xffff }
  enum E_MACHINE { EM_NONE = 0, EM_386 = 3, EM_X86_64 = 62 }
  enum SH_TYPE { SHT_NULL = 0, SHT_PROGBITS = 1, SHT_SYMTAB = 2, SHT_STRTAB = 3, SHT_RELA = 4, SHT_HASH = 5, SHT_DYNAMIC = 6, SHT_NOTE = 7, SHT_NOBITS = 8, SHT_REL = 9, SHT_SHLIB = 10, SHT_DYNSYM = 11, SHT_INIT_ARRAY = 14, SHT_FINI_ARRAY = 15, SHT_PREINIT_ARRAY = 16, SHT_GROUP = 17, SHT_SYMTAB_SHNDX = 18, SHT_NUM = 19, SHT_LOOS = 0x60000000, SHT_HIOS = 0x6fffffff, SHT_LOPROC = 0x70000000, SHT_HIPROC = 0x7fffffff, SHT_LOUSER = 0x80000000, SHT_HIUSER = 0xffffffff }
  enum P_TYPE { PT_NULL = 0, PT_LOAD = 1, PT_DYNAMIC = 2, PT_INTERP = 3, PT_NOTE = 4, PT_SHLIB = 5, PT_PHDR = 6, PT_TLS = 7, PT_NUM = 8, PT_LOOS = 0x60000000, PT_HIOS = 0x6fffffff, PT_LOPROC = 0x70000000, PT_HIPROC = 0x7fffffff }

  struct ELF_Ident {
    byte magic[4];    // 0x7F, 'E', 'L', 'F'
    EI_CLASS class;
    EI_DATA data;
    byte version;
    byte osabi;
    byte abiversion;
    byte pad[7];
  }

  struct ELF_Header {
    ELF_Ident ident;
    E_TYPE type;
    E_MACHINE machine;
    uint32 version;
    alt (class) {
      case ELFCLASS32: uint32 entry;
      case ELFCLASS64: uint64 entry;
    }
    alt (class) {
      case ELFCLASS32: uint32 phoff;
      case ELFCLASS64: uint64 phoff;
    }
    alt (class) {
      case ELFCLASS32: uint32 shoff;
      case ELFCLASS64: uint64 shoff;
    }
    uint32 flags;
    uint16 ehsize;
    uint16 phentsize;
    uint16 phnum;
    uint16 shentsize;
    uint16 shnum;
    uint16 shstrndx;
  }

  struct Section_Header {
    uint32 name;
    SH_TYPE type;
    alt (class) {
      case ELFCLASS32: uint32 flags;
      case ELFCLASS64: uint64 flags;
    }
    alt (class) {
      case ELFCLASS32: uint32 addr;
      case ELFCLASS64: uint64 addr;
    }
    alt (class) {
      case ELFCLASS32: uint32 offset;
      case ELFCLASS64: uint64 offset;
    }
    alt (class) {
      case ELFCLASS32: uint32 size;
      case ELFCLASS64: uint64 size;
    }
    uint32 link;
    uint32 info;
    alt (class) {
      case ELFCLASS32: uint32 addralign;
      case ELFCLASS64: uint64 addralign;
    }
    alt (class) {
      case ELFCLASS32: uint32 entsize;
      case ELFCLASS64: uint64 entsize;
    }
  }

  struct Program_Header {
    P_TYPE type;
    alt (class) {
      case ELFCLASS32: uint32 flags;
      case ELFCLASS64: uint32 flags; // Note: might also be uint64 in some specifications
    }
    alt (class) {
      case ELFCLASS32: uint32 offset;
      case ELFCLASS64: uint64 offset;
    }
    alt (class) {
      case ELFCLASS32: uint32 vaddr;
      case ELFCLASS64: uint64 vaddr;
    }
    alt (class) {
      case ELFCLASS32: uint32 paddr;
      case ELFCLASS64: uint64 paddr;
    }
    alt (class) {
      case ELFCLASS32: uint32 filesz;
      case ELFCLASS64: uint64 filesz;
    }
    alt (class) {
      case ELFCLASS32: uint32 memsz;
      case ELFCLASS64: uint64 memsz;
    }
    alt (class) {
      case ELFCLASS32: uint32 align;
      case ELFCLASS64: uint64 align;
    }
  }

  ELF_Header header;
  array<Program_Header> program_headers[header.phnum] @ header.phoff;
  array<Section_Header> section_headers[header.shnum] @ header.shoff;
}