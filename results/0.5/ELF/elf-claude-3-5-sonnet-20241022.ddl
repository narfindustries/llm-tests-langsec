def Magic = {0x7f:8, 'E':8, 'L':8, 'F':8}

def Class = {
  ELFCLASSNONE = 0,
  ELFCLASS32   = 1,
  ELFCLASS64   = 2
}

def Data = {
  ELFDATANONE = 0,
  ELFDATA2LSB = 1,
  ELFDATA2MSB = 2
}

def Version = {
  EV_NONE    = 0,
  EV_CURRENT = 1
}

def OSABI = {
  ELFOSABI_SYSV     = 0,
  ELFOSABI_HPUX     = 1,
  ELFOSABI_NETBSD   = 2,
  ELFOSABI_LINUX    = 3,
  ELFOSABI_SOLARIS  = 6,
  ELFOSABI_AIX      = 7,
  ELFOSABI_IRIX     = 8,
  ELFOSABI_FREEBSD  = 9,
  ELFOSABI_TRU64    = 10,
  ELFOSABI_MODESTO  = 11,
  ELFOSABI_OPENBSD  = 12,
  ELFOSABI_ARM      = 97,
  ELFOSABI_STANDALONE = 255
}

def Type = {
  ET_NONE   = 0,
  ET_REL    = 1,
  ET_EXEC   = 2,
  ET_DYN    = 3,
  ET_CORE   = 4,
  ET_LOOS   = 0xfe00,
  ET_HIOS   = 0xfeff,
  ET_LOPROC = 0xff00,
  ET_HIPROC = 0xffff
}

def Machine = {
  EM_NONE        = 0,
  EM_M32         = 1,
  EM_SPARC       = 2,
  EM_386         = 3,
  EM_68K         = 4,
  EM_88K         = 5,
  EM_860         = 7,
  EM_MIPS        = 8,
  EM_X86_64      = 62
}

def Ident = {
  magic     : Magic,
  class     : Class,
  data      : Data,
  version   : Version,
  osabi     : OSABI,
  abiversion: uint8,
  pad       : uint8[7]
}

def Header = {
  ident     : Ident,
  type      : Type,
  machine   : Machine,
  version   : Version,
  entry     : uint64,
  phoff     : uint64,
  shoff     : uint64,
  flags     : uint32,
  ehsize    : uint16,
  phentsize : uint16,
  phnum     : uint16,
  shentsize : uint16,
  shnum     : uint16,
  shstrndx  : uint16
}

def PType = {
  PT_NULL    = 0,
  PT_LOAD    = 1,
  PT_DYNAMIC = 2,
  PT_INTERP  = 3,
  PT_NOTE    = 4,
  PT_SHLIB   = 5,
  PT_PHDR    = 6,
  PT_TLS     = 7,
  PT_LOOS    = 0x60000000,
  PT_HIOS    = 0x6fffffff,
  PT_LOPROC  = 0x70000000,
  PT_HIPROC  = 0x7fffffff
}

def PFlags = {
  PF_X        = 0x1,
  PF_W        = 0x2,
  PF_R        = 0x4,
  PF_MASKOS   = 0x0ff00000,
  PF_MASKPROC = 0xf0000000
}

def ProgramHeader = {
  type   : PType,
  flags  : PFlags,
  offset : uint64,
  vaddr  : uint64,
  paddr  : uint64,
  filesz : uint64,
  memsz  : uint64,
  align  : uint64
}

def SType = {
  SHT_NULL          = 0,
  SHT_PROGBITS      = 1,
  SHT_SYMTAB        = 2,
  SHT_STRTAB        = 3,
  SHT_RELA          = 4,
  SHT_HASH          = 5,
  SHT_DYNAMIC       = 6,
  SHT_NOTE          = 7,
  SHT_NOBITS        = 8,
  SHT_REL           = 9,
  SHT_SHLIB         = 10,
  SHT_DYNSYM        = 11,
  SHT_INIT_ARRAY    = 14,
  SHT_FINI_ARRAY    = 15,
  SHT_PREINIT_ARRAY = 16,
  SHT_GROUP         = 17,
  SHT_SYMTAB_SHNDX  = 18
}

def SFlags = {
  SHF_WRITE            = 0x1,
  SHF_ALLOC            = 0x2,
  SHF_EXECINSTR        = 0x4,
  SHF_MERGE            = 0x10,
  SHF_STRINGS          = 0x20,
  SHF_INFO_LINK        = 0x40,
  SHF_LINK_ORDER       = 0x80,
  SHF_OS_NONCONFORMING = 0x100,
  SHF_GROUP            = 0x200,
  SHF_TLS              = 0x400,
  SHF_MASKOS          = 0x0ff00000,
  SHF_MASKPROC        = 0xf0000000
}

def SectionHeader = {
  name      : uint32,
  type      : SType,
  flags     : SFlags,
  addr      : uint64,
  offset    : uint64,
  size      : uint64,
  link      : uint32,
  info      : uint32,
  addralign : uint64,
  entsize   : uint64
}

def SymbolBinding = {
  STB_LOCAL  = 0,
  STB_GLOBAL = 1,
  STB_WEAK   = 2
}

def SymbolType = {
  STT_NOTYPE  = 0,
  STT_OBJECT  = 1,
  STT_FUNC    = 2,
  STT_SECTION = 3,
  STT_FILE    = 4,
  STT_COMMON  = 5,
  STT_TLS     = 6
}

def SymbolVisibility = {
  STV_DEFAULT   = 0,
  STV_INTERNAL  = 1,
  STV_HIDDEN    = 2,
  STV_PROTECTED = 3
}

def Symbol = {
  name  : uint32,
  info  : uint8,
  other : SymbolVisibility,
  shndx : uint16,
  value : uint64,
  size  : uint64
}

def RelocationWithAddend = {
  offset : uint64,
  info   : uint64,
  addend : int64
}

def Relocation = {
  offset : uint64,
  info   : uint64
}

def ELF = {
  header   : Header,
  segments : ProgramHeader[header.phnum],
  sections : SectionHeader[header.shnum]
}