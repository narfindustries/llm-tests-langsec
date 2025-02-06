def EI_MAG0     = 0x7f
def EI_MAG1     = 'E'
def EI_MAG2     = 'L'
def EI_MAG3     = 'F'
def EI_CLASS    = 4
def EI_DATA     = 5
def EI_VERSION  = 6
def EI_OSABI    = 7
def EI_ABIVERSION = 8
def EI_PAD      = 9
def EI_NIDENT   = 16

def ELFCLASSNONE = 0
def ELFCLASS32   = 1
def ELFCLASS64   = 2

def ELFDATANONE  = 0
def ELFDATA2LSB  = 1
def ELFDATA2MSB  = 2

def ET_NONE = 0
def ET_REL  = 1
def ET_EXEC = 2
def ET_DYN  = 3
def ET_CORE = 4

def SHT_NULL     = 0
def SHT_PROGBITS = 1
def SHT_SYMTAB   = 2
def SHT_STRTAB   = 3
def SHT_RELA     = 4
def SHT_HASH     = 5
def SHT_DYNAMIC  = 6
def SHT_NOTE     = 7
def SHT_NOBITS   = 8
def SHT_REL      = 9
def SHT_SHLIB    = 10
def SHT_DYNSYM   = 11

def PT_NULL    = 0
def PT_LOAD    = 1
def PT_DYNAMIC = 2
def PT_INTERP  = 3
def PT_NOTE    = 4
def PT_SHLIB   = 5
def PT_PHDR    = 6
def PT_TLS     = 7

def PF_X = 0x1
def PF_W = 0x2
def PF_R = 0x4

def SHF_WRITE      = 0x1
def SHF_ALLOC      = 0x2
def SHF_EXECINSTR  = 0x4
def SHF_MERGE      = 0x10
def SHF_STRINGS    = 0x20
def SHF_INFO_LINK  = 0x40

struct Ident {
  magic0 : uint8 where magic0 == EI_MAG0,
  magic1 : uint8 where magic1 == EI_MAG1,
  magic2 : uint8 where magic2 == EI_MAG2,
  magic3 : uint8 where magic3 == EI_MAG3,
  class : uint8,
  data : uint8,
  version : uint8,
  osabi : uint8,
  abiversion : uint8,
  pad : uint8[7]
}

struct Header32 {
  ident : Ident where ident.class == ELFCLASS32,
  type : uint16,
  machine : uint16,
  version : uint32,
  entry : uint32,
  phoff : uint32,
  shoff : uint32,
  flags : uint32,
  ehsize : uint16,
  phentsize : uint16,
  phnum : uint16,
  shentsize : uint16,
  shnum : uint16,
  shstrndx : uint16
}

struct Header64 {
  ident : Ident where ident.class == ELFCLASS64,
  type : uint16,
  machine : uint16,
  version : uint32,
  entry : uint64,
  phoff : uint64,
  shoff : uint64,
  flags : uint32,
  ehsize : uint16,
  phentsize : uint16,
  phnum : uint16,
  shentsize : uint16,
  shnum : uint16,
  shstrndx : uint16
}

struct ProgramHeader32 {
  type : uint32,
  offset : uint32,
  vaddr : uint32,
  paddr : uint32,
  filesz : uint32,
  memsz : uint32,
  flags : uint32,
  align : uint32
}

struct ProgramHeader64 {
  type : uint32,
  flags : uint32,
  offset : uint64,
  vaddr : uint64,
  paddr : uint64,
  filesz : uint64,
  memsz : uint64,
  align : uint64
}

struct SectionHeader32 {
  name : uint32,
  type : uint32,
  flags : uint32,
  addr : uint32,
  offset : uint32,
  size : uint32,
  link : uint32,
  info : uint32,
  addralign : uint32,
  entsize : uint32
}

struct SectionHeader64 {
  name : uint32,
  type : uint32,
  flags : uint64,
  addr : uint64,
  offset : uint64,
  size : uint64,
  link : uint32,
  info : uint32,
  addralign : uint64,
  entsize : uint64
}

struct ELF32 {
  header : Header32,
  program_headers : ProgramHeader32[header.phnum] @ header.phoff,
  section_headers : SectionHeader32[header.shnum] @ header.shoff
}

struct ELF64 {
  header : Header64,
  program_headers : ProgramHeader64[header.phnum] @ header.phoff,
  section_headers : SectionHeader64[header.shnum] @ header.shoff
}

union ELF {
  elf32 : ELF32,
  elf64 : ELF64
}

format ELF