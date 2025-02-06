def EI_MAG0 = 0x7f
def EI_MAG1 = 0x45
def EI_MAG2 = 0x4c
def EI_MAG3 = 0x46

def ELFCLASSNONE = 0
def ELFCLASS32   = 1
def ELFCLASS64   = 2

def ELFDATANONE  = 0
def ELFDATA2LSB  = 1
def ELFDATA2MSB  = 2

def EV_NONE    = 0
def EV_CURRENT = 1

def ET_NONE = 0
def ET_REL  = 1
def ET_EXEC = 2
def ET_DYN  = 3
def ET_CORE = 4

def EM_NONE    = 0
def EM_M32     = 1
def EM_SPARC   = 2
def EM_386     = 3
def EM_68K     = 4
def EM_88K     = 5
def EM_860     = 7
def EM_MIPS    = 8
def EM_X86_64  = 62

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

def SHF_WRITE     = 0x1
def SHF_ALLOC     = 0x2
def SHF_EXECINSTR = 0x4

def STB_LOCAL  = 0
def STB_GLOBAL = 1
def STB_WEAK   = 2

def STT_NOTYPE   = 0
def STT_OBJECT   = 1
def STT_FUNC     = 2
def STT_SECTION  = 3
def STT_FILE     = 4

def EI_NIDENT = 16

typedef Elf64_Ehdr = {
  e_ident     : uint8[EI_NIDENT],
  e_type      : uint16,
  e_machine   : uint16,
  e_version   : uint32,
  e_entry     : uint64,
  e_phoff     : uint64,
  e_shoff     : uint64,
  e_flags     : uint32,
  e_ehsize    : uint16,
  e_phentsize : uint16,
  e_phnum     : uint16,
  e_shentsize : uint16,
  e_shnum     : uint16,
  e_shstrndx  : uint16
}

typedef Elf64_Phdr = {
  p_type   : uint32,
  p_flags  : uint32,
  p_offset : uint64,
  p_vaddr  : uint64,
  p_paddr  : uint64,
  p_filesz : uint64,
  p_memsz  : uint64,
  p_align  : uint64
}

typedef Elf64_Shdr = {
  sh_name      : uint32,
  sh_type      : uint32,
  sh_flags     : uint64,
  sh_addr      : uint64,
  sh_offset    : uint64,
  sh_size      : uint64,
  sh_link      : uint32,
  sh_info      : uint32,
  sh_addralign : uint64,
  sh_entsize   : uint64
}

typedef Elf64_Sym = {
  st_name  : uint32,
  st_info  : uint8,
  st_other : uint8,
  st_shndx : uint16,
  st_value : uint64,
  st_size  : uint64
}

typedef Elf64_Rel = {
  r_offset : uint64,
  r_info   : uint64
}

typedef Elf64_Rela = {
  r_offset : uint64,
  r_info   : uint64,
  r_addend : int64
}

typedef Elf64_Dyn = {
  d_tag : int64,
  d_val : uint64
}

typedef ELF64 = {
  header : Elf64_Ehdr,
  Assert(header.e_ident[0] == EI_MAG0),
  Assert(header.e_ident[1] == EI_MAG1),
  Assert(header.e_ident[2] == EI_MAG2),
  Assert(header.e_ident[3] == EI_MAG3),
  Assert(header.e_ident[4] == ELFCLASS64),
  
  if header.e_phnum > 0 {
    Seek(header.e_phoff),
    program_headers : Elf64_Phdr[header.e_phnum]
  },
  
  if header.e_shnum > 0 {
    Seek(header.e_shoff),
    section_headers : Elf64_Shdr[header.e_shnum]
  }
}

def Main = ELF64