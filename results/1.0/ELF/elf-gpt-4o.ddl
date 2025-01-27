# Daedalus specification for ELF file format

struct ELFFile {
    magic: MagicNumber,           # 4-byte magic number "\x7FELF"
    file_class: ELFClass,         # 1-byte file class
    encoding: ELFData,            # 1-byte data encoding
    version: Version,             # 1-byte version
    os_abi: OSABI,                # 1-byte OS ABI
    abi_version: u8,              # 1-byte ABI version
    padding: Padding,             # 7-byte padding
    file_type: FileType,          # 2-byte file type
    machine: Machine,             # 2-byte machine type
    e_version: ElfVersion,        # 4-byte ELF version
    entry: u32,                   # 4-byte entry point
    ph_offset: u32,               # 4-byte program header offset
    sh_offset: u32,               # 4-byte section header offset
    flags: u32,                   # 4-byte processor-specific flags
    eh_size: u16,                 # 2-byte ELF header size
    ph_entry_size: u16,           # 2-byte program header entry size
    ph_num: u16,                  # 2-byte program header entry count
    sh_entry_size: u16,           # 2-byte section header entry size
    sh_num: u16,                  # 2-byte section header entry count
    sh_str_index: u16,            # 2-byte section header string table index
}

enum 1 ELFClass {
    NONE = 0,    # Invalid class
    CLASS32 = 1, # 32-bit objects
    CLASS64 = 2  # 64-bit objects
}

enum 1 ELFData {
    NONE = 0,    # Invalid data encoding
    LSB = 1,     # Little-endian
    MSB = 2      # Big-endian
}

enum 1 Version {
    NONE = 0,    # Invalid version
    CURRENT = 1  # Current version
}

enum 1 OSABI {
    SYSTEM_V = 0,       # UNIX System V ABI
    HPUX = 1,           # HP-UX
    NETBSD = 2,         # NetBSD
    LINUX = 3,          # Linux
    SOLARIS = 6,        # Solaris
    AIX = 7,            # AIX
    IRIX = 8,           # IRIX
    FREEBSD = 9,        # FreeBSD
    TRU64 = 10,         # TRU64 UNIX
    MODESTO = 11,       # Novell Modesto
    OPENBSD = 12,       # OpenBSD
    OPENVMS = 13,       # OpenVMS
    NSK = 14,           # NonStop Kernel
    AROS = 15,          # AROS
    FENIX_OS = 16,      # Fenix OS
    CLOUD_ABI = 17,     # Nuxi CloudABI
    SORTIX = 18         # Sortix
}

enum 2 FileType {
    NONE = 0,           # No file type
    REL = 1,            # Relocatable file
    EXEC = 2,           # Executable file
    DYN = 3,            # Shared object file
    CORE = 4            # Core file
}

enum 2 Machine {
    NONE = 0,           # No machine
    M32 = 1,            # AT&T WE 32100
    SPARC = 2,          # SPARC
    X86 = 3,            # Intel 80386
    MIPS = 8,           # MIPS RS3000
    POWERPC = 0x14,     # PowerPC
    X86_64 = 0x3E,      # AMD x86-64
    ARM = 0x28,         # ARM
    IA_64 = 0x32,       # Intel IA-64
    RISCV = 0xF3        # RISC-V
}

enum 4 ElfVersion {
    NONE = 0,           # Invalid version
    CURRENT = 1         # Current version
}

struct MagicNumber {
    value: Bytes<4> = [0x7F, 0x45, 0x4C, 0x46]  # "\x7FELF" in ASCII
}

struct Padding {
    bytes: Bytes<7>
}