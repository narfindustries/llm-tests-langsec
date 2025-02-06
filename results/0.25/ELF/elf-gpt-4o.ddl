enum EI_CLASS : u8 {
    ELFCLASSNONE = 0,
    ELFCLASS32 = 1,
    ELFCLASS64 = 2
};

enum EI_DATA : u8 {
    ELFDATANONE = 0,
    ELFDATA2LSB = 1,
    ELFDATA2MSB = 2
};

enum EI_OSABI : u8 {
    ELFOSABI_SYSV = 0,
    ELFOSABI_HPUX = 1,
    ELFOSABI_NETBSD = 2,
    ELFOSABI_LINUX = 3,
    ELFOSABI_SOLARIS = 6,
    ELFOSABI_AIX = 7,
    ELFOSABI_IRIX = 8,
    ELFOSABI_FREEBSD = 9,
    ELFOSABI_TRU64 = 10,
    ELFOSABI_MODESTO = 11,
    ELFOSABI_OPENBSD = 12,
    ELFOSABI_OPENVMS = 13,
    ELFOSABI_NSK = 14,
    ELFOSABI_AROS = 15,
    ELFOSABI_FENIXOS = 16,
    ELFOSABI_CLOUDABI = 17,
    ELFOSABI_OPENVOS = 18
};

enum e_type : u16 {
    ET_NONE = 0,
    ET_REL = 1,
    ET_EXEC = 2,
    ET_DYN = 3,
    ET_CORE = 4
};

enum e_machine : u16 {
    EM_NONE = 0,
    EM_M32 = 1,
    EM_SPARC = 2,
    EM_386 = 3,
    EM_68K = 4,
    EM_88K = 5,
    EM_IAMCU = 6,
    EM_860 = 7,
    EM_MIPS = 8,
    EM_S370 = 9,
    EM_MIPS_RS3_LE = 10,
    EM_PARISC = 15,
    EM_VPP500 = 17,
    EM_SPARC32PLUS = 18,
    EM_960 = 19,
    EM_PPC = 20,
    EM_PPC64 = 21,
    EM_S390 = 22,
    EM_SPU = 23,
    EM_V800 = 36,
    EM_FR20 = 37,
    EM_RH32 = 38,
    EM_RCE = 39,
    EM_ARM = 40,
    EM_ALPHA = 41,
    EM_SH = 42,
    EM_SPARCV9 = 43,
    EM_TRICORE = 44,
    EM_ARC = 45,
    EM_H8_300 = 46,
    EM_H8_300H = 47,
    EM_H8S = 48,
    EM_H8_500 = 49,
    EM_IA_64 = 50,
    EM_MIPS_X = 51,
    EM_COLDFIRE = 52,
    EM_68HC12 = 53,
    EM_MMA = 54,
    EM_PCP = 55,
    EM_NCPU = 56,
    EM_NDR1 = 57,
    EM_STARCORE = 58,
    EM_ME16 = 59,
    EM_ST100 = 60,
    EM_TINYJ = 61,
    EM_X86_64 = 62,
    EM_PDSP = 63,
    EM_PDP10 = 64,
    EM_PDP11 = 65,
    EM_FX66 = 66,
    EM_ST9PLUS = 67,
    EM_ST7 = 68,
    EM_68HC16 = 69,
    EM_68HC11 = 70,
    EM_68HC08 = 71,
    EM_68HC05 = 72,
    EM_SVX = 73,
    EM_ST19 = 74,
    EM_VAX = 75,
    EM_CRIS = 76,
    EM_JAVELIN = 77,
    EM_FIREPATH = 78,
    EM_ZSP = 79,
    EM_MMIX = 80,
    EM_HUANY = 81,
    EM_PRISM = 82,
    EM_AVR = 83,
    EM_FR30 = 84,
    EM_D10V = 85,
    EM_D30V = 86,
    EM_V850 = 87,
    EM_M32R = 88,
    EM_MN10300 = 89,
    EM_MN10200 = 90,
    EM_PJ = 91,
    EM_OPENRISC = 92,
    EM_ARC_COMPACT = 93,
    EM_XTENSA = 94,
    EM_VIDEOCORE = 95,
    EM_TMM_GPP = 96,
    EM_NS32K = 97,
    EM_TPC = 98,
    EM_SNP1K = 99,
    EM_ST200 = 100,
    EM_IP2K = 101,
    EM_MAX = 102,
    EM_CR = 103,
    EM_F2MC16 = 104,
    EM_MSP430 = 105,
    EM_BLACKFIN = 106,
    EM_SE_C33 = 107,
    EM_SEP = 108,
    EM_ARCA = 109,
    EM_UNICORE = 110,
    EM_EXCESS = 111,
    EM_DXP = 112,
    EM_ALTERA_NIOS2 = 113,
    EM_CRX = 114,
    EM_XGATE = 115,
    EM_C166 = 116,
    EM_M16C = 117,
    EM_DSPIC30F = 118,
    EM_CE = 119,
    EM_M32C = 120,
    EM_TSK3000 = 131,
    EM_RS08 = 132,
    EM_SHARC = 133,
    EM_ECOG2 = 134,
    EM_SCORE7 = 135,
    EM_DSP24 = 136,
    EM_VIDEOCORE3 = 137,
    EM_LATTICEMICO32 = 138,
    EM_SE_C17 = 139,
    EM_TI_C6000 = 140,
    EM_TI_C2000 = 141,
    EM_TI_C5500 = 142,
    EM_TI_ARP32 = 143,
    EM_TI_PRU = 144,
    EM_MMDSP_PLUS = 160,
    EM_CYPRESS_M8C = 161,
    EM_R32C = 162,
    EM_TRIMEDIA = 163,
    EM_QDSP6 = 164,
    EM_8051 = 165,
    EM_STXP7X = 166,
    EM_NDS32 = 167,
    EM_ECOG1 = 168,
    EM_ECOG1X = 168,
    EM_MAXQ30 = 169,
    EM_XIMO16 = 170,
    EM_MANIK = 171,
    EM_CRAYNV2 = 172,
    EM_RX = 173,
    EM_METAG = 174,
    EM_MCST_ELBRUS = 175,
    EM_ECOG16 = 176,
    EM_CR16 = 177,
    EM_ETPU = 178,
    EM_SLE9X = 179,
    EM_L10M = 180,
    EM_K10M = 181,
    EM_AARCH64 = 183,
    EM_AVR32 = 185,
    EM_STM8 = 186,
    EM_TILE64 = 187,
    EM_TILEPRO = 188,
    EM_MICROBLAZE = 189,
    EM_CUDA = 190,
    EM_TILEGX = 191,
    EM_CLOUDSHIELD = 192,
    EM_COREA_1ST = 193,
    EM_COREA_2ND = 194,
    EM_ARC_COMPACT2 = 195,
    EM_OPEN8 = 196,
    EM_RL78 = 197,
    EM_VIDEOCORE5 = 198,
    EM_78KOR = 199,
    EM_56800EX = 200,
    EM_BA1 = 201,
    EM_BA2 = 202,
    EM_XCORE = 203,
    EM_MCHP_PIC = 204,
    EM_INTEL205 = 205,
    EM_INTEL206 = 206,
    EM_INTEL207 = 207,
    EM_INTEL208 = 208,
    EM_INTEL209 = 209,
    EM_KM32 = 210,
    EM_KMX32 = 211,
    EM_KMX16 = 212,
    EM_KMX8 = 213,
    EM_KVARC = 214,
    EM_CDP = 215,
    EM_COGE = 216,
    EM_COOL = 217,
    EM_NORC = 218,
    EM_CSR_KALIMBA = 219,
    EM_Z80 = 220,
    EM_VISIUM = 221,
    EM_FT32 = 222,
    EM_MOXIE = 223,
    EM_AMDGPU = 224,
    EM_RISCV = 243,
    EM_BPF = 247,
    EM_CSKY = 252
};

enum p_type : u32 {
    PT_NULL = 0,
    PT_LOAD = 1,
    PT_DYNAMIC = 2,
    PT_INTERP = 3,
    PT_NOTE = 4,
    PT_SHLIB = 5,
    PT_PHDR = 6,
    PT_TLS = 7,
    PT_NUM = 8,
    PT_LOOS = 0x60000000,
    PT_GNU_EH_FRAME = 0x6474e550,
    PT_GNU_STACK = 0x6474e551,
    PT_GNU_RELRO = 0x6474e552,
    PT_LOSUNW = 0x6ffffffa,
    PT_SUNWBSS = 0x6ffffffa,
    PT_SUNWSTACK = 0x6ffffffb,
    PT_HISUNW = 0x6fffffff,
    PT_HIOS = 0x6fffffff,
    PT_LOPROC = 0x70000000,
    PT_HIPROC = 0x7fffffff
};

enum sh_type : u32 {
    SHT_NULL = 0,
    SHT_PROGBITS = 1,
    SHT_SYMTAB = 2,
    SHT_STRTAB = 3,
    SHT_RELA = 4,
    SHT_HASH = 5,
    SHT_DYNAMIC = 6,
    SHT_NOTE = 7,
    SHT_NOBITS = 8,
    SHT_REL = 9,
    SHT_SHLIB = 10,
    SHT_DYNSYM = 11,
    SHT_INIT_ARRAY = 14,
    SHT_FINI_ARRAY = 15,
    SHT_PREINIT_ARRAY = 16,
    SHT_GROUP = 17,
    SHT_SYMTAB_SHNDX = 18,
    SHT_NUM = 19,
    SHT_LOOS = 0x60000000,
    SHT_GNU_ATTRIBUTES = 0x6ffffff5,
    SHT_GNU_HASH = 0x6ffffff6,
    SHT_GNU_LIBLIST = 0x6ffffff7,
    SHT_CHECKSUM = 0x6ffffff8,
    SHT_LOSUNW = 0x6ffffffa,
    SHT_SUNW_move = 0x6ffffffa,
    SHT_SUNW_COMDAT = 0x6ffffffb,
    SHT_SUNW_syminfo = 0x6ffffffc,
    SHT_GNU_verdef = 0x6ffffffd,
    SHT_GNU_verneed = 0x6ffffffe,
    SHT_GNU_versym = 0x6fffffff,
    SHT_HISUNW = 0x6fffffff,
    SHT_HIOS = 0x6fffffff,
    SHT_LOPROC = 0x70000000,
    SHT_HIPROC = 0x7fffffff,
    SHT_LOUSER = 0x80000000,
    SHT_HIUSER = 0x8fffffff
};

enum st_bind : u8 {
    STB_LOCAL = 0,
    STB_GLOBAL = 1,
    STB_WEAK = 2,
    STB_NUM = 3,
    STB_LOOS = 10,
    STB_HIOS = 12,
    STB_LOPROC = 13,
    STB_HIPROC = 15
};

enum st_type : u8 {
    STT_NOTYPE = 0,
    STT_OBJECT = 1,
    STT_FUNC = 2,
    STT_SECTION = 3,
    STT_FILE = 4,
    STT_COMMON = 5,
    STT_TLS = 6,
    STT_NUM = 7,
    STT_LOOS = 10,
    STT_GNU_IFUNC = 10,
    STT_HIOS = 12,
    STT_LOPROC = 13,
    STT_HIPROC = 15
};

enum st_visibility : u8 {
    STV_DEFAULT = 0,
    STV_INTERNAL = 1,
    STV_HIDDEN = 2,
    STV_PROTECTED = 3
};

struct Elf32_Ehdr {
    u8[4] e_ident_magic;
    EI_CLASS e_ident_class;
    EI_DATA e_ident_data;
    u8 e_ident_version;
    EI_OSABI e_ident_osabi;
    u8 e_ident_abiversion;
    u8[7] e_ident_pad;
    e_type e_type;
    e_machine e_machine;
    u32 e_version;
    u32 e_entry;
    u32 e_phoff;
    u32 e_shoff;
    u32 e_flags;
    u16 e_ehsize;
    u16 e_phentsize;
    u16 e_phnum;
    u16 e_shentsize;
    u16 e_shnum;
    u16 e_shstrndx;
};

struct Elf64_Ehdr {
    u8[4] e_ident_magic;
    EI_CLASS e_ident_class;
    EI_DATA e_ident_data;
    u8 e_ident_version;
    EI_OSABI e_ident_osabi;
    u8 e_ident_abiversion;
    u8[7] e_ident_pad;
    e_type e_type;
    e_machine e_machine;
    u32 e_version;
    u64 e_entry;
    u64 e_phoff;
    u64 e_shoff;
    u32 e_flags;
    u16 e_ehsize;
    u16 e_phentsize;
    u16 e_phnum;
    u16 e_shentsize;
    u16 e_shnum;
    u16 e_shstrndx;
};

struct Elf32_Phdr {
    p_type p_type;
    u32 p_offset;
    u32 p_vaddr;
    u32 p_paddr;
    u32 p_filesz;
    u32 p_memsz;
    u32 p_flags;
    u32 p_align;
};

struct Elf64_Phdr {
    p_type p_type;
    u32 p_flags;
    u64 p_offset;
    u64 p_vaddr;
    u64 p_paddr;
    u64 p_filesz;
    u64 p_memsz;
    u64 p_align;
};

struct Elf32_Shdr {
    u32 sh_name;
    sh_type sh_type;
    u32 sh_flags;
    u32 sh_addr;
    u32 sh_offset;
    u32 sh_size;
    u32 sh_link;
    u32 sh_info;
    u32 sh_addralign;
    u32 sh_entsize;
};

struct Elf64_Shdr {
    u32 sh_name;
    sh_type sh_type;
    u64 sh_flags;
    u64 sh_addr;
    u64 sh_offset;
    u64 sh_size;
    u32 sh_link;
    u32 sh_info;
    u64 sh_addralign;
    u64 sh_entsize;
};

struct Elf32_Sym {
    u32 st_name;
    u32 st_value;
    u32 st_size;
    u8 st_info;
    u8 st_other;
    u16 st_shndx;
};

struct Elf64_Sym {
    u32 st_name;
    u8 st_info;
    u8 st_other;
    u16 st_shndx;
    u64 st_value;
    u64 st_size;
};

struct ELF {
    union {
        Elf32_Ehdr elf32_header;
        Elf64_Ehdr elf64_header;
    };
    // Program headers and section headers would be parsed based on offsets and counts from the header
};