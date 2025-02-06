use nom::{
    bits::complete::{tag, take},
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::{digit1, hex_digit1, oct_digit1, space0, space1},
    combinator::{map, map_res, opt, verify},
    error::{context, Error, ErrorKind, ParseError},
    multi::{count, many0, many1, separated_list0, separated_list1},
    number::complete::{be_f32, be_f64, be_i128, be_i16, be_i32, be_i64, be_i8, be_u128, be_u16, be_u32, be_u64, be_u8, le_f32, le_f64, le_i128, le_i16, le_i32, le_i64, le_i8, le_u128, le_u16, le_u32, le_u64, le_u8},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, stdin};
use std::path::Path;

#[derive(Debug, PartialEq)]
enum EI_CLASS {
    ELFCLASS32,
    ELFCLASS64,
}

#[derive(Debug, PartialEq)]
enum EI_DATA {
    ELFDATA2LSB,
    ELFDATA2MSB,
}

#[derive(Debug, PartialEq)]
enum EI_VERSION {
    EV_CURRENT,
}

#[derive(Debug, PartialEq)]
enum EI_OSABI {
    ELFOSABI_NONE,
    ELFOSABI_SYSV,
    ELFOSABI_HPUX,
    ELFOSABI_NETBSD,
    ELFOSABI_LINUX,
    ELFOSABI_SOLARIS,
    ELFOSABI_IRIX,
    ELFOSABI_FREEBSD,
    ELFOSABI_TRU64,
    ELFOSABI_MODESTO,
    ELFOSABI_OPENBSD,
}

#[derive(Debug, PartialEq)]
enum ET {
    ET_NONE,
    ET_REL,
    ET_EXEC,
    ET_DYN,
    ET_CORE,
}

#[derive(Debug, PartialEq)]
enum EM {
    EM_NONE,
    EM_M32,
    EM_SPARC,
    EM_386,
    EM_68K,
    EM_88K,
    EM_860,
    EM_MIPS,
    EM_S370,
    EM_MIPS_RS3_LE,
    EM_PARISC,
    EM_VPP500,
    EM_SPARC32PLUS,
    EM_960,
    EM_PPC,
    EM_PPC64,
    EM_S390,
    EM_SPU,
    EM_V800,
    EM_FR20,
    EM_RH32,
    EM_RCE,
    EM_ARM,
    EM_ALPHA,
    EM_SH,
    EM_SPARCV9,
    EM_TRICORE,
    EM_ARC,
    EM_X86_64,
    EM_AVR32,
    EM_FROB,
}

#[derive(Debug, PartialEq)]
enum EV {
    EV_NONE,
    EV_CURRENT,
}

#[derive(Debug, PartialEq)]
enum PT {
    PT_NULL,
    PT_LOAD,
    PT_DYNAMIC,
    PT_INTERP,
    PT_NOTE,
    PT_SHLIB,
    PT_PHDR,
    PT_TLS,
    PT_LOPROC,
    PT_HIPROC,
}

#[derive(Debug, PartialEq)]
enum PF {
    PF_X,
    PF_W,
    PF_R,
    PF_MASKPROC,
}

#[derive(Debug, PartialEq)]
enum SHT {
    SHT_NULL,
    SHT_PROGBITS,
    SHT_SYMTAB,
    SHT_STRTAB,
    SHT_RELA,
    SHT_HASH,
    SHT_DYNAMIC,
    SHT_NOTE,
    SHT_NOBITS,
    SHT_REL,
    SHT_SHLIB,
    SHT_DYNSYM,
    SHT_INIT_ARRAY,
    SHT_FINI_ARRAY,
    SHT_PREINIT_ARRAY,
    SHT_GROUP,
    SHT_SYMTAB_SHNDX,
    SHT_LOPROC,
    SHT_HIPROC,
}

#[derive(Debug, PartialEq)]
enum SHF {
    SHF_WRITE,
    SHF_ALLOC,
    SHF_EXECINSTR,
    SHF_MERGE,
    SHF_STRINGS,
    SHF_INFO_LINK,
    SHF_LINK_ORDER,
    SHF_OS_NONCONFORMING,
    SHF_GROUP,
    SHF_TLS,
    SHF_MASKPROC,
}

#[derive(Debug, PartialEq)]
enum STB {
    STB_LOCAL,
    STB_GLOBAL,
    STB_WEAK,
    STB_LOPROC,
    STB_HIPROC,
}

#[derive(Debug, PartialEq)]
enum STV {
    STV_DEFAULT,
    STV_INTERNAL,
    STV_HIDDEN,
    STV_PROTECTED,
}

#[derive(Debug, PartialEq)]
enum SHN {
    SHN_UNDEF,
    SHN_ABS,
    SHN_COMMON,
}

#[derive(Debug, PartialEq)]
enum R_386 {
    R_386_NONE,
    R_386_32,
    R_386_PC32,
    R_386_GOT32,
    R_386_PLT32,
    R_386_COPY,
    R_386_GLOB_DAT,
    R_386_JMP_SLOT,
    R_386_RELATIVE,
    R_386_GOTOFF,
    R_386_GOTPC,
}

fn elf_class(input: &[u8]) -> IResult<&[u8], EI_CLASS> {
    alt((map(tag(&[1]), |_| EI_CLASS::ELFCLASS32), map(tag(&[2]), |_| EI_CLASS::ELFCLASS64)))(input)
}

fn elf_data(input: &[u8]) -> IResult<&[u8], EI_DATA> {
    alt((map(tag(&[1]), |_| EI_DATA::ELFDATA2LSB), map(tag(&[2]), |_| EI_DATA::ELFDATA2MSB)))(input)
}

fn elf_version(input: &[u8]) -> IResult<&[u8], EI_VERSION> {
    map(tag(&[1]), |_| EI_VERSION::EV_CURRENT)(input)
}

fn elf_osabi(input: &[u8]) -> IResult<&[u8], EI_OSABI> {
    alt((
        map(tag(&[0]), |_| EI_OSABI::ELFOSABI_NONE),
        map(tag(&[1]), |_| EI_OSABI::ELFOSABI_SYSV),
        map(tag(&[2]), |_| EI_OSABI::ELFOSABI_HPUX),
        map(tag(&[3]), |_| EI_OSABI::ELFOSABI_NETBSD),
        map(tag(&[6]), |_| EI_OSABI::ELFOSABI_LINUX),
        map(tag(&[7]), |_| EI_OSABI::ELFOSABI_SOLARIS),
        map(tag(&[8]), |_| EI_OSABI::ELFOSABI_IRIX),
        map(tag(&[9]), |_| EI_OSABI::ELFOSABI_FREEBSD),
        map(tag(&[10]), |_| EI_OSABI::ELFOSABI_TRU64),
        map(tag(&[11]), |_| EI_OSABI::ELFOSABI_MODESTO),
        map(tag(&[12]), |_| EI_OSABI::ELFOSABI_OPENBSD),
    ))(input)
}

fn et(input: &[u8]) -> IResult<&[u8], ET> {
    alt((
        map(be_u16(0), |_| ET::ET_NONE),
        map(be_u16(1), |_| ET::ET_REL),
        map(be_u16(2), |_| ET::ET_EXEC),
        map(be_u16(3), |_| ET::ET_DYN),
        map(be_u16(4), |_| ET::ET_CORE),
    ))(input)
}

fn em(input: &[u8]) -> IResult<&[u8], EM> {
    alt((
        map(be_u16(0), |_| EM::EM_NONE),
        map(be_u16(1), |_| EM::EM_M32),
        map(be_u16(2), |_| EM::EM_SPARC),
        map(be_u16(3), |_| EM::EM_386),
        map(be_u16(4), |_| EM::EM_68K),
        map(be_u16(5), |_| EM::EM_88K),
        map(be_u16(7), |_| EM::EM_860),
        map(be_u16(8), |_| EM::EM_MIPS),
        map(be_u16(9), |_| EM::EM_S370),
        map(be_u16(10), |_| EM::EM_MIPS_RS3_LE),
        map(be_u16(15), |_| EM::EM_PARISC),
        map(be_u16(17), |_| EM::EM_VPP500),
        map(be_u16(18), |_| EM::EM_SPARC32PLUS),
        map(be_u16(19), |_| EM::EM_960),
        map(be_u16(20), |_| EM::EM_PPC),
        map(be_u16(21), |_| EM::EM_PPC64),
        map(be_u16(22), |_| EM::EM_S390),
        map(be_u16(23), |_| EM::EM_SPU),
        map(be_u16(36), |_| EM::EM_V800),
        map(be_u16(37), |_| EM::EM_FR20),
        map(be_u16(38), |_| EM::EM_RH32),
        map(be_u16(39), |_| EM::EM_RCE),
        map(be_u16(40), |_| EM::EM_ARM),
        map(be_u16(41), |_| EM::EM_ALPHA),
        map(be_u16(42), |_| EM::EM_SH),
        map(be_u16(43), |_| EM::EM_SPARCV9),
        map(be_u16(44), |_| EM::EM_TRICORE),
        map(be_u16(45), |_| EM::EM_ARC),
        map(be_u16(62), |_| EM::EM_X86_64),
        map(be_u16(83), |_| EM::EM_AVR32),
        map(be_u16(84), |_| EM::EM_FROB),
    ))(input)
}

fn ev(input: &[u8]) -> IResult<&[u8], EV> {
    alt((map(be_u32(0), |_| EV::EV_NONE), map(be_u32(1), |_| EV::EV_CURRENT)))(input)
}

fn pt(input: &[u8]) -> IResult<&[u8], PT> {
    alt((
        map(be_u32(0), |_| PT::PT_NULL),
        map(be_u32(1), |_| PT::PT_LOAD),
        map(be_u32(2), |_| PT::PT_DYNAMIC),
        map(be_u32(3), |_| PT::PT_INTERP),
        map(be_u32(4), |_| PT::PT_NOTE),
        map(be_u32(5), |_| PT::PT_SHLIB),
        map(be_u32(6), |_| PT::PT_PHDR),
        map(be_u32(7), |_| PT::PT_TLS),
        map(be_u32(0x70000000), |_| PT::PT_LOPROC),
        map(be_u32(0x7fffffff), |_| PT::PT_HIPROC),
    ))(input)
}

fn pf(input: &[u8]) -> IResult<&[u8], PF> {
    alt((
        map(be_u32(1), |_| PF::PF_X),
        map(be_u32(2), |_| PF::PF_W),
        map(be_u32(4), |_| PF::PF_R),
        map(be_u32(0xf0000000), |_| PF::PF_MASKPROC),
    ))(input)
}

fn sht(input: &[u8]) -> IResult<&[u8], SHT> {
    alt((
        map(be_u32(0), |_| SHT::SHT_NULL),
        map(be_u32(1), |_| SHT::SHT_PROGBITS),
        map(be_u32(2), |_| SHT::SHT_SYMTAB),
        map(be_u32(3), |_| SHT::SHT_STRTAB),
        map(be_u32(4), |_| SHT::SHT_RELA),
        map(be_u32(5), |_| SHT::SHT_HASH),
        map(be_u32(6), |_| SHT::SHT_DYNAMIC),
        map(be_u32(7), |_| SHT::SHT_NOTE),
        map(be_u32(8), |_| SHT::SHT_NOBITS),
        map(be_u32(9), |_| SHT::SHT_REL),
        map(be_u32(10), |_| SHT::SHT_SHLIB),
        map(be_u32(11), |_| SHT::SHT_DYNSYM),
        map(be_u32(14), |_| SHT::SHT_INIT_ARRAY),
        map(be_u32(15), |_| SHT::SHT_FINI_ARRAY),
        map(be_u32(16), |_| SHT::SHT_PREINIT_ARRAY),
        map(be_u32(17), |_| SHT::SHT_GROUP),
        map(be_u32(18), |_| SHT::SHT_SYMTAB_SHNDX),
        map(be_u32(0x70000000), |_| SHT::SHT_LOPROC),
        map(be_u32(0x7fffffff), |_| SHT::SHT_HIPROC),
    ))(input)
}

fn shf(input: &[u8]) -> IResult<&[u8], SHF> {
    alt((
        map(be_u32(1), |_| SHF::SHF_WRITE),
        map(be_u32(2), |_| SHF::SHF_ALLOC),
        map(be_u32(4), |_| SHF::SHF_EXECINSTR),
        map(be_u32(0x10), |_| SHF::SHF_MERGE),
        map(be_u32(0x20), |_| SHF::SHF_STRINGS),
        map(be_u32(0x40), |_| SHF::SHF_INFO_LINK),
        map(be_u32(0x80), |_| SHF::SHF_LINK_ORDER),
        map(be_u32(0x100), |_| SHF::SHF_OS_NONCONFORMING),
        map(be_u32(0x200), |_| SHF::SHF_GROUP),
        map(be_u32(0x400), |_| SHF::SHF_TLS),
        map(be_u32(0xf0000000), |_| SHF::SHF_MASKPROC),
    ))(input)
}

fn stb(input: &[u8]) -> IResult<&[u8], STB> {
    alt((
        map(be_u8(0), |_| STB::STB_LOCAL),
        map(be_u8(1), |_| STB::STB_GLOBAL),
        map(be_u8(2), |_| STB::STB_WEAK),
        map(be_u8(0x10), |_| STB::STB_LOPROC),
        map(be_u8(0x12), |_| STB::STB_HIPROC),
    ))(input)
}

fn stv(input: &[u8]) -> IResult<&[u8], STV> {
    alt((
        map(be_u8(0), |_| STV::STV_DEFAULT),
        map(be_u8(1), |_| STV::STV_INTERNAL),
        map(be_u8(2), |_| STV::STV_HIDDEN),
        map(be_u8(3), |_| STV::STV_PROTECTED),
    ))(input)
}

fn shn(input: &[u8]) -> IResult<&[u8], SHN> {
    alt((
        map(be_u16(0), |_| SHN::SHN_UNDEF),
        map(be_u16(0xfff1), |_| SHN::SHN_ABS),
        map(be_u16(0xfff2), |_| SHN::SHN_COMMON),
    ))(input)
}

fn r_386(input: &[u8]) -> IResult<&[u8], R_386> {
    alt((
        map(be_u32(0), |_| R_386::R_386_NONE),
        map(be_u32(1), |_| R_386::R_386_32),
        map(be_u32(2), |_| R_386::R_386_PC32),
        map(be_u32(3), |_| R_386::R_386_GOT32),
        map(be_u32(4), |_| R_386::R_386_PLT32),
        map(be_u32(5), |_| R_386::R_386_COPY),
        map(be_u32(6), |_| R_386::R_386_GLOB_DAT),
        map(be_u32(7), |_| R_386::R_386_JMP_SLOT),
        map(be_u32(8), |_| R_386::R_386_RELATIVE),
        map(be_u32(9), |_| R_386::R_386_GOTOFF),
        map(be_u32(10), |_| R_386::R_386_GOTPC),
    ))(input)
}

fn elf_header(input: &[u8]) -> IResult<&[u8], (EI_CLASS, EI_DATA, EI_VERSION, EI_OSABI, ET, EM, EV, u32, u32, u32, u32, u16, u16, u16, u16)> {
    let (input, _) = tag(&[0x7f, 0x45, 0x4c, 0x46])(input)?;
    let (input, elf_class) = elf_class(input)?;
    let (input, elf_data) = elf_data(input)?;
    let (input, elf_version) = elf_version(input)?;
    let (input, elf_osabi) = elf_osabi(input)?;
    let (input, et) = et(input)?;
    let (input, em) = em(input)?;
    let (input, ev) = ev(input)?;
    let (input, e_entry) = be_u32(input)?;
    let (input, e_phoff) = be_u32(input)?;
    let (input, e_shoff) = be_u32(input)?;
    let (input, e_flags) = be_u32(input)?;
    let (input, e_ehsize) = be_u16(input)?;
    let (input, e_phentsize) = be_u16(input)?;
    let (input, e_phnum) = be_u16(input)?;
    let (input, e_shentsize) = be_u16(input)?;
    let (input, e_shnum) = be_u16(input)?;
    let (input, e_shstrndx) = be_u16(input)?;
    Ok((input, (elf_class, elf_data, elf_version, elf_osabi, et, em, ev, e_entry, e_phoff, e_shoff, e_flags, e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx)))
}

fn program_header(input: &[u8]) -> IResult<&[u8], (PT, u32, u32, u32, u32, u32, PF)> {
    let (input, pt) = pt(input)?;
    let (input, p_offset) = be_u32(input)?;
    let (input, p_vaddr) = be_u32(input)?;
    let (input, p_paddr) = be_u32(input)?;
    let (input, p_filesz) = be_u32(input)?;
    let (input, p_memsz)