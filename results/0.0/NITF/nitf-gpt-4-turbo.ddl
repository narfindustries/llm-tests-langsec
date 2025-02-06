grammar NITF;

import std.ascii;
import std.util;

struct FileHeader {
    FHDR : ascii.string(9);
    CLEVEL : ascii.string(2);
    STYPE : ascii.string(4);
    OSTAID : ascii.string(10);
    FDT : ascii.string(14);
    FTITLE : ascii.string(80);
    FSCLAS : ascii.string(1);
    FSCLSY : ascii.string(2);
    FSCODE : ascii.string(11);
    FSCTLH : ascii.string(2);
    FSREL : ascii.string(20);
    FSCAUT : ascii.string(20);
    FSCTLN : ascii.string(6);
    FSDWNG : ascii.string(6);
    FSDEVT : ascii.string(40);
    ENCRYP : ascii.string(1);
    FBKGC : ascii.string(3);
    ONAME : ascii.string(24);
    OPHONE : ascii.string(18);
    FL : ascii.string(6);
    HL : ascii.string(6);
    NUMI : ascii.string(3);
    LISH : std.util.array(ascii.string(6), std.util.to_uint(NUMI));
    LI : std.util.array(ascii.string(10), std.util.to_uint(NUMI));
    NUMS : ascii.string(3);
    LSSH : std.util.array(ascii.string(4), std.util.to_uint(NUMS));
    LS : std.util.array(ascii.string(6), std.util.to_uint(NUMS));
    NUMX : ascii.string(3);
    NUMT : ascii.string(3);
    LTSH : std.util.array(ascii.string(4), std.util.to_uint(NUMT));
    LT : std.util.array(ascii.string(5), std.util.to_uint(NUMT));
    NUMDES : ascii.string(3);
    LDSH : std.util.array(ascii.string(4), std.util.to_uint(NUMDES));
    LD : std.util.array(ascii.string(9), std.util.to_uint(NUMDES));
    NUMRES : ascii.string(3);
    LRESH : std.util.array(ascii.string(4), std.util.to_uint(NUMRES));
    LRE : std.util.array(ascii.string(7), std.util.to_uint(NUMRES));
    UDHDL : ascii.string(5);
    UDHOFL : ascii.string(3);
    UDOFL : ascii.string(6);
    XHDL : ascii.string(5);
    XHDLOFL : ascii.string(3);
    XOF : ascii.string(4);
}

struct ImageSegment {
    IID1 : ascii.string(10);
    IDATIM : ascii.string(14);
    TGTID : ascii.string(17);
    IID2 : ascii.string(80);
    ISCLAS : ascii.string(1);
    ISCLSY : ascii.string(2);
    ISCODE : ascii.string(11);
    ISCTLH : ascii.string(2);
    ISREL : ascii.string(20);
    ISCAUT : ascii.string(20);
    ISCTLN : ascii.string(6);
    ISDWNG : ascii.string(6);
    ISDEVT : ascii.string(40);
    ISORGN : ascii.string(27);
    ICAT : ascii.string(8);
    ABPP : ascii.string(2);
    PJUST : ascii.string(1);
    ICORDS : ascii.string(1);
    IGEOLO : ascii.string(60);
    NICOM : ascii.string(1);
    ICOM : std.util.array(ascii.string(80), std.util.to_uint(NICOM));
    NBANDS : ascii.string(1);
    XBANDS : ascii.string(5);
    IMODE : ascii.string(1);
    NBPR : ascii.string(4);
    NBPC : ascii.string(4);
    NPPBH : ascii.string(4);
    NPPBV : ascii.string(4);
    NBPP : ascii.string(2);
    IDLVL : ascii.string(3);
    IALVL : ascii.string(3);
    ILOC : ascii.string(10);
    IMAG : ascii.string(4);
    UDIDL : ascii.string(5);
    UDOFL : ascii.string(3);
    IXSHDL : ascii.string(5);
    IXSOFL : ascii.string(3);
    IMDATOFF : ascii.string(9);
    BMRLNTH : ascii.string(5);
    TMRLNTH : ascii.string(5);
    TPSIZE : ascii.string(2);
    TPEXT : ascii.string(3);
    TPNUM : ascii.string(2);
    TPLVL : ascii.string(3);
    IMAG2 : ascii.string(4);
    UDID : std.util.array(ascii.string(5), std.util.to_uint(UDIDL));
    IXSHD : std.util.array(ascii.string(5), std.util.to_uint(IXSHDL));
}

struct NITFFile {
    Header : FileHeader;
    Images : std.util.array(ImageSegment, std.util.to_uint(Header.NUMI));
}

root NITFFile;