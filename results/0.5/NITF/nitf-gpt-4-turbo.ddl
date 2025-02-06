module NITF {
  using std::string;
  using std::uint16;
  using std::uint32;
  using std::uint8;

  struct FileHeader {
    string FHDR : 9;
    uint16 CLEVEL;
    string STYPE : 4;
    string OSTAID : 10;
    string FDT : 14;
    string FTITLE : 80;
    char FSCLAS;
    string FSCLSY : 2;
    string FSCODE : 11;
    string FSCTLH : 2;
    string FSREL : 20;
    string FSDCTP : 2;
    string FSDCDT : 8;
    string FSDCXM : 4;
    string FSORGN : 27;
    string FSCAUT : 40;
    string FSCTLN : 15;
    uint16 FSCOP;
    uint16 FSCPYS;
    char ENCRYP;
    string FBKGC : 3;
    string ONAME : 24;
    string OPHONE : 18;
    uint32 FL;
    uint16 HL;
  }

  struct ImageSubheader {
    string IID1 : 10;
    string IDATIM : 14;
    string TGTID : 17;
    string IID2 : 80;
    char ISCLAS;
    string ISCLSY : 2;
    string ISCODE : 11;
    string ISCTLH : 2;
    string ISREL : 20;
    string ISDCTP : 2;
    string ISDCDT : 8;
    string ISDCXM : 4;
    string ISORGN : 27;
    string ISCAUT : 40;
    string ISCTLN : 15;
    string ISDWNG : 6;
    string ISDEVT : 12;
    string ISORCE : 42;
    uint32 NROWS;
    uint32 NCOLS;
    string PVTYPE : 3;
    string IREP : 8;
    string ICAT : 8;
    uint8 ABPP;
    char PJUST;
    char ICORDS;
    string IGEOLO : 60;
    uint16 NICOM;
    array<string, 80> ICOM : NICOM;
    string IC : 2;
    string COMRAT : 4;
    uint8 NBANDS;
    uint8 XBANDS;
    char IMODE;
    uint16 NBPR;
    uint16 NBPC;
    uint16 NPPBH;
    uint16 NPPBV;
    uint8 NBPP;
    uint16 IDLVL;
    uint16 IALVL;
    string ILOC : 10;
    string IMAG : 4;
    uint16 UDIDL;
    uint16 UDOFL;
    uint16 IXSHDL;
    uint16 IXSOFL;
    uint32 IMDATOFF;
    uint32 BMRLNTH;
    uint32 TMRLNTH;
    uint32 TPSOFL;
    uint32 TPSBND;
    array<uint8, 5> Reserved;
    string UserDefinedSubheader : UDIDL;
    string ExtendedSubheader : IXSHDL;
  }

  struct NITF {
    FileHeader header;
    array<ImageSubheader> images : header.FL;
  }
}