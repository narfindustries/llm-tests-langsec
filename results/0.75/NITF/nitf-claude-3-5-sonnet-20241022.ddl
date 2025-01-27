def Main = [
    def Version = "2.1"
    def Type = "NITF"
    def Header = {
        FHDR: FWD 4 of "NITF";
        FVER: FWD 5 of Version;
        CLEVEL: !FWD 2;
        STYPE: FWD 4 of Type;
        OSTAID: !FWD 10;
        FDT: !FWD 14;
        FTITLE: !FWD 80;
        FSCLAS: !FWD 1;
        FSCLSY: !FWD 2;
        HL: !UINT 6;
        NUMI: !UINT 3
    }

    def ImageSegment = {
        IM: FWD 2 of "IM";
        IID1: !FWD 10;
        IDATIM: !FWD 14;
        TGTID: !FWD 17;
        IID2: !FWD 80;
        ISORCE: !FWD 42;
        NROWS: !UINT 8;
        NCOLS: !UINT 8
    }

    hdr: Header;
    images: Many(ImageSegment, $hdr.NUMI)
]