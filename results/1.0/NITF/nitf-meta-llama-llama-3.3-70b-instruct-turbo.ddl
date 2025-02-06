format nitf {
  header {
    field fnfmt: string = "NITF";
    field fver: string = "02.10";
    field clevl: uint8;
    field std: string = "NITF";
    field orgn: string;
    field orgnct: string;
    field fsc: enum { U, C, S, TS };
    field ftitle: string;
    field fsh: string;
    field fsscl: enum { U, C, S, TS };
    field fsicl: string;
    field fdescending: bool;
    field fdatetime: uint64;
    field foffset: uint32;
    field fl: uint32;
    field fheaderlength: uint32;
    field standard: string = "MIL-STD-2500C";
  }

  segment image_segment {
    field iid1: string;
    field mcm: string;
    field icat: string;
    field icle: string;
    field icom: string;
    field icop: string;
    field irep: enum { MONO, RGB, YCbCr, XYZ };
    field idat1: enum { INT8, UINT8, INT16, UINT16, INT32, UINT32, FLOAT32, FLOAT64 };
    field iweek: uint16;
    field iband: uint8;
    field ibandcount: uint8;
    field irow: uint32;
    field icol: uint32;
    field isx: uint32;
    field isy: uint32;
    field pxpx: uint16;
    field igc: string;
    field igi: string;
    field irm: string;
    field irp: string;
    field itx: string;
    field icb: string;
    field icp: string;
    field ico: string;
    field nppbv: uint16;
    field nppbn: uint16;
    field npfc: uint16;
    field npfn: uint16;
    field npac: uint16;
    field npft: uint16;
    field npfec: uint16;
    field npfen: uint16;
    field npbc: uint16;
    field npbt: uint16;
    field nppc: uint16;
    field nppn: uint16;
  }

  segment graphic_segment {
    field gid1: string;
    field gtype: enum { G, T };
    field gm: string;
    field gcat: string;
    field gcle: string;
    field gcom: string;
    field gop: string;
    field grep: enum { VECTOR, RASTER };
    field gdat: enum { INT8, UINT8, INT16, UINT16, INT32, UINT32, FLOAT32, FLOAT64 };
    field gweek: uint16;
    field gband: uint8;
    field gbandcount: uint8;
    field grow: uint32;
    field gcol: uint32;
    field gsx: uint32;
    field gsy: uint32;
    field gpxpx: uint16;
    field ggc: string;
    field ggi: string;
    field grm: string;
    field grp: string;
    field gtx: string;
    field gcb: string;
    field gcp: string;
    field gco: string;
  }

  segment text_segment {
    field tid1: string;
    field ttype: enum { ASCII, EBCDIC };
    field ttx: string;
    field tdx: uint32;
    field tnc: uint32;
    field tnl: uint32;
    field trc: uint32;
  }

  segment data_extension_segment {
    field dxid: string;
    field dxtype: string;
    field dxe: string;
    field dxo: string;
    field dxi: string;
    field dxp: string;
    field dxf: string;
    field dxs: string;
  }

  segment data_extension_segment_cont {
    field cdxid: string;
    field cdtype: string;
    field cdxe: string;
    field cdxo: string;
    field cdxi: string;
    field cdxp: string;
    field cdxf: string;
    field cdxs: string;
  }
}