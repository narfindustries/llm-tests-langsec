module NITF {

  type U16 = uint(16)
  type U32 = uint(32)
  type ASCII = bytes

  record Header {
    fhdr: ASCII[21]
    fver: ASCII[9]
    clevel: U16
    stype: ASCII[4]
    ostaId: ASCII[10]
    ftitle: ASCII[80]
    fsclas: ASCII[1]
    fsclsy: ASCII[2]
    fscode: ASCII[11]
    fsctlh: ASCII[2]
    fsrel: ASCII[20]
    fsdctp: ASCII[2]
    fsdcdt: ASCII[8]
    fsdcxm: ASCII[4]
    fsdg: ASCII[1]
    fsdgdt: ASCII[8]
    fscltx: ASCII[43]
    fscatp: ASCII[1]
    fscaut: ASCII[40]
    fscrsn: ASCII[1]
    fssrdt: ASCII[8]
    fsctln: ASCII[15]
    encrypted: ASCII[1]
    fbkgc: ASCII[3]
    oname: ASCII[24]
    ophone: ASCII[18]
    fl: U32
    hl: U16
    numi: U16
  }

  record ImageSegment {
    im: ASCII[2]
    iid1: ASCII[10]
    idatim: ASCII[14]
    ttarget: ASCII[17]
    iid2: ASCII[80]
    isclas: ASCII[1]
    isclsy: ASCII[2]
    iscode: ASCII[11]
    isctlh: ASCII[2]
    isrel: ASCII[20]
    isdctp: ASCII[2]
    isdcdt: ASCII[8]
    isdcxm: ASCII[4]
    isdg: ASCII[1]
    isdgdt: ASCII[8]
    iscltx: ASCII[43]
    iscatp: ASCII[1]
    iscaut: ASCII[40]
    iscrsn: ASCII[1]
    issrdt: ASCII[8]
    isctln: ASCII[15]
    encrypted: ASCII[1]
    isorce: ASCII[42]
    nbits: ASCII[2]
    nbands: U16
    x: U32
    y: U32
    mode: ASCII[1]
    imdatoff: U32
    imdatlen: U32
    bmralvl: U16
    lvls: U16
    locs: U16
    lovl: U16
    comrat: ASCII[4]
    imdatyp: ASCII[3]
    imode: ASCII[1]
    nbpr: U16
    nbpc: U16
    nppbh: U16
    nppbv: U16
    nbpp: U16
    idlvl: U16
    ialvl: U16
    iloc: U16
    imag: U16
  }

  record NITF {
    header: Header
    imageSegments: [ImageSegment](_.header.numi)
  }

  let nitf = parse NITF
}