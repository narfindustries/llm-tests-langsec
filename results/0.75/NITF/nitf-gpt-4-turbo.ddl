module NITF {

  type U8 = UInt(8)
  type U16 = UInt(16)
  type U32 = UInt(32)
  type ASCII = String(ASCII)

  type Header = struct {
    fhdr: ASCII[21],
    fver: ASCII[9],
    clevel: ASCII[2],
    stype: ASCII[4],
    ostaID: ASCII[10],
    ftitle: ASCII[80],
    fsclas: ASCII[1],
    fscltx: ASCII[43],
    fsclrs: ASCII[1],
    fsclas_sys: ASCII[2],
    fsclas_codewords: ASCII[11],
    fsclas_ctlh: ASCII[2],
    fsclas_rel: ASCII[20],
    fsclas_dectl: ASCII[2],
    fsclas_declim: ASCII[1],
    origsta: ASCII[10],
    flng: ASCII[12],
    hl: U16
  }

  type ImageSegment = struct {
    im: ASCII[2],
    imid: ASCII[10],
    imdt: ASCII[14],
    imclass: ASCII[1],
    imenc: ASCII[1],
    imode: ASCII[1],
    imsh: ASCII[4],
    imcom: U8,
    imfl: U32
  }

  type TextSegment = struct {
    tx: ASCII[2],
    textid: ASCII[7],
    txtalvl: U8,
    txtdate: ASCII[14],
    txtitl: ASCII[80],
    txtfmt: ASCII[3],
    txfl: U32
  }

  type DataSegment = struct {
    de: ASCII[2],
    desid: ASCII[25],
    desver: U8,
    declvl: U8,
    desclass: ASCII[1],
    desclsy: ASCII[2],
    descode: ASCII[11],
    desctlh: ASCII[2],
    desrel: ASCII[20],
    desdctp: ASCII[2],
    desdecl: ASCII[1],
    desdeclim: ASCII[1],
    dedica: ASCII[1],
    dedata: ASCII[15]
  }

  type File = struct {
    header: Header,
    imageSegments: [ImageSegment](@header.hl),
    textSegments: [TextSegment](@header.hl),
    dataSegments: [DataSegment](@header.hl)
  }

}