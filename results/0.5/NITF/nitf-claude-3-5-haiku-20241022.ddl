nitf = module {
    FileHeader = struct {
        fhdr: [4]u8 where fhdr == b"NITF",
        fver: [5]u8 where fver == b"02.10",
        clevel: u8,
        stype: u8,
        fscop: [2]u8,
        fsclsy: [11]u8,
        fscode: [14]u8,
        fsctlh: [20]u8,
        fsrel: [20]u8,
        fsdcop: [8]u8,
        flength: u32,
        hlength: u32,
        reserved: [3]u8,
        encrypt: u8,
        originator: [27]u8,
        origphone: [18]u8
    }

    ImageSubheader = struct {
        marker: [2]u8 where marker == b"IM",
        datetime: [14]u8,
        targetid: [10]u8,
        source: [42]u8,
        rows: u32,
        cols: u32,
        pixeltype: u8,
        representation: u8,
        bitsperpixel: u8,
        compression: u8,
        geolocation: Geolocation
    }

    Geolocation = struct {
        coords: [4]{
            lat: f64,
            lon: f64
        }
    }

    GraphicSubheader = struct {
        marker: [2]u8 where marker == b"SY",
        graphictype: u8
    }

    TextSubheader = struct {
        marker: [2]u8 where marker == b"TX",
        textformat: u8
    }

    NITFFile = struct {
        header: FileHeader,
        images: [256]ImageSubheader,
        graphics: [256]GraphicSubheader,
        texts: [256]TextSubheader
    }
}