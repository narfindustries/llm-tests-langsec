module NITF {
    type FileHeader = {
        fhdr: [4]u8 = [0x4E, 0x49, 0x54, 0x46],
        fver: [5]u8 = [0x30, 0x32, 0x2E, 0x31, 0x30],
        clevel: u8,
        stype: u8,
        fsdwng: [40]u8,
        fscop: [2]u8,
        fscpys: [5]u8,
        encrypt: u8,
        originst: [27]u8,
        filedt: [14]u8,
        ftitle: [80]u8,
        fpseudoname: [40]u8?,
        num_image_segments: [3]u8,
        num_graphic_segments: [3]u8,
        num_text_segments: [3]u8,
        num_data_extension_segments: [3]u8,
        num_reserved_extension_segments: [3]u8
    }

    type ImageSubheader = {
        im: [2]u8 = [0x49, 0x4D],
        icat: [8]u8,
        isorce: [42]u8,
        nrows: [8]u8,
        ncols: [8]u8,
        pvtype: [3]u8,
        irep: [8]u8,
        imode: [4]u8,
        nbands: u8,
        xbands: [5]u8?,
        isync: u8,
        ilocrow: [10]u8,
        iloccol: [10]u8,
        geolocation: GeolocationData?
    }

    type GeolocationData = {
        coords: [60]u8,
        nicom: u8,
        com: [80]u8[]
    }

    type GraphicSubheader = {
        sg: [2]u8 = [0x53, 0x47],
        stype: [10]u8,
        scolor: [6]u8,
        sdlvl: [3]u8,
        salvl: [3]u8
    }

    type TextSubheader = {
        te: [2]u8 = [0x54, 0x45],
        textid: [10]u8,
        txtalvl: [3]u8
    }

    type DataExtensionSubheader = {
        de: [2]u8 = [0x44, 0x45],
        desid: [25]u8,
        desver: [2]u8
    }

    type NITFFile = {
        file_header: FileHeader,
        image_segments: ImageSubheader[],
        graphic_segments: GraphicSubheader[]?,
        text_segments: TextSubheader[]?,
        data_extension_segments: DataExtensionSubheader[]?
    }
}