use nom::{
    bytes::complete::take,
    combinator::{map, opt},
    multi::count,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NitfHeader {
    fhdr: [u8; 4],
    fver: [u8; 5],
    clevel: u16,
    stype: [u8; 4],
    ostaid: [u8; 10],
    fdt: [u8; 14],
    ftitle: [u8; 80],
    fsclas: u8,
    fs_copy_num: u16,
    fs_num_copies: u16,
    encryp: u8,
    fbkgc: [u8; 3],
    oname: [u8; 24],
    ophone: [u8; 18],
    fl: u32,
    hl: u16,
    numi: u16,
    lish: Vec<u16>,
    li: Vec<u32>,
    nums: u16,
    lssh: Vec<u16>,
    ls: Vec<u32>,
}

#[derive(Debug)]
struct ImageSegment {
    im: [u8; 2],
    iid1: [u8; 10],
    idatim: [u8; 14],
    tgtid: [u8; 17],
    iid2: [u8; 80],
    isclas: u8,
    encryp: u8,
    isorce: [u8; 42],
    nrows: u32,
    ncols: u32,
    pvtype: u8,
    irep: [u8; 8],
    icat: [u8; 8],
    abpp: u8,
    pjust: u8,
    icords: Option<u8>,
    igeolo: Option<[u8; 60]>,
    nicom: u16,
    ic: [u8; 2],
    comrat: [u8; 4],
    nbands: u8,
    xbands: Option<u16>,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, (
        fhdr,
        fver,
        clevel,
        stype,
        ostaid,
        fdt,
        ftitle,
        fsclas,
        fs_copy_num,
        fs_num_copies,
        encryp,
        fbkgc,
        oname,
        ophone,
        fl,
        hl,
        numi
    )) = tuple((
        take(4usize),
        take(5usize),
        be_u16,
        take(4usize),
        take(10usize),
        take(14usize),
        take(80usize),
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        take(3usize),
        take(24usize),
        take(18usize),
        be_u32,
        be_u16,
        be_u16
    ))(input)?;

    let (input, lish) = count(be_u16, numi as usize)(input)?;
    let (input, li) = count(be_u32, numi as usize)(input)?;
    let (input, nums) = be_u16(input)?;
    let (input, lssh) = count(be_u16, nums as usize)(input)?;
    let (input, ls) = count(be_u32, nums as usize)(input)?;

    let header = NitfHeader {
        fhdr: fhdr.try_into().unwrap(),
        fver: fver.try_into().unwrap(),
        clevel,
        stype: stype.try_into().unwrap(),
        ostaid: ostaid.try_into().unwrap(),
        fdt: fdt.try_into().unwrap(),
        ftitle: ftitle.try_into().unwrap(),
        fsclas: fsclas as u8,
        fs_copy_num,
        fs_num_copies,
        encryp: encryp as u8,
        fbkgc: fbkgc.try_into().unwrap(),
        oname: oname.try_into().unwrap(),
        ophone: ophone.try_into().unwrap(),
        fl,
        hl,
        numi,
        lish,
        li,
        nums,
        lssh,
        ls,
    };

    Ok((input, header))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, (
        im,
        iid1,
        idatim,
        tgtid,
        iid2,
        isclas,
        encryp,
        isorce,
        nrows,
        ncols,
        pvtype,
        irep,
        icat,
        abpp,
        pjust,
    )) = tuple((
        take(2usize),
        take(10usize),
        take(14usize),
        take(17usize),
        take(80usize),
        be_u16,
        be_u16,
        take(42usize),
        be_u32,
        be_u32,
        be_u16,
        take(8usize),
        take(8usize),
        be_u16,
        be_u16,
    ))(input)?;

    let (input, icords) = opt(be_u16)(input)?;
    let (input, igeolo) = match icords {
        Some(_) => map(take(60usize), |v: &[u8]| Some(v.try_into().unwrap()))(input)?,
        None => (input, None),
    };

    let (input, (
        nicom,
        ic,
        comrat,
        nbands
    )) = tuple((
        be_u16,
        take(2usize),
        take(4usize),
        be_u16,
    ))(input)?;

    let (input, xbands) = if nbands as u8 == 0 {
        let (input, xb) = be_u16(input)?;
        (input, Some(xb))
    } else {
        (input, None)
    };

    let image_segment = ImageSegment {
        im: im.try_into().unwrap(),
        iid1: iid1.try_into().unwrap(),
        idatim: idatim.try_into().unwrap(),
        tgtid: tgtid.try_into().unwrap(),
        iid2: iid2.try_into().unwrap(),
        isclas: isclas as u8,
        encryp: encryp as u8,
        isorce: isorce.try_into().unwrap(),
        nrows,
        ncols,
        pvtype: pvtype as u8,
        irep: irep.try_into().unwrap(),
        icat: icat.try_into().unwrap(),
        abpp: abpp as u8,
        pjust: pjust as u8,
        icords: icords.map(|v| v as u8),
        igeolo,
        nicom,
        ic: ic.try_into().unwrap(),
        comrat: comrat.try_into().unwrap(),
        nbands: nbands as u8,
        xbands,
    };

    Ok((input, image_segment))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_header(&buffer) {
        Ok((mut remaining, header)) => {
            println!("NITF Header: {:?}", header);
            
            for _ in 0..header.numi {
                match parse_image_segment(remaining) {
                    Ok((new_remaining, image_segment)) => {
                        println!("Image Segment: {:?}", image_segment);
                        remaining = new_remaining;
                    }
                    Err(e) => {
                        eprintln!("Error parsing image segment: {:?}", e);
                        break;
                    }
                }
            }
        }
        Err(e) => eprintln!("Error parsing NITF header: {:?}", e),
    }

    Ok(())
}