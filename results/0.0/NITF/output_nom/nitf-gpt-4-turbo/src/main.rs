use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map_res, opt},
    multi::count,
    number::complete::{be_u16, be_u32, be_u64},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct FileHeader {
    file_profile_name: String,
    file_version: String,
    complexity_level: String,
    system_type: String,
    origin_station_id: String,
    file_date_time: String,
    file_title: String,
    file_security_class: String,
    file_copy_number: String,
    file_num_of_copys: String,
    encryption: u16,
    file_bg_color: String,
    originator_name: String,
    originator_phone: String,
    file_length: u32,
    header_length: u16,
}

#[derive(Debug)]
struct ImageSegment {
    iid1: String,
    idatim: String,
    tgtid: String,
    iid2: String,
    isorce: String,
    nrows: u32,
    ncols: u32,
    pvtype: String,
    irep: String,
    icat: String,
    abpp: u16,
    pjust: String,
    icords: String,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct DataExtensionSegment {
    desid: String,
    des_version: u16,
    declasn: String,
    overflowed_header_type: String,
    data_item_overflowed: String,
    des_data: Vec<u8>,
}

#[derive(Debug)]
struct NITF {
    header: FileHeader,
    image_segments: Vec<ImageSegment>,
    data_extension_segments: Vec<DataExtensionSegment>,
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, file_profile_name) = map_res(take(4usize), std::str::from_utf8)(input)?;
    let (input, file_version) = map_res(take(5usize), std::str::from_utf8)(input)?;
    let (input, complexity_level) = map_res(take(2usize), std::str::from_utf8)(input)?;
    let (input, system_type) = map_res(take(4usize), std::str::from_utf8)(input)?;
    let (input, origin_station_id) = map_res(take(10usize), std::str::from_utf8)(input)?;
    let (input, file_date_time) = map_res(take(14usize), std::str::from_utf8)(input)?;
    let (input, file_title) = map_res(take(80usize), std::str::from_utf8)(input)?;
    let (input, file_security_class) = map_res(take(1usize), std::str::from_utf8)(input)?;
    let (input, file_copy_number) = map_res(take(5usize), std::str::from_utf8)(input)?;
    let (input, file_num_of_copys) = map_res(take(5usize), std::str::from_utf8)(input)?;
    let (input, encryption) = be_u16(input)?;
    let (input, file_bg_color) = map_res(take(3usize), std::str::from_utf8)(input)?;
    let (input, originator_name) = map_res(take(24usize), std::str::from_utf8)(input)?;
    let (input, originator_phone) = map_res(take(18usize), std::str::from_utf8)(input)?;
    let (input, file_length) = be_u32(input)?;
    let (input, header_length) = be_u16(input)?;
    Ok((
        input,
        FileHeader {
            file_profile_name: file_profile_name.to_string(),
            file_version: file_version.to_string(),
            complexity_level: complexity_level.to_string(),
            system_type: system_type.to_string(),
            origin_station_id: origin_station_id.to_string(),
            file_date_time: file_date_time.to_string(),
            file_title: file_title.to_string(),
            file_security_class: file_security_class.to_string(),
            file_copy_number: file_copy_number.to_string(),
            file_num_of_copys: file_num_of_copys.to_string(),
            encryption,
            file_bg_color: file_bg_color.to_string(),
            originator_name: originator_name.to_string(),
            originator_phone: originator_phone.to_string(),
            file_length,
            header_length,
        },
    ))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, iid1) = map_res(take(10usize), std::str::from_utf8)(input)?;
    let (input, idatim) = map_res(take(14usize), std::str::from_utf8)(input)?;
    let (input, tgtid) = map_res(take(17usize), std::str::from_utf8)(input)?;
    let (input, iid2) = map_res(take(80usize), std::str::from_utf8)(input)?;
    let (input, isorce) = map_res(take(42usize), std::str::from_utf8)(input)?;
    let (input, nrows) = be_u32(input)?;
    let (input, ncols) = be_u32(input)?;
    let (input, pvtype) = map_res(take(3usize), std::str::from_utf8)(input)?;
    let (input, irep) = map_res(take(8usize), std::str::from_utf8)(input)?;
    let (input, icat) = map_res(take(8usize), std::str::from_utf8)(input)?;
    let (input, abpp) = be_u16(input)?;
    let (input, pjust) = map_res(take(1usize), std::str::from_utf8)(input)?;
    let (input, icords) = map_res(take(60usize), std::str::from_utf8)(input)?;
    let (input, image_data_length) = be_u32(input)?;
    let (input, image_data) = take(image_data_length)(input)?;
    Ok((
        input,
        ImageSegment {
            iid1: iid1.to_string(),
            idatim: idatim.to_string(),
            tgtid: tgtid.to_string(),
            iid2: iid2.to_string(),
            isorce: isorce.to_string(),
            nrows,
            ncols,
            pvtype: pvtype.to_string(),
            irep: irep.to_string(),
            icat: icat.to_string(),
            abpp,
            pjust: pjust.to_string(),
            icords: icords.to_string(),
            image_data: image_data.to_vec(),
        },
    ))
}

fn parse_data_extension_segment(input: &[u8]) -> IResult<&[u8], DataExtensionSegment> {
    let (input, desid) = map_res(take(25usize), std::str::from_utf8)(input)?;
    let (input, des_version) = be_u16(input)?;
    let (input, declasn) = map_res(take(2usize), std::str::from_utf8)(input)?;
    let (input, overflowed_header_type) = map_res(take(6usize), std::str::from_utf8)(input)?;
    let (input, data_item_overflowed) = map_res(take(3usize), std::str::from_utf8)(input)?;
    let (input, des_data_length) = be_u32(input)?;
    let (input, des_data) = take(des_data_length)(input)?;
    Ok((
        input,
        DataExtensionSegment {
            desid: desid.to_string(),
            des_version,
            declasn: declasn.to_string(),
            overflowed_header_type: overflowed_header_type.to_string(),
            data_item_overflowed: data_item_overflowed.to_string(),
            des_data: des_data.to_vec(),
        },
    ))
}

fn parse_nitf(input: &[u8]) -> IResult<&[u8], NITF> {
    let (input, header) = parse_file_header(input)?;
    let (input, num_image_segments) = be_u16(input)?;
    let (input, image_segments) = count(parse_image_segment, num_image_segments as usize)(input)?;
    let (input, num_des) = be_u16(input)?;
    let (input, data_extension_segments) = count(parse_data_extension_segment, num_des as usize)(input)?;
    Ok((
        input,
        NITF {
            header,
            image_segments,
            data_extension_segments,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "No input file specified",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf(&buffer) {
        Ok((_, nitf)) => {
            println!("{:#?}", nitf);
        }
        Err(e) => {
            println!("Failed to parse NITF file: {:?}", e);
        }
    }

    Ok(())
}