use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
enum TiffTag {
    ImageWidth(u32),
    ImageLength(u32),
    BitsPerSample(Vec<u16>),
    Compression(u16),
    PhotometricInterpretation(u16),
    StripOffsets(Vec<u32>),
    SamplesPerPixel(u16),
    RowsPerStrip(u32),
    StripByteCounts(Vec<u32>),
    XResolution(f64),
    YResolution(f64),
    ResolutionUnit(u16),

    // Add other TIFF tags as needed...
    Unknown(u16, Vec<u8>),
}


fn tiff_header(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag(b"II\0\0")(input)?; //Check for little endian
    let (input, _) = be_u16(input)?; //Version
    Ok((input, ()))
}


fn tiff_ifd(input: &[u8]) -> IResult<&[u8], Vec<TiffTag>> {
    let (input, num_entries) = be_u16(input)?;
    let mut tags = Vec::new();
    let mut remaining_input = input;

    for _ in 0..num_entries {
        let (i, tag) = tiff_tag(remaining_input)?;
        tags.push(tag);
        remaining_input = i;
    }

    let (input, next_ifd_offset) = be_u32(remaining_input)?;
    Ok((input, tags))
}


fn tiff_tag(input: &[u8]) -> IResult<&[u8], TiffTag> {
    let (input, tag_id) = be_u16(input)?;
    let (input, tag_type) = be_u16(input)?;
    let (input, count) = be_u32(input)?;
    let (input, value_offset) = be_u32(input)?;

    match tag_id {
        256 => map(be_u32, |v| TiffTag::ImageWidth(v))(input),
        257 => map(be_u32, |v| TiffTag::ImageLength(v))(input),
        258 => {
            let num_samples = count as usize;
            map(recognize(tuple((be_u16, take(2*(num_samples as usize-1))))), |bytes| {
                TiffTag::BitsPerSample(bytes.chunks(2).map(|chunk| u16::from_be_bytes(chunk.try_into().unwrap())).collect())
            })(input)
        },
        259 => map(be_u16, |v| TiffTag::Compression(v))(input),
        262 => map(be_u16, |v| TiffTag::PhotometricInterpretation(v))(input),
        273 => {
            let num_strips = count as usize;
            map(recognize(tuple((be_u32, take(4*(num_strips as usize -1))))), |bytes| {
                TiffTag::StripOffsets(bytes.chunks(4).map(|chunk| u32::from_be_bytes(chunk.try_into().unwrap())).collect())
            })(input)
        },
        277 => map(be_u16, |v| TiffTag::SamplesPerPixel(v))(input),
        278 => map(be_u32, |v| TiffTag::RowsPerStrip(v))(input),
        279 => {
            let num_strips = count as usize;
            map(recognize(tuple((be_u32, take(4*(num_strips as usize -1))))), |bytes| {
                TiffTag::StripByteCounts(bytes.chunks(4).map(|chunk| u32::from_be_bytes(chunk.try_into().unwrap())).collect())
            })(input)
        },
        282 => map_res(take(8), |bytes| {
            f64::from_be_bytes(bytes.try_into().unwrap())
        }, |v| TiffTag::XResolution(v))(input),
        283 => map_res(take(8), |bytes| {
            f64::from_be_bytes(bytes.try_into().unwrap())
        }, |v| TiffTag::YResolution(v))(input),
        296 => map(be_u16, |v| TiffTag::ResolutionUnit(v))(input),
        _ => map(tuple((tag_id, take(count as usize))), |(id, data)| TiffTag::Unknown(id, data.to_vec()))(input),
    }
}



fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", err);
        return;
    }

    match tiff_header(&buffer) {
        Ok((remaining, _)) => {
            match tiff_ifd(remaining) {
                Ok((_, tags)) => {
                    println!("{:?}", tags);
                }
                Err(err) => {
                    eprintln!("Error parsing IFD: {:?}", err);
                }
            }
        }
        Err(err) => {
            eprintln!("Error parsing header: {:?}", err);
        }
    }
}
