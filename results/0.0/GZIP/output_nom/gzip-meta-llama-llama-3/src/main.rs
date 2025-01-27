use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::take_while_m_n,
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
struct GzipHeader {
    id1: u8,
    id2: u8,
    compression_method: u8,
    flags: u8,
    modification_time: u32,
    extra_flags: u8,
    operating_system: u8,
}

impl GzipHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, id1) = verify(take(1usize), |x| x[0] == 0x1f)(input)?;
        let (input, id2) = verify(take(1usize), |x| x[0] == 0x8b)(input)?;
        let (input, compression_method) = take(1usize)(input)?;
        let (input, flags) = take(1usize)(input)?;
        let (input, modification_time) = be_u32(input)?;
        let (input, extra_flags) = take(1usize)(input)?;
        let (input, operating_system) = take(1usize)(input)?;
        Ok((
            input,
            GzipHeader {
                id1: id1[0],
                id2: id2[0],
                compression_method: compression_method[0],
                flags: flags[0],
                modification_time,
                extra_flags: extra_flags[0],
                operating_system: operating_system[0],
            },
        ))
    }
}

#[derive(Debug)]
struct GzipFooter {
    crc16: u32,
    isize: u32,
}

impl GzipFooter {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, crc16) = be_u32(input)?;
        let (input, isize) = be_u32(input)?;
        Ok((input, GzipFooter { crc16, isize }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).unwrap();
    let (input, header) = GzipHeader::parse(&data).unwrap();
    println!("Gzip Header: {:?}", header);
    let (input, _) = take_while_m_n(0, header.flags as usize, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m_n(0, 1, |x| x != 0)(input).unwrap();
    let (input, _) = take_while_m