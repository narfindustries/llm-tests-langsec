use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map_res},
    sequence::tuple,
    IResult,
};
use std::{fs::File, io::Read, env, str};

#[derive(Debug)]
struct FileHeader {
    fhdr: String,
    fver: String,
    clevel: u16,
    stype: String,
    osta_id: String,
    fdt: String,
    ftitle: String,
    fsclas: String,
    fscop: u32,
    fscpys: u32,
    encryp: String,
}

fn parse_u32(input: &[u8]) -> Result<u32, std::num::ParseIntError> {
    str::from_utf8(input).unwrap().trim().parse::<u32>()
}

fn parse_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    map_res(
        tuple((
            take(9usize),
            take(2usize),
            take_while_m_n(2, 2, |c: u8| c.is_ascii_digit()),
            take(4usize),
            take(10usize),
            take(14usize),
            take(80usize),
            take(1usize),
            take(5usize),
            take(5usize),
            take(1usize),
        )),
        |(fhdr, fver, clevel, stype, osta_id, fdt, ftitle, fsclas, fscop, fscpys, encryp)| {
            Ok(FileHeader {
                fhdr: str::from_utf8(fhdr)?.to_string(),
                fver: str::from_utf8(fver)?.to_string(),
                clevel: str::from_utf8(clevel)?.parse::<u16>()?,
                stype: str::from_utf8(stype)?.to_string(),
                osta_id: str::from_utf8(osta_id)?.to_string(),
                fdt: str::from_utf8(fdt)?.to_string(),
                ftitle: str::from_utf8(ftitle)?.to_string(),
                fsclas: str::from_utf8(fsclas)?.to_string(),
                fscop: parse_u32(fscop)?,
                fscpys: parse_u32(fscpys)?,
                encryp: str::from_utf8(encryp)?.to_string(),
            })
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <path_to_nitf_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_header(&buffer) {
        Ok((_remaining, header)) => {
            println!("Parsed Header: {:?}", header);
            // Further parsing would continue here with `_remaining`
        }
        Err(e) => {
            eprintln!("Failed to parse NITF file: {:?}", e);
        }
    }
}