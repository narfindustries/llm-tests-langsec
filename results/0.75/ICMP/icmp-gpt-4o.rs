use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::env;
use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{be_u8, be_u16, be_u32},
    combinator::map,
    sequence::tuple,
};

#[derive(Debug)]
struct ICMP {
    typ: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
    data: Vec<u8>,
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], ICMP> {
    let (input, (typ, code, checksum, rest_of_header)) = tuple((
        be_u8,
        be_u8,
        be_u16,
        be_u32,
    ))(input)?;

    let (input, data) = map(take(input.len()), |d: &[u8]| d.to_vec())(input)?;

    Ok((input, ICMP {
        typ,
        code,
        checksum,
        rest_of_header,
        data,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
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

    match parse_icmp(&buffer) {
        Ok((_, icmp)) => println!("{:?}", icmp),
        Err(err) => eprintln!("Error parsing ICMP packet: {:?}", err),
    }
}