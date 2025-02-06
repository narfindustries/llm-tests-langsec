use nom::{
    bytes::complete::take,
    combinator::map_res,
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct NITFFile {
    fhdr: String,
    clevel: String,
    stype: String,
    ostaid: String,
    fdt: String,
    ftitle: String,
    fsclas: String,
    // Add more fields as needed
}

fn parse_fixed_string(input: &[u8], length: usize) -> IResult<&[u8], String> {
    map_res(take(length), |s: &[u8]| std::str::from_utf8(s).map(String::from))(input)
}

fn parse_nitf_file(input: &[u8]) -> IResult<&[u8], NITFFile> {
    let (input, (fhdr, clevel, stype, ostaid, fdt, ftitle, fsclas)) = tuple((
        |i| parse_fixed_string(i, 9),  // FHDR
        |i| parse_fixed_string(i, 2),  // CLEVEL
        |i| parse_fixed_string(i, 4),  // STYPE
        |i| parse_fixed_string(i, 10), // OSTAID
        |i| parse_fixed_string(i, 14), // FDT
        |i| parse_fixed_string(i, 80), // FTITLE
        |i| parse_fixed_string(i, 1),  // FSCLAS
        // Parse more fields as needed
    ))(input)?;

    Ok((
        input,
        NITFFile {
            fhdr,
            clevel,
            stype,
            ostaid,
            fdt,
            ftitle,
            fsclas,
            // Initialize more fields as needed
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf-file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_nitf_file(&buffer) {
        Ok((_, nitf)) => println!("{:?}", nitf),
        Err(e) => eprintln!("Failed to parse NITF file: {:?}", e),
    }
}