use nom::{
    bytes::complete::{tag, take},
    combinator::{map_res, opt},
    number::complete::{be_u16, be_u32, be_u64},
    IResult,
};
use std::{env, fs::File, io::Read, str};

struct NitfHeader {
    file_profile_name: String,
    file_version: String,
    complexity_level: String,
    system_type: String,
    origin_station_id: String,
    file_date_time: String,
    file_title: String,
    file_classification: String,
    message_copy_num: Option<String>,
    message_num_copies: Option<String>,
}

struct Nitf {
    header: NitfHeader,
}

impl Nitf {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, header) = Nitf::parse_header(input)?;
        Ok((input, Nitf { header }))
    }

    fn parse_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
        let (input, file_profile_name) = map_res(take(4usize), str::from_utf8)(input)?;
        let (input, file_version) = map_res(take(5usize), str::from_utf8)(input)?;
        let (input, complexity_level) = map_res(take(2usize), str::from_utf8)(input)?;
        let (input, system_type) = map_res(take(4usize), str::from_utf8)(input)?;
        let (input, origin_station_id) = map_res(take(10usize), str::from_utf8)(input)?;
        let (input, file_date_time) = map_res(take(14usize), str::from_utf8)(input)?;
        let (input, file_title) = map_res(take(80usize), str::from_utf8)(input)?;
        let (input, file_classification) = map_res(take(2usize), str::from_utf8)(input)?;
        let (input, message_copy_num) = opt(map_res(take(5usize), str::from_utf8))(input)?;
        let (input, message_num_copies) = opt(map_res(take(5usize), str::from_utf8))(input)?;

        Ok((
            input,
            NitfHeader {
                file_profile_name: file_profile_name.to_string(),
                file_version: file_version.to_string(),
                complexity_level: complexity_level.to_string(),
                system_type: system_type.to_string(),
                origin_station_id: origin_station_id.to_string(),
                file_date_time: file_date_time.to_string(),
                file_title: file_title.to_string(),
                file_classification: file_classification.to_string(),
                message_copy_num: message_copy_num.map(|s| s.to_string()),
                message_num_copies: message_num_copies.map(|s| s.to_string()),
            },
        ))
    }
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match Nitf::parse(&buffer) {
        Ok((_, nitf)) => {
            println!("Parsed NITF successfully!");
            println!("File Profile Name: {}", nitf.header.file_profile_name);
            println!("File Version: {}", nitf.header.file_version);
            println!("Complexity Level: {}", nitf.header.complexity_level);
            println!("System Type: {}", nitf.header.system_type);
            println!("Origin Station ID: {}", nitf.header.origin_station_id);
            println!("File Date and Time: {}", nitf.header.file_date_time);
            println!("File Title: {}", nitf.header.file_title);
            println!("File Classification: {}", nitf.header.file_classification);
            if let Some(copy_num) = nitf.header.message_copy_num {
                println!("Message Copy Number: {}", copy_num);
            }
            if let Some(num_copies) = nitf.header.message_num_copies {
                println!("Message Number of Copies: {}", num_copies);
            }
        }
        Err(e) => println!("Error parsing NITF: {:?}", e),
    }

    Ok(())
}