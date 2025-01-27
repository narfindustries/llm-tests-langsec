use nom::{
    bytes::complete::{take, take_until},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct ZipFileHeader {
    signature: u32,
    version_needed: u16,
    flags: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
}

#[derive(Debug)]
struct CentralDirectoryFileHeader {
    signature: u32,
    version_made_by: u16,
    version_needed: u16,
    flags: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    relative_offset_of_local_header: u32,
    file_name: Vec<u8>,
    extra_field: Vec<u8>,
    file_comment: Vec<u8>,
}

#[derive(Debug)]
struct EndOfCentralDirectoryRecord {
    signature: u32,
    number_of_this_disk: u16,
    number_of_disk_start: u16,
    total_entries_this_disk: u16,
    total_entries: u16,
    size_of_central_directory: u32,
    offset_of_start_central_directory: u32,
    comment_length: u16,
    comment: Vec<u8>,
}

fn parse_zip_header(input: &[u8]) -> IResult<&[u8], ZipFileHeader> {
    let (
        input,
        (
            signature,
            version_needed,
            flags,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            file_name,
            extra_field,
        ),
    ) = tuple((
        le_u32,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u32,
        le_u16,
        le_u16,
        map_res(le_u16, move |len: u16| take(len as usize)),
        map_res(le_u16, move |len: u16| take(len as usize)),
    ))(input)?;
    Ok((
        input,
        ZipFileHeader {
            signature,
            version_needed,
            flags,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            file_name: file_name(file_name_length).to_vec(),
            extra_field: extra_field(extra_field_length).to_vec(),
        },
    ))
}

fn parse_central_directory_header(input: &[u8]) -> IResult<&[u8], CentralDirectoryFileHeader> {
    let (
        input,
        (
            signature,
            version_made_by,
            version_needed,
            flags,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            comment_length,
            disk_number_start,
            internal_file_attributes,
            external_file_attributes,
            relative_offset_of_local_header,
            file_name,
            extra_field,
            file_comment,
        ),
    ) = tuple((
        le_u32,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u32,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        map_res(le_u16, move |len: u16| take(len as usize)),
        map_res(le_u16, move |len: u16| take(len as usize)),
        map_res(le_u16, move |len: u16| take(len as usize)),
    ))(input)?;
    Ok((
        input,
        CentralDirectoryFileHeader {
            signature,
            version_made_by,
            version_needed,
            flags,
            compression_method,
            last_mod_time,
            last_mod_date,
            crc32,
            compressed_size,
            uncompressed_size,
            file_name_length,
            extra_field_length,
            comment_length,
            disk_number_start,
            internal_file_attributes,
            external_file_attributes,
            relative_offset_of_local_header,
            file_name: file_name(file_name_length).to_vec(),
            extra_field: extra_field(extra_field_length).to_vec(),
            file_comment: file_comment(comment_length).to_vec(),
        },
    ))
}

fn parse_end_of_central_directory_record(input: &[u8]) -> IResult<&[u8], EndOfCentralDirectoryRecord> {
    let (
        input,
        (
            signature,
            number_of_this_disk,
            number_of_disk_start,
            total_entries_this_disk,
            total_entries,
            size_of_central_directory,
            offset_of_start_central_directory,
            comment_length,
            comment,
        ),
    ) = tuple((
        le_u32,
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u32,
        le_u32,
        le_u16,
        map_res(le_u16, move |len: u16| take(len as usize)),
    ))(input)?;
    Ok((
        input,
        EndOfCentralDirectoryRecord {
            signature,
            number_of_this_disk,
            number_of_disk_start,
            total_entries_this_disk,
            total_entries,
            size_of_central_directory,
            offset_of_start_central_directory,
            comment_length,
            comment: comment(comment_length).to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <zip_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(error) => {
            eprintln!("Failed to open file {}: {}", path.display(), error);
            return;
        },
    };

    let mut buffer = Vec::new();
    if file.read_to_end(&mut buffer).is_err() {
        eprintln!("Failed to read file {}", path.display());
        return;
    }

    match parse_zip_header(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(error) => eprintln!("Error parsing ZIP header: {:?}", error),
    };
}