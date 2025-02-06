use nom::{
    bytes::complete::{take},
    combinator::{map},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum TransferSyntax {
    ImplicitLittleEndian,
    ExplicitLittleEndian,
    ExplicitBigEndian,
    // Add more transfer syntaxes as needed
}

#[derive(Debug)]
struct FileMetaInformation {
    transfer_syntax_uid: String,
    media_storage_sop_instance_uid: String,
    implementation_class_uid: String,
    transfer_syntax: TransferSyntax,
}

fn parse_file_meta_information(input: &[u8]) -> IResult<&[u8], FileMetaInformation> {
    let (input, transfer_syntax_uid) = take(64u8)(input)?;
    let (input, media_storage_sop_instance_uid) = take(64u8)(input)?;
    let (input, implementation_class_uid) = take(64u8)(input)?;
    let (input, transfer_syntax) = map(be_u32, |x| match x {
        1 => TransferSyntax::ImplicitLittleEndian,
        2 => TransferSyntax::ExplicitLittleEndian,
        3 => TransferSyntax::ExplicitBigEndian,
        _ => panic!("Unknown transfer syntax"),
    })(input)?;
    Ok((
        input,
        FileMetaInformation {
            transfer_syntax_uid: String::from_utf8_lossy(transfer_syntax_uid).into_owned(),
            media_storage_sop_instance_uid: String::from_utf8_lossy(media_storage_sop_instance_uid)
                .into_owned(),
            implementation_class_uid: String::from_utf8_lossy(implementation_class_uid)
                .into_owned(),
            transfer_syntax,
        },
    ))
}

#[derive(Debug)]
struct PatientInformation {
    patient_id: String,
    patient_name: String,
    patient_birth_date: String,
    patient_sex: String,
    patient_age: u16,
}

fn parse_patient_information(input: &[u8]) -> IResult<&[u8], PatientInformation> {
    let (input, patient_id) = take(64u8)(input)?;
    let (input, patient_name) = take(64u8)(input)?;
    let (input, patient_birth_date) = take(8u8)(input)?;
    let (input, patient_sex) = take(1u8)(input)?;
    let (input, patient_age) = be_u16(input)?;
    Ok((
        input,
        PatientInformation {
            patient_id: String::from_utf8_lossy(patient_id).into_owned(),
            patient_name: String::from_utf8_lossy(patient_name).into_owned(),
            patient_birth_date: String::from_utf8_lossy(patient_birth_date).into_owned(),
            patient_sex: String::from_utf8_lossy(patient_sex).into_owned(),
            patient_age,
        },
    ))
}

#[derive(Debug)]
struct StudyInformation {
    study_instance_uid: String,
    study_date: String,
    study_time: String,
    study_description: String,
    modality: String,
}

fn parse_study_information(input: &[u8]) -> IResult<&[u8], StudyInformation> {
    let (input, study_instance_uid) = take(64u8)(input)?;
    let (input, study_date) = take(8u8)(input)?;
    let (input, study_time) = take(6u8)(input)?;
    let (input, study_description) = take(64u8)(input)?;
    let (input, modality) = take(2u8)(input)?;
    Ok((
        input,
        StudyInformation {
            study_instance_uid: String::from_utf8_lossy(study_instance_uid).into_owned(),
            study_date: String::from_utf8_lossy(study_date).into_owned(),
            study_time: String::from_utf8_lossy(study_time).into_owned(),
            study_description: String::from_utf8_lossy(study_description).into_owned(),
            modality: String::from_utf8_lossy(modality).into_owned(),
        },
    ))
}

#[derive(Debug)]
struct SeriesInformation {
    series_instance_uid: String,
    series_number: u16,
    series_description: String,
    series_date: String,
    series_time: String,
}

fn parse_series_information(input: &[u8]) -> IResult<&[u8], SeriesInformation> {
    let (input, series_instance_uid) = take(64u8)(input)?;
    let (input, series_number) = be_u16(input)?;
    let (input, series_description) = take(64u8)(input)?;
    let (input, series_date) = take(8u8)(input)?;
    let (input, series_time) = take(6u8)(input)?;
    Ok((
        input,
        SeriesInformation {
            series_instance_uid: String::from_utf8_lossy(series_instance_uid).into_owned(),
            series_number,
            series_description: String::from_utf8_lossy(series_description).into_owned(),
            series_date: String::from_utf8_lossy(series_date).into_owned(),
            series_time: String::from_utf8_lossy(series_time).into_owned(),
        },
    ))
}

#[derive(Debug)]
struct ImageInformation {
    sop_instance_uid: String,
    image_type: String,
    image_orientation: String,
    image_position: String,
    pixel_data: Vec<u8>,
}

fn parse_image_information(input: &[u8]) -> IResult<&[u8], ImageInformation> {
    let (input, sop_instance_uid) = take(64u8)(input)?;
    let (input, image_type) = take(2u8)(input)?;
    let (input, image_orientation) = take(6u8)(input)?;
    let (input, image_position) = take(12u8)(input)?;
    let (input, pixel_data) = take(usize)(input)?;
    Ok((
        input,
        ImageInformation {
            sop_instance_uid: String::from_utf8_lossy(sop_instance_uid).into_owned(),
            image_type: String::from_utf8_lossy(image_type).into_owned(),
            image_orientation: String::from_utf8_lossy(image_orientation).into_owned(),
            image_position: String::from_utf8_lossy(image_position).into_owned(),
            pixel_data: pixel_data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut input = Vec::new();
    file.read_to_end(&mut input).unwrap();
    let (input, file_meta_information) = parse_file_meta_information(&input).unwrap();
    let (input, patient_information) = parse_patient_information(input).unwrap();
    let (input, study_information) = parse_study_information(input).unwrap();
    let (input, series_information) = parse_series_information(input).unwrap();
    let (_input, image_information) = parse_image_information(input).unwrap();
    println!(
        "File Meta Information: {:?}",
        file_meta_information
    );
    println!("Patient Information: {:?}", patient_information);
    println!("Study Information: {:?}", study_information);
    println!("Series Information: {:?}", series_information);
    println!("Image Information: {:?}", image_information);
}