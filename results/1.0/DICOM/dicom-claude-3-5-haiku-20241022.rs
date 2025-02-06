use nom::{
    combinator::{map, opt},
    multi::{many1},
    number::complete::{le_u16},
    sequence::{tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct PatientModule {
    patient_name: Option<String>,
    patient_id: Option<String>,
    patient_birthdate: Option<String>,
    patient_sex: Option<String>,
}

#[derive(Debug)]
struct StudyModule {
    study_instance_uid: Option<String>,
    study_date: Option<String>,
    study_time: Option<String>,
    accession_number: Option<String>,
    referring_physician_name: Option<String>,
}

#[derive(Debug)]
struct SeriesModule {
    modality: Option<String>,
    series_description: Option<String>,
    series_number: Option<u16>,
    series_instance_uid: Option<String>,
}

#[derive(Debug)]
struct ImageModule {
    sop_class_uid: Option<String>,
    image_type: Option<String>,
    pixel_data: Vec<u8>,
    bits_allocated: Option<u16>,
    bits_stored: Option<u16>,
    high_bit: Option<u16>,
    photometric_interpretation: Option<String>,
}

#[derive(Debug)]
struct DICOMImage {
    patient: PatientModule,
    study: StudyModule,
    series: SeriesModule,
    image: ImageModule,
}

fn parse_string(input: &[u8]) -> IResult<&[u8], Option<String>> {
    opt(map(nom::bytes::complete::take_while(|c| c != 0), |bytes: &[u8]| String::from_utf8_lossy(bytes).to_string()))(input)
}

fn parse_patient_module(input: &[u8]) -> IResult<&[u8], PatientModule> {
    map(
        tuple((
            parse_string,
            parse_string,
            parse_string,
            parse_string
        )),
        |(patient_name, patient_id, patient_birthdate, patient_sex)| PatientModule {
            patient_name,
            patient_id,
            patient_birthdate,
            patient_sex,
        }
    )(input)
}

fn parse_study_module(input: &[u8]) -> IResult<&[u8], StudyModule> {
    map(
        tuple((
            parse_string,
            parse_string,
            parse_string,
            parse_string,
            parse_string
        )),
        |(study_instance_uid, study_date, study_time, accession_number, referring_physician_name)| StudyModule {
            study_instance_uid,
            study_date,
            study_time,
            accession_number,
            referring_physician_name,
        }
    )(input)
}

fn parse_series_module(input: &[u8]) -> IResult<&[u8], SeriesModule> {
    map(
        tuple((
            parse_string,
            parse_string,
            opt(le_u16),
            parse_string
        )),
        |(modality, series_description, series_number, series_instance_uid)| SeriesModule {
            modality,
            series_description,
            series_number,
            series_instance_uid,
        }
    )(input)
}

fn parse_image_module(input: &[u8]) -> IResult<&[u8], ImageModule> {
    map(
        tuple((
            parse_string,
            parse_string,
            many1(nom::bytes::complete::take(1usize)),
            opt(le_u16),
            opt(le_u16),
            opt(le_u16),
            parse_string
        )),
        |(sop_class_uid, image_type, pixel_data, bits_allocated, bits_stored, high_bit, photometric_interpretation)| ImageModule {
            sop_class_uid,
            image_type,
            pixel_data: pixel_data.into_iter().flatten().cloned().collect(),
            bits_allocated,
            bits_stored,
            high_bit,
            photometric_interpretation,
        }
    )(input)
}

fn parse_dicom_image(input: &[u8]) -> IResult<&[u8], DICOMImage> {
    map(
        tuple((
            parse_patient_module,
            parse_study_module,
            parse_series_module,
            parse_image_module
        )),
        |(patient, study, series, image)| DICOMImage {
            patient,
            study,
            series,
            image,
        }
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dicom_image(&buffer) {
        Ok((_, dicom)) => {
            println!("Parsed DICOM Image: {:?}", dicom);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}