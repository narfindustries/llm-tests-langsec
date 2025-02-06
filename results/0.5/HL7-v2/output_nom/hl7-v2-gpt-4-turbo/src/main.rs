use nom::{
    bytes::complete::{tag, take_until},
    character::complete::{alphanumeric1, char},
    combinator::map_res,
    multi::separated_list0,
    sequence::{preceded, tuple},
    IResult,
};
use std::{env, fs::File, io::Read, str};

#[derive(Debug)]
struct HL7Message {
    segments: Vec<Segment>,
}

#[derive(Debug)]
enum Segment {
    MSH(MSHSegment),
    PID(PIDSegment),
    OBR(OBRSegment),
    OBX(OBXSegment),
}

#[derive(Debug)]
struct MSHSegment {
    sending_application: String,
    sending_facility: String,
    receiving_application: String,
    receiving_facility: String,
    date_time_of_message: String,
    message_type: String,
    message_control_id: String,
    processing_id: String,
    version_id: String,
}

#[derive(Debug)]
struct PIDSegment {
    patient_id: String,
    patient_name: String,
}

#[derive(Debug)]
struct OBRSegment {
    set_id: String,
    placer_order_number: String,
    filler_order_number: String,
}

#[derive(Debug)]
struct OBXSegment {
    set_id: String,
    value_type: String,
    observation_identifier: String,
    observation_value: String,
}

fn parse_msh_segment(input: &str) -> IResult<&str, MSHSegment> {
    map_res(
        preceded(
            tag("MSH"),
            tuple((
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
            )),
        ),
        |(
            sending_application,
            sending_facility,
            receiving_application,
            receiving_facility,
            date_time_of_message,
            message_type,
            message_control_id,
            processing_id,
            version_id,
        )| {
            Ok(MSHSegment {
                sending_application: sending_application.to_string(),
                sending_facility: sending_facility.to_string(),
                receiving_application: receiving_application.to_string(),
                receiving_facility: receiving_facility.to_string(),
                date_time_of_message: date_time_of_message.to_string(),
                message_type: message_type.to_string(),
                message_control_id: message_control_id.to_string(),
                processing_id: processing_id.to_string(),
                version_id: version_id.to_string(),
            })
        },
    )(input)
}

fn parse_pid_segment(input: &str) -> IResult<&str, PIDSegment> {
    map_res(
        preceded(
            tag("PID"),
            tuple((
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
            )),
        ),
        |(patient_id, patient_name)| {
            Ok(PIDSegment {
                patient_id: patient_id.to_string(),
                patient_name: patient_name.to_string(),
            })
        },
    )(input)
}

fn parse_obr_segment(input: &str) -> IResult<&str, OBRSegment> {
    map_res(
        preceded(
            tag("OBR"),
            tuple((
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
            )),
        ),
        |(set_id, placer_order_number, filler_order_number)| {
            Ok(OBRSegment {
                set_id: set_id.to_string(),
                placer_order_number: placer_order_number.to_string(),
                filler_order_number: filler_order_number.to_string(),
            })
        },
    )(input)
}

fn parse_obx_segment(input: &str) -> IResult<&str, OBXSegment> {
    map_res(
        preceded(
            tag("OBX"),
            tuple((
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
                preceded(char('|'), alphanumeric1),
            )),
        ),
        |(set_id, value_type, observation_identifier, observation_value)| {
            Ok(OBXSegment {
                set_id: set_id.to_string(),
                value_type: value_type.to_string(),
                observation_identifier: observation_identifier.to_string(),
                observation_value: observation_value.to_string(),
            })
        },
    )(input)
}

fn parse_segments(input: &str) -> IResult<&str, Vec<Segment>> {
    separated_list0(char('\r'), parse_segment)(input)
}

fn parse_segment(input: &str) -> IResult<&str, Segment> {
    let (input, segment_id) = take_until("|")(input)?;
    match segment_id {
        "MSH" => map_res(parse_msh_segment, |s| Ok(Segment::MSH(s)))(input),
        "PID" => map_res(parse_pid_segment, |s| Ok(Segment::PID(s)))(input),
        "OBR" => map_res(parse_obr_segment, |s| Ok(Segment::OBR(s)))(input),
        "OBX" => map_res(parse_obx_segment, |s| Ok(Segment::OBX(s)))(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::NoneOf))),
    }
}

fn parse_hl7_message(input: &str) -> IResult<&str, HL7Message> {
    map_res(parse_segments, |segments| Ok(HL7Message { segments }))(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err("Usage: hl7_parser <file>".into());
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;
    let input = str::from_utf8(&contents)?;

    match parse_hl7_message(input) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => println!("Failed to parse HL7 message: {:?}", e),
    }

    Ok(())
}