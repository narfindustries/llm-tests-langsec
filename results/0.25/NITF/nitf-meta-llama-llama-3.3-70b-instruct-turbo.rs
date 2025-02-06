use nom::{
    bytes::complete::{tag, take, take_while_m_n},
    character::complete::{digit1, multispace0},
    combinator::{map, map_res},
    multi::many0,
    number::complete::{be_u32},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fmt,
    fs::File,
    io::{BufReader, Read},
    path::Path,
    str,
};

#[derive(Debug, PartialEq)]
enum SecurityType {
    NF,
    CR,
    RS,
    SI,
    UC,
}

impl fmt::Display for SecurityType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SecurityType::NF => write!(f, "NF"),
            SecurityType::CR => write!(f, "CR"),
            SecurityType::RS => write!(f, "RS"),
            SecurityType::SI => write!(f, "SI"),
            SecurityType::UC => write!(f, "UC"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ClassificationLevel {
    U,
    C,
    S,
    TS,
}

impl fmt::Display for ClassificationLevel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ClassificationLevel::U => write!(f, "U"),
            ClassificationLevel::C => write!(f, "C"),
            ClassificationLevel::S => write!(f, "S"),
            ClassificationLevel::TS => write!(f, "TS"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ImageSource {
    EO,
    IR,
    MS,
    SAR,
    OTHER,
}

impl fmt::Display for ImageSource {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ImageSource::EO => write!(f, "EO"),
            ImageSource::IR => write!(f, "IR"),
            ImageSource::MS => write!(f, "MS"),
            ImageSource::SAR => write!(f, "SAR"),
            ImageSource::OTHER => write!(f, "OTHER"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum PixelValue {
    INT,
    REAL,
    COMPLEX,
}

impl fmt::Display for PixelValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PixelValue::INT => write!(f, "INT"),
            PixelValue::REAL => write!(f, "REAL"),
            PixelValue::COMPLEX => write!(f, "COMPLEX"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ImageRepresentation {
    MONO,
    RGB,
    MULTI,
}

impl fmt::Display for ImageRepresentation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ImageRepresentation::MONO => write!(f, "MONO"),
            ImageRepresentation::RGB => write!(f, "RGB"),
            ImageRepresentation::MULTI => write!(f, "MULTI"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum GraphicType {
    ANNOTATION,
    OVERLAY,
    OTHER,
}

impl fmt::Display for GraphicType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GraphicType::ANNOTATION => write!(f, "ANNOTATION"),
            GraphicType::OVERLAY => write!(f, "OVERLAY"),
            GraphicType::OTHER => write!(f, "OTHER"),
        }
    }
}

#[derive(Debug, PartialEq)]
enum TextType {
    ANNOTATION,
    OVERLAY,
    OTHER,
}

impl fmt::Display for TextType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TextType::ANNOTATION => write!(f, "ANNOTATION"),
            TextType::OVERLAY => write!(f, "OVERLAY"),
            TextType::OTHER => write!(f, "OTHER"),
        }
    }
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], (String, String, ClassificationLevel, SecurityType, String, String, String, String, String)> {
    let (input, _) = tag("NITF02.10")(input)?;
    let (input, _) = tag("02.10")(input)?;
    let (input, clevel) = map_res(take(1usize), |x: &[u8]| match x {
        b"U" => Ok(ClassificationLevel::U),
        b"C" => Ok(ClassificationLevel::C),
        b"S" => Ok(ClassificationLevel::S),
        b"TS" => Ok(ClassificationLevel::TS),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, stype) = map_res(take(2usize), |x: &[u8]| match x {
        b"NF" => Ok(SecurityType::NF),
        b"CR" => Ok(SecurityType::CR),
        b"RS" => Ok(SecurityType::RS),
        b"SI" => Ok(SecurityType::SI),
        b"UC" => Ok(SecurityType::UC),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, osta_id) = map_res(take_while_m_n(1, 25, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, fdt) = map_res(take(8usize), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ftm) = map_res(take(6usize), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ftitle) = map_res(take_while_m_n(1, 80, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, fsclas) = map_res(take(1usize), |x: &[u8]| match x {
        b"U" => Ok(ClassificationLevel::U),
        b"C" => Ok(ClassificationLevel::C),
        b"S" => Ok(ClassificationLevel::S),
        b"TS" => Ok(ClassificationLevel::TS),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, fscode) = map_res(take_while_m_n(1, 10, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, fsgtxt) = map_res(take_while_m_n(1, 20, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    Ok((input, (
        "NITF02.10".to_string(),
        "02.10".to_string(),
        clevel,
        stype,
        osta_id.to_string(),
        fdt.to_string(),
        ftm.to_string(),
        ftitle.to_string(),
        fsclas.to_string(),
    )))
}

fn parse_image_header(input: &[u8]) -> IResult<&[u8], (String, String, u32, u32, ImageSource, u32, u32, PixelValue, ImageRepresentation, String)> {
    let (input, iid1) = map_res(take_while_m_n(1, 25, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, idatim) = map_res(take(14usize), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _itim) = map_res(take(6usize), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, irow) = map_res(be_u32, |x: u32| Ok(x))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, icol) = map_res(be_u32, |x: u32| Ok(x))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, iid2) = map_res(take_while_m_n(1, 25, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, isorce) = map_res(take(2usize), |x: &[u8]| match x {
        b"EO" => Ok(ImageSource::EO),
        b"IR" => Ok(ImageSource::IR),
        b"MS" => Ok(ImageSource::MS),
        b"SAR" => Ok(ImageSource::SAR),
        b"OTHER" => Ok(ImageSource::OTHER),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, nrow) = map_res(be_u32, |x: u32| Ok(x))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ncol) = map_res(be_u32, |x: u32| Ok(x))(input)?;
    let (input, _) = multispace0(input)?;
    let (input, pvtype) = map_res(take(4usize), |x: &[u8]| match x {
        b"INT" => Ok(PixelValue::INT),
        b"REAL" => Ok(PixelValue::REAL),
        b"COMPLEX" => Ok(PixelValue::COMPLEX),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, irepb) = map_res(take(4usize), |x: &[u8]| match x {
        b"MONO" => Ok(ImageRepresentation::MONO),
        b"RGB" => Ok(ImageRepresentation::RGB),
        b"MULTI" => Ok(ImageRepresentation::MULTI),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, iband) = map_res(take_while_m_n(1, 10, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    Ok((input, (
        iid1.to_string(),
        idatim.to_string(),
        nrow,
        ncol,
        isorce,
        irow,
        icol,
        pvtype,
        irepb,
        iband.to_string(),
    )))
}

fn parse_graphic_header(input: &[u8]) -> IResult<&[u8], (String, String, GraphicType, String)> {
    let (input, gid1) = map_res(take_while_m_n(1, 25, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, gdatim) = map_res(take(14usize), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _gtim) = map_res(take(6usize), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, gtype) = map_res(take(9usize), |x: &[u8]| match x {
        b"ANNOTATION" => Ok(GraphicType::ANNOTATION),
        b"OVERLAY" => Ok(GraphicType::OVERLAY),
        b"OTHER" => Ok(GraphicType::OTHER),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, giid2) = map_res(take_while_m_n(1, 25, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    Ok((input, (
        gid1.to_string(),
        gdatim.to_string(),
        gtype,
        giid2.to_string(),
    )))
}

fn parse_text_header(input: &[u8]) -> IResult<&[u8], (String, String, TextType, String)> {
    let (input, tid1) = map_res(take_while_m_n(1, 25, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, tdatim) = map_res(take(14usize), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _ttim) = map_res(take(6usize), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, ttype) = map_res(take(9usize), |x: &[u8]| match x {
        b"ANNOTATION" => Ok(TextType::ANNOTATION),
        b"OVERLAY" => Ok(TextType::OVERLAY),
        b"OTHER" => Ok(TextType::OTHER),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, tiid2) = map_res(take_while_m_n(1, 25, |x| x != b' ' && x != b'\t'), |x: &[u8]| {
        str::from_utf8(x).map_err(|_| nom::Err::Error(nom::error::ErrorKind::AlphaNumeric))
    })(input)?;
    Ok((input, (
        tid1.to_string(),
        tdatim.to_string(),
        ttype,
        tiid2.to_string(),
    )))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, img_data) = nom::bytes::complete::take_while(|x| x != 0)(input)?;
    Ok((input, img_data.to_vec()))
}

fn parse_graphic_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, graphic_data) = nom::bytes::complete::take_while(|x| x != 0)(input)?;
    Ok((input, graphic_data.to_vec()))
}

fn parse_text_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, text_data) = nom::bytes::complete::take_while(|x| x != 0)(input)?;
    Ok((input, text_data.to_vec()))
}

fn parse_trailer(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, _) = tag("NITF02.10")(input)?;
    let (input, _) = tag("02.10")(input)?;
    Ok((input, ()))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let path = Path::new(input_file);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file