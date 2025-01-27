use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data, take_till},
    number::complete::{be_u16, be_u24, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum CipherSuite {
    TLS_NULL_WITH_NULL_NULL,
    TLS_RSA_WITH_NULL_MD5,
    TLS_RSA_WITH_NULL_SHA,
    // ... add other cipher suites
}

#[derive(Debug, PartialEq)]
enum CompressionMethod {
    NULL,
    // ... add other compression methods
}

#[derive(Debug, PartialEq)]
enum ExtensionType {
    SERVER_NAME,
    MAX_FRAGMENT_LENGTH,
    CLIENT_CERTIFICATE_URL,
    TRUSTED_CA_KEYS,
    TRUNCATED_HMAC,
    STATUS_REQUEST,
    // ... add other extension types
}

#[derive(Debug, PartialEq)]
struct ServerName {
    name_type: u8,
    host_name: String,
}

#[derive(Debug, PartialEq)]
struct MaxFragmentLength {
    max_fragment_length: u8,
}

#[derive(Debug, PartialEq)]
struct ClientCertificateURL {
    url_and_hash_id_list: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct TrustedCAKeys {
    trusted_ca_keys: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct TruncatedHMAC {
    truncated_hmac: u8,
}

#[derive(Debug, PartialEq)]
struct StatusRequest {
    status_type: u8,
    responder_id_list: Vec<u8>,
    request_extensions: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct Extension {
    extension_type: ExtensionType,
    extension_data: Vec<u8>,
}

fn cipher_suite(i: &[u8]) -> IResult<&[u8], CipherSuite> {
    map(be_u16, |x| match x {
        0x0000 => CipherSuite::TLS_NULL_WITH_NULL_NULL,
        0x0001 => CipherSuite::TLS_RSA_WITH_NULL_MD5,
        0x0002 => CipherSuite::TLS_RSA_WITH_NULL_SHA,
        // ... add other cipher suites
        _ => unreachable!(),
    })(i)
}

fn compression_method(i: &[u8]) -> IResult<&[u8], CompressionMethod> {
    map(be_u8, |x| match x {
        0x00 => CompressionMethod::NULL,
        // ... add other compression methods
        _ => unreachable!(),
    })(i)
}

fn extension_type(i: &[u8]) -> IResult<&[u8], ExtensionType> {
    map(be_u16, |x| match x {
        0x0000 => ExtensionType::SERVER_NAME,
        0x0001 => ExtensionType::MAX_FRAGMENT_LENGTH,
        0x0002 => ExtensionType::CLIENT_CERTIFICATE_URL,
        0x0003 => ExtensionType::TRUSTED_CA_KEYS,
        0x0004 => ExtensionType::TRUNCATED_HMAC,
        0x0005 => ExtensionType::STATUS_REQUEST,
        // ... add other extension types
        _ => unreachable!(),
    })(i)
}

fn server_name(i: &[u8]) -> IResult<&[u8], ServerName> {
    map(
        tuple((be_u8, length_data(be_u16))),
        |(name_type, host_name)| ServerName { name_type, host_name: String::from_utf8_lossy(host_name).into_owned() },
    )(i)
}

fn max_fragment_length(i: &[u8]) -> IResult<&[u8], MaxFragmentLength> {
    map(be_u8, |x| MaxFragmentLength { max_fragment_length: x })(i)
}

fn client_certificate_url(i: &[u8]) -> IResult<&[u8], ClientCertificateURL> {
    map(length_data(be_u16), |x| ClientCertificateURL { url_and_hash_id_list: x.to_vec() })(i)
}

fn trusted_ca_keys(i: &[u8]) -> IResult<&[u8], TrustedCAKeys> {
    map(length_data(be_u16), |x| TrustedCAKeys { trusted_ca_keys: x.to_vec() })(i)
}

fn truncated_hmac(i: &[u8]) -> IResult<&[u8], TruncatedHMAC> {
    map(be_u8, |x| TruncatedHMAC { truncated_hmac: x })(i)
}

fn status_request(i: &[u8]) -> IResult<&[u8], StatusRequest> {
    map(
        tuple((be_u8, length_data(be_u16), length_data(be_u16))),
        |(status_type, responder_id_list, request_extensions)| StatusRequest {
            status_type,
            responder_id_list: responder_id_list.to_vec(),
            request_extensions: request_extensions.to_vec(),
        },
    )(i)
}

fn extension(i: &[u8]) -> IResult<&[u8], Extension> {
    map(
        tuple((extension_type, length_data(be_u16))),
        |(extension_type, extension_data)| match extension_type {
            ExtensionType::SERVER_NAME => {
                let (_, server_name) = server_name(extension_data)?;
                Extension {
                    extension_type,
                    extension_data: server_name.host_name.as_bytes().to_vec(),
                }
            }
            ExtensionType::MAX_FRAGMENT_LENGTH => {
                let (_, max_fragment_length) = max_fragment_length(extension_data)?;
                Extension {
                    extension_type,
                    extension_data: vec![max_fragment_length.max_fragment_length],
                }
            }
            ExtensionType::CLIENT_CERTIFICATE_URL => {
                let (_, client_certificate_url) = client_certificate_url(extension_data)?;
                Extension {
                    extension_type,
                    extension_data: client_certificate_url.url_and_hash_id_list,
                }
            }
            ExtensionType::TRUSTED_CA_KEYS => {
                let (_, trusted_ca_keys) = trusted_ca_keys(extension_data)?;
                Extension {
                    extension_type,
                    extension_data: trusted_ca_keys.trusted_ca_keys,
                }
            }
            ExtensionType::TRUNCATED_HMAC => {
                let (_, truncated_hmac) = truncated_hmac(extension_data)?;
                Extension {
                    extension_type,
                    extension_data: vec![truncated_hmac.truncated_hmac],
                }
            }
            ExtensionType::STATUS_REQUEST => {
                let (_, status_request) = status_request(extension_data)?;
                Extension {
                    extension_type,
                    extension_data: status_request
                        .responder_id_list
                        .iter()
                        .chain(status_request.request_extensions.iter())
                        .copied()
                        .collect(),
                }
            }
        },
    )(i)
}

fn client_hello(i: &[u8]) -> IResult<&[u8], (&[u8], Vec<CipherSuite>, Vec<CompressionMethod>, Vec<Extension>)> {
    map(
        tuple((
            tag([0x03, 0x01]), // record layer
            be_u16, // record length
            tag([0x01]), // handshake message type
            be_u24, // handshake message length
            be_u16, // protocol version
            take(32), // random
            be_u8, // session id length
            length_data(be_u8), // session id
            length_data(be_u16), // cipher suite length
            map(length_data(be_u16), |cipher_suites| {
                cipher_suites
                    .chunks(2)
                    .map(|x| cipher_suite(x).unwrap().1)
                    .collect()
            }), // cipher suites
            length_data(be_u8), // compression methods length
            map(length_data(be_u8), |compression_methods| {
                compression_methods
                    .iter()
                    .map(|x| compression_method(std::slice::from_ref(x)).unwrap().1)
                    .collect()
            }), // compression methods
            opt(length_data(be_u16)), // extensions length
        )),
        |(
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            _,
            cipher_suites,
            compression_methods,
            extensions_length,
        )| {
            (
                extensions_length.unwrap_or_default().as_slice(),
                cipher_suites,
                compression_methods,
                extensions_length
                    .unwrap_or_default()
                    .chunks(4)
                    .map(|x| extension(x).unwrap().1)
                    .collect(),
            )
        },
    )(i)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }

    let mut file = File::open(&args[1]).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();

    let (_, (_, cipher_suites, compression_methods, extensions)) = client_hello(&data).unwrap();

    println!("Cipher Suites: {:?}", cipher_suites);
    println!("Compression Methods: {:?}", compression_methods);
    println!("Extensions: {:?}", extensions);
}