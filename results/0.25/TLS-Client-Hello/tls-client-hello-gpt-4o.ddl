module TLSClientHello

type TLSClientHello = struct {
    handshake_type: u8,
    length: u24,
    version: u16,
    random: Random,
    session_id: SessionID,
    cipher_suites: CipherSuites,
    compression_methods: CompressionMethods,
    extensions: Extensions
}

type Random = struct {
    gmt_unix_time: u32,
    random_bytes: u8[28]
}

type SessionID = struct {
    session_id_length: u8,
    session_id: u8[session_id_length]
}

type CipherSuites = struct {
    cipher_suites_length: u16,
    cipher_suites: u16[cipher_suites_length / 2]
}

type CompressionMethods = struct {
    compression_methods_length: u8,
    compression_methods: u8[compression_methods_length]
}

type Extensions = struct {
    extensions_length: u16,
    extensions: ExtensionList
}

type ExtensionList = array {
    until: eos,
    element: Extension
}

type Extension = struct {
    extension_type: u16,
    extension_data: ExtensionData
}

type ExtensionData = struct {
    extension_data_length: u16,
    extension_data: u8[extension_data_length]
}