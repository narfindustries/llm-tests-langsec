TLSClientHello {
    protocol_version: UInt8,
    random: Bytes[32],
    session_id: UInt8,
    session_id_length: UInt8,
    cipher_suites: UInt16[],
    cipher_suites_length: UInt16,
    compression_methods: UInt8[],
    compression_methods_length: UInt8,
    extensions: Extension[],
    extensions_length: UInt16
}

Extension {
    extension_type: UInt16,
    extension_length: UInt16,
    extension_data: Bytes[extension_length]
}