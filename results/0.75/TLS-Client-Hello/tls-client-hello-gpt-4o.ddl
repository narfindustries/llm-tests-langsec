protocol TLSClientHello {
    uint16 version;
    opaque random[32];
    opaque session_id<0..32>;
    CipherSuite cipher_suites<2..2^16-2>;
    opaque compression_methods<1..2^8-1>;
    select (version) {
        case {0x0301, 0x0302, 0x0303}:
            Extensions extensions<0..2^16-1>;
    }
}

record CipherSuite {
    uint16 suite_code;
}

record Extensions {
    uint16 length;
    Extension extensions[length / 2];
}

record Extension {
    uint16 type;
    opaque data<0..2^16-1>;
}