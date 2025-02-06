JPEGFile ::= STRUCT {
    soi SOI,
    segments SEQUENCE OF Segment,
    eoi EOI
}

SOI ::= 16#FFD8#

EOI ::= 16#FFD9#

Segment ::= CHOICE {
    app0 APP0,
    app1 APP1,
    app2 APP2,
    app3 APP3,
    app4 APP4,
    app5 APP5,
    app6 APP6,
    app7 APP7,
    app8 APP8,
    app9 APP9,
    app10 APP10,
    app11 APP11,
    app12 APP12,
    app13 APP13,
    app14 APP14,
    app15 APP15,
    com COM,
    dqt DQT,
    sof0 SOF0,
    sof2 SOF2,
    dht DHT,
    sos SOS
}

APP0 ::= STRUCT {
    length UINT16,
    data OCTET STRING (SIZE(length - 2))
}

APP1 ::= APP0
APP2 ::= APP0
APP3 ::= APP0
APP4 ::= APP0
APP5 ::= APP0
APP6 ::= APP0
APP7 ::= APP0
APP8 ::= APP0
APP9 ::= APP0
APP10 ::= APP0
APP11 ::= APP0
APP12 ::= APP0
APP13 ::= APP0
APP14 ::= APP0
APP15 ::= APP0

COM ::= STRUCT {
    length UINT16,
    comment OCTET STRING (SIZE(length - 2))
}

DQT ::= STRUCT {
    length UINT16,
    tables SEQUENCE OF QuantizationTable (SIZE(length - 2))
}

QuantizationTable ::= STRUCT {
    info UINT8,
    table OCTET STRING (SIZE((info & 16#F0#) ? 128 : 64))
}

SOF0 ::= STRUCT {
    length UINT16,
    precision UINT8,
    height UINT16,
    width UINT16,
    num_components UINT8,
    components SEQUENCE OF FrameComponent (SIZE(num_components))
}

SOF2 ::= SOF0

FrameComponent ::= STRUCT {
    component_id UINT8,
    sampling_factors UINT8,
    quant_table_id UINT8
}

DHT ::= STRUCT {
    length UINT16,
    tables SEQUENCE OF HuffmanTable (SIZE(length - 2))
}

HuffmanTable ::= STRUCT {
    info UINT8,
    lengths OCTET STRING (SIZE(16)),
    values OCTET STRING (SIZE(SUM(lengths)))
}

SOS ::= STRUCT {
    length UINT16,
    num_components UINT8,
    components SEQUENCE OF ScanComponent (SIZE(num_components)),
    start_spectral UINT8,
    end_spectral UINT8,
    approx_high UINT8,
    approx_low UINT8
}

ScanComponent ::= STRUCT {
    component_id UINT8,
    huffman_table UINT8
}