NetworkTimeProtocolV4 :: {
    li: uimsbf[2];
    vn: uimsbf[3];
    mode: uimsbf[3];
    stratum: uimsbf[8];
    poll: uimsbf[8];
    precision: uimsbf[8];
    root_delay: uimsbf[32];
    root_dispersion: uimsbf[32];
    reference_identifier: uimsbf[32];
    reference_timestamp: uimsbf[64];
    originate_timestamp: uimsbf[64];
    receive_timestamp: uimsbf[64];
    transmit_timestamp: uimsbf[64];
    authenticator: {
        key_identifier: uimsbf[32];
        message_digest: uimsbf[64];
    }?
}