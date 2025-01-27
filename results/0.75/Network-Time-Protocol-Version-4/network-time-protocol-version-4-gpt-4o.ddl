network_time_protocol_version_4 {
    header {
        leap_indicator: int(2);
        version_number: int(3);
        mode: int(3);
        stratum: uint8;
        poll_interval: uint8;
        precision: int8;
        root_delay: int32;
        root_dispersion: uint32;
        reference_identifier: string(4);
        reference_timestamp: uint64;
        originate_timestamp: uint64;
        receive_timestamp: uint64;
        transmit_timestamp: uint64;
    }
}