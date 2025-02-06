format NTPv4 {
    leap_indicator : bits(2);
    version_number : bits(3);
    mode : bits(3);
    stratum : u8;
    poll_interval : s8;
    precision : s8;
    root_delay : u32;
    root_dispersion : u32;
    reference_identifier : [u8; 4];
    reference_timestamp : u64;
    origin_timestamp : u64;
    receive_timestamp : u64;
    transmit_timestamp : u64;
}