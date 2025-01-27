module Network-Time-Protocol-Version-4

type NTPPacket = {
    leap_indicator: bit[2],
    version_number: bit[3],
    mode: bit[3],
    stratum: bit[8],
    poll_interval: bit[8],
    precision: bit[8],
    root_delay: bit[32],
    root_dispersion: bit[32],
    reference_id: bit[32],
    reference_timestamp: bit[64],
    originate_timestamp: bit[64],
    receive_timestamp: bit[64],
    transmit_timestamp: bit[64]
}

parse ntp_packet = {
    leap_indicator = take[2],
    version_number = take[3],
    mode = take[3],
    stratum = take[8],
    poll_interval = take[8],
    precision = take[8],
    root_delay = take[32],
    root_dispersion = take[32],
    reference_id = take[32],
    reference_timestamp = take[64],
    originate_timestamp = take[64],
    receive_timestamp = take[64],
    transmit_timestamp = take[64]
} as NTPPacket

main = ntp_packet