module Network.Time.Protocol.Version4 {
  import Network.Time.Protocol.Common;

  type NTPHeader = struct {
    li           : uint(2);  // Leap Indicator
    vn           : uint(3);  // Version Number
    mode         : uint(3);  // Mode
    stratum      : uint(8);  // Stratum level of the local clock
    poll         : int(8);   // Maximum interval between successive messages
    precision    : int(8);   // Precision of the local clock

    rootDelay    : fixed(32);  // Total round trip delay time
    rootDispersion: fixed(32); // Max error aloud from primary clock source
    refId        : uint(32);   // Reference clock identifier

    refTm_s      : uint(32);   // Reference time-stamp seconds
    refTm_f      : uint(32);   // Reference time-stamp fraction of a second

    origTm_s     : uint(32);   // Originate time-stamp seconds
    origTm_f     : uint(32);   // Originate time-stamp fraction of a second

    rxTm_s       : uint(32);   // Received time-stamp seconds
    rxTm_f       : uint(32);   // Received time-stamp fraction of a second

    txTm_s       : uint(32);   // Transmit time-stamp seconds
    txTm_f       : uint(32);   // Transmit time-stamp fraction of a second
  }
}