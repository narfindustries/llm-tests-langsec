NetworkTimeProtocolV4 : structure = {
    li : uint(2),  // Leap Indicator
    vn : uint(3),  // Version Number
    mode : uint(3),  // Mode
    stratum : uint(8),  // Stratum
    poll : int(8),  // Poll Interval
    precision : int(8),  // Precision
    rootDelay : uint(32),  // Root Delay in NTP short format
    rootDispersion : uint(32),  // Root Dispersion in NTP short format
    referenceIdentifier : uint(32),  // Reference Identifier
    referenceTimestamp : uint(64),  // Reference Timestamp in NTP timestamp format
    originateTimestamp : uint(64),  // Originate Timestamp in NTP timestamp format
    receiveTimestamp : uint(64),  // Receive Timestamp in NTP timestamp format
    transmitTimestamp : uint(64),  // Transmit Timestamp in NTP timestamp format
    keyIdentifier : uint(32)?,  // Optional Key Identifier for authentication
    messageDigest : bytes(16)?  // Optional Message Digest for authentication
}