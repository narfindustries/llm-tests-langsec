// Network Time Protocol Version 4 (NTPv4) Daedalus Specification
// This specification defines the structure of NTPv4 packets.

struct NTPv4Packet {
  leap_indicator: uint2;      // Leap indicator (LI)
  version: uint3;             // Protocol version (VN)
  mode: uint3;                // Mode (Mode)
  stratum: uint8;             // Stratum level of the local clock
  poll: uint8;                // Maximum interval between successive messages
  precision: uint8;           // Precision of the local clock
  root_delay: uint32;         // Total round-trip delay to the reference clock
  root_dispersion: uint32;    // Total dispersion to the reference clock
  reference_id: uint32;       // Reference clock identifier
  reference_timestamp: uint64; // Time when the system clock was last set or corrected
  origin_timestamp: uint64;   // Time at the client when the request departed for the server
  receive_timestamp: uint64;  // Time at the server when the request arrived from the client
  transmit_timestamp: uint64; // Time at the server when the response left for the client
  key_identifier: uint32;     // Optional: Key identifier for authentication
  message_digest: uint128;    // Optional: Message digest (MD5) for authentication
}