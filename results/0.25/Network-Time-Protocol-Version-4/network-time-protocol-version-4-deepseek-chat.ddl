// Network Time Protocol Version 4 (NTPv4) Daedalus Specification
// This specification defines the structure of NTPv4 packets.

struct NTPv4Packet {
  leap_indicator: uint2;  // Leap indicator (2 bits)
  version: uint3;         // Version number (3 bits)
  mode: uint3;            // Mode (3 bits)
  stratum: uint8;         // Stratum level (8 bits)
  poll: uint8;            // Poll interval (8 bits)
  precision: uint8;       // Precision (8 bits)
  root_delay: uint32;     // Root delay (32 bits)
  root_dispersion: uint32; // Root dispersion (32 bits)
  reference_id: uint32;   // Reference ID (32 bits)
  reference_timestamp: uint64; // Reference timestamp (64 bits)
  origin_timestamp: uint64;    // Origin timestamp (64 bits)
  receive_timestamp: uint64;   // Receive timestamp (64 bits)
  transmit_timestamp: uint64;  // Transmit timestamp (64 bits)
  extension_fields: uint8[];   // Optional extension fields (variable length)
}