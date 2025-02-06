grammar TLS_Client_Hello {
  start = ClientHello;

  ClientHello = 
    legacy_version: ProtocolVersion, 
    random: Random, 
    legacy_session_id: byte[0..32], 
    cipher_suites: CipherSuite[2..2^16-2], 
    legacy_compression_methods: byte[1..2^8-1], 
    extensions: Extensions;

  ProtocolVersion = 
    major: uint8, 
    minor: uint8;

  Random = 
    byte[32];

  CipherSuite = 
    uint16;

  ExtensionType = 
    uint16;

  Extension = 
    extension_type: ExtensionType, 
    extension_length: uint16, 
    extension_data: byte[extension_length];

  Extensions = 
    Extension[0..];

  ServerName = 
    server_name_type: uint8, 
    server_name_length: uint16, 
    server_name: byte[server_name_length];

  MaxFragmentLength = 
    max_fragment_length: uint8;

  ClientCertificateURL = 
    client_certificate_url_length: uint16, 
    client_certificate_url: byte[client_certificate_url_length];

  TrustedCAKeys = 
    trusted_ca_keys_length: uint16, 
    trusted_ca_keys: byte[trusted_ca_keys_length];

  TruncatedHMAC = 
    truncated_hmac: uint8;

  StatusRequest = 
    status_request_type: uint8, 
    status_request_length: uint16, 
    status_request: byte[status_request_length];

  UserMapping = 
    user_mapping_type: uint8, 
    user_mapping_length: uint16, 
    user_mapping: byte[user_mapping_length];

  ClientAuthz = 
    client_authz_type: uint8, 
    client_authz_length: uint16, 
    client_authz: byte[client_authz_length];

  ServerAuthz = 
    server_authz_type: uint8, 
    server_authz_length: uint16, 
    server_authz: byte[server_authz_length];

  CertType = 
    cert_type_length: uint8, 
    cert_type: byte[cert_type_length];

  SupportedGroups = 
    supported_groups_length: uint16, 
    supported_groups: uint16[supported_groups_length];

  ECPointFormats = 
    ec_point_formats_length: uint8, 
    ec_point_formats: byte[ec_point_formats_length];

  SRP = 
    srp_type: uint8, 
    srp_length: uint16, 
    srp: byte[srp_length];

  SignatureAlgorithms = 
    signature_algorithms_length: uint16, 
    signature_algorithms: uint16[signature_algorithms_length];

  UseSRTP = 
    use_srtp_length: uint16, 
    use_srtp: byte[use_srtp_length];

  Heartbeat = 
    heartbeat_type: uint8, 
    heartbeat_length: uint16, 
    heartbeat: byte[heartbeat_length];

  ApplicationLayerProtocolNegotiation = 
    application_layer_protocol_negotiation_length: uint16, 
    application_layer_protocol_negotiation: byte[application_layer_protocol_negotiation_length];

  StatusRequestV2 = 
    status_request_v2_type: uint8, 
    status_request_v2_length: uint16, 
    status_request_v2: byte[status_request_v2_length];

  SignedCertificateTimestamp = 
    signed_certificate_timestamp_length: uint16, 
    signed_certificate_timestamp: byte[singed_certificate_timestamp_length];

  ClientCertificateType = 
    client_certificate_type_length: uint8, 
    client_certificate_type: byte[client_certificate_type_length];

  ServerCertificateType = 
    server_certificate_type_length: uint8, 
    server_certificate_type: byte[server_certificate_type_length];

  Padding = 
    padding_length: uint8, 
    padding: byte[padding_length];

  KeyShare = 
    key_share_entry: {
      group: uint16, 
      key_exchange_length: uint16, 
      key_exchange: byte[key_exchange_length],
    }[1..2^16-1];

  PreSharedKey = 
    pre_shared_key_identity: {
      identity_length: uint16, 
      identity: byte[identity_length], 
      obfuscated_ticket_age: uint32,
    }[1..2^16-1];

  EarlyData = 
    early_data_max_size: uint32;

  SupportedVersions = 
    versions: uint16[1..2^16-1];

  Cookie = 
    cookie: byte[1..2^8-1];

  PSKKeyExchangeModes = 
    ke_modes: uint8[1..2^8-1];

  TicketEarlyDataInfo = 
    ticket_early_data_info_max_early_data_size: uint32, 
    ticket_early_data_info_max_early_data_sizes: uint32[1..2^16-1];
}