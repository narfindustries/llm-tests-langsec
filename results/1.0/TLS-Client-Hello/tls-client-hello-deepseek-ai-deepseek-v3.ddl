tls_client_hello {
  legacy_version: uint16 = 0x0303;
  random: uint8[32];
  legacy_session_id: uint8, uint8[legacy_session_id];
  cipher_suites: uint16, uint16[(length / 2)];
  legacy_compression_methods: uint8, uint8[legacy_compression_methods];
  extensions_length: uint16;
  extensions: extension[extensions_length];
  padding?: uint16, uint8[length];
}

extension {
  extension_type: uint16;
  extension_length: uint16;
  extension_data: uint8[extension_length];
}

extension_supported_versions {
  extension_type: uint16 = 0x002b;
  extension_length: uint16;
  supported_versions_length: uint8;
  supported_versions: uint16[(supported_versions_length / 2)];
}

extension_key_share {
  extension_type: uint16 = 0x0033;
  extension_length: uint16;
  key_share_list_length: uint16;
  key_share_entries: key_share_entry[key_share_list_length];
}

key_share_entry {
  group: uint16;
  key_exchange_length: uint16;
  key_exchange: uint8[key_exchange_length];
}

extension_signature_algorithms {
  extension_type: uint16 = 0x000d;
  extension_length: uint16;
  supported_signature_algorithms_length: uint16;
  supported_signature_algorithms: signature_scheme[(supported_signature_algorithms_length / 2)];
}

signature_scheme {
  algorithm: uint16;
}

extension_supported_groups {
  extension_type: uint16 = 0x000a;
  extension_length: uint16;
  named_group_list_length: uint16;
  named_group_list: uint16[(named_group_list_length / 2)];
}

extension_psk_key_exchange_modes {
  extension_type: uint16 = 0x002d;
  extension_length: uint16;
  ke_modes_length: uint8;
  ke_modes: uint8[ke_modes_length];
}

extension_server_name {
  extension_type: uint16 = 0x0000;
  extension_length: uint16;
  server_name_list_length: uint16;
  server_name_list: server_name[server_name_list_length];
}

server_name {
  name_type: uint8;
  length: uint16;
  host_name: uint8[length];
}

extension_application_layer_protocol_negotiation {
  extension_type: uint16 = 0x0010;
  extension_length: uint16;
  protocol_name_list_length: uint16;
  protocol_name_list: protocol_name[protocol_name_list_length];
}

protocol_name {
  length: uint8;
  name: uint8[length];
}

extension_early_data {
  extension_type: uint16 = 0x002a;
  extension_length: uint16;
  max_early_data_size: uint32;
}

extension_pre_shared_key {
  extension_type: uint16 = 0x0029;
  extension_length: uint16;
  identities_length: uint16;
  identities: psk_identity[identities_length];
  binders_length: uint16;
  binders: binder[binders_length];
}

psk_identity {
  identity_length: uint16;
  identity: uint8[identity_length];
  obfuscated_ticket_age: uint32;
}

binder {
  binder_length: uint8;
  binder: uint8[binder_length];
}