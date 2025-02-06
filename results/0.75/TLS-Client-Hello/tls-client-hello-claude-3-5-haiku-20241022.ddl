type TLSClientHello = 
    let handshake_type : u8 = 1;
    let length : u24;
    let client_version : u16 = 0x0303;
    let random : [u8; 32];
    let session_id_length : u8;
    let session_id : [u8; session_id_length];
    let cipher_suite_length : u16;
    let cipher_suites : [u16; cipher_suite_length / 2];
    let compression_method_length : u8;
    let compression_methods : [u8; compression_method_length];
    let extensions_length : u16;
    let extensions : [TLSExtension; extensions_length]
in {};

type TLSExtension = 
    let extension_type : u16;
    let extension_length : u16;
    let extension_data : [u8; extension_length]
in {};

type TLSServerNameExtension = 
    let list_length : u16;
    let server_name_type : u8;
    let server_name_length : u16;
    let server_name : [u8; server_name_length]
in {};

type TLSSupportedVersionsExtension = 
    let versions_length : u8;
    let versions : [u16; versions_length / 2]
in {};

type TLSSignatureAlgorithmsExtension = 
    let algorithms_length : u16;
    let algorithms : [u16; algorithms_length / 2]
in {};

type TLSSupportedGroupsExtension = 
    let groups_length : u16;
    let groups : [u16; groups_length / 2]
in {};

type TLSKeyShareExtension = 
    let key_share_length : u16;
    let key_shares : [KeyShareEntry; key_share_length]
in {};

type KeyShareEntry = 
    let group : u16;
    let key_exchange_length : u16;
    let key_exchange : [u8; key_exchange_length]
in {};

type TLSPreSharedKeyExtension = 
    let identities_length : u16;
    let identities : [PreSharedKeyIdentity; identities_length];
    let binders_length : u16;
    let binders : [u8; binders_length]
in {};

type PreSharedKeyIdentity = 
    let identity_length : u16;
    let identity : [u8; identity_length];
    let obfuscated_ticket_age : u32
in {};

type TLSEarlyDataExtension = 
    let max_early_data_size : u32
in {};