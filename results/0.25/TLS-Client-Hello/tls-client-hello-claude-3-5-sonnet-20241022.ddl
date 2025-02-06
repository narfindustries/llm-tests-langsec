def TLSClientHello = {|
    legacy_version: U16 = 0x0303,
    random: U8[32],
    legacy_session_id_length: U8 where legacy_session_id_length <= 32,
    legacy_session_id: U8[legacy_session_id_length],
    cipher_suites_length: U16,
    cipher_suites: U16[cipher_suites_length / 2],
    compression_methods_length: U8,
    compression_methods: U8[compression_methods_length] where forall x in compression_methods: x == 0x00,
    extensions_length: U16,
    extensions: Extension[extensions_length]
|}

def Extension = {|
    extension_type: U16,
    extension_length: U16,
    extension_data: ExtensionData(extension_type, extension_length)
|}

def ExtensionData(type: U16, length: U16) = {|
    ServerName if type == 0x0000 |
    SupportedGroups if type == 0x000a |
    SignatureAlgorithms if type == 0x000d |
    ALPN if type == 0x0010 |
    Padding if type == 0x0015 |
    EarlyData if type == 0x002a |
    PreSharedKey if type == 0x0029 |
    SupportedVersions if type == 0x002b |
    Cookie if type == 0x002c |
    PSKKeyExchangeModes if type == 0x002d |
    CertificateAuthorities if type == 0x002f |
    PostHandshakeAuth if type == 0x0031 |
    KeyShare if type == 0x0033 |
    RawBytes
|}

def ServerName = {|
    server_name_list_length: U16,
    server_name_entries: ServerNameEntry[server_name_list_length]
|}

def ServerNameEntry = {|
    name_type: U8,
    name_length: U16,
    name: U8[name_length]
|}

def SupportedGroups = {|
    supported_groups_list_length: U16,
    supported_groups: U16[supported_groups_list_length / 2]
|}

def SignatureAlgorithms = {|
    supported_signature_algorithms_length: U16,
    signature_algorithms: U16[supported_signature_algorithms_length / 2]
|}

def ALPN = {|
    alpn_list_length: U16,
    protocol_name_list: ProtocolName[alpn_list_length]
|}

def ProtocolName = {|
    protocol_name_length: U8,
    protocol_name: U8[protocol_name_length]
|}

def Padding = {|
    padding_data: U8[length]
|}

def EarlyData = {||}

def PreSharedKey = {|
    identities_length: U16,
    identities: PSKIdentity[identities_length],
    binders_length: U16,
    binders: PSKBinder[binders_length]
|}

def PSKIdentity = {|
    identity_length: U16,
    identity: U8[identity_length],
    obfuscated_ticket_age: U32
|}

def PSKBinder = {|
    binder_length: U8,
    binder: U8[binder_length]
|}

def SupportedVersions = {|
    versions_length: U8,
    versions: U16[versions_length / 2]
|}

def Cookie = {|
    cookie_length: U16,
    cookie: U8[cookie_length]
|}

def PSKKeyExchangeModes = {|
    ke_modes_length: U8,
    ke_modes: U8[ke_modes_length]
|}

def CertificateAuthorities = {|
    authorities_length: U16,
    authorities: DistinguishedName[authorities_length]
|}

def DistinguishedName = {|
    dn_length: U16,
    dn_data: U8[dn_length]
|}

def PostHandshakeAuth = {||}

def KeyShare = {|
    client_shares_length: U16,
    client_shares: KeyShareEntry[client_shares_length]
|}

def KeyShareEntry = {|
    group: U16,
    key_exchange_length: U16,
    key_exchange: U8[key_exchange_length]
|}

def RawBytes = {|
    data: U8[length]
|}