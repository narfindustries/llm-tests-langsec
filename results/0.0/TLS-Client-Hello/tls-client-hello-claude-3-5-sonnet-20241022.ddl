def TLSClientHello = {|
    legacy_version: U16 where legacy_version = 0x0303,
    random: U8[32],
    legacy_session_id_length: U8 where legacy_session_id_length <= 32,
    legacy_session_id: U8[legacy_session_id_length],
    cipher_suites_length: U16,
    cipher_suites: U16[cipher_suites_length / 2],
    compression_methods_length: U8,
    compression_methods: U8[compression_methods_length] where compression_methods = [0x00],
    extensions_length: U16,
    extensions: Extension[extensions_length]
|}

def Extension = {|
    extension_type: U16,
    extension_length: U16,
    extension_data: ExtensionData(extension_type, extension_length)
|}

def ExtensionData(type: U16, length: U16) = {|
    data: switch type {
        case 0x002b: SupportedVersions
        case 0x000d: SignatureAlgorithms
        case 0x000a: SupportedGroups
        case 0x0033: KeyShare
        case 0x0000: ServerName
        case 0x002d: PSKKeyExchangeModes
        case 0x0029: PreSharedKey
        case 0x002a: EarlyData
        case 0x002c: Cookie
        case 0x0015: Padding
        default: U8[length]
    }
|}

def SupportedVersions = {|
    versions_length: U8,
    versions: U16[versions_length / 2]
|}

def SignatureAlgorithms = {|
    supported_signature_algorithms_length: U16,
    supported_signature_algorithms: U16[supported_signature_algorithms_length / 2]
|}

def SupportedGroups = {|
    supported_groups_length: U16,
    supported_groups: U16[supported_groups_length / 2]
|}

def KeyShare = {|
    client_shares_length: U16,
    client_shares: KeyShareEntry[client_shares_length]
|}

def KeyShareEntry = {|
    group: U16,
    key_exchange_length: U16,
    key_exchange: U8[key_exchange_length]
|}

def ServerName = {|
    server_name_list_length: U16,
    server_name_list: ServerNameEntry[server_name_list_length]
|}

def ServerNameEntry = {|
    name_type: U8,
    server_name_length: U16,
    server_name: U8[server_name_length]
|}

def PSKKeyExchangeModes = {|
    ke_modes_length: U8,
    ke_modes: U8[ke_modes_length]
|}

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

def EarlyData = {||}

def Cookie = {|
    cookie_length: U16,
    cookie: U8[cookie_length]
|}

def Padding = {|
    padding: U8[*] where padding = [0x00]
|}