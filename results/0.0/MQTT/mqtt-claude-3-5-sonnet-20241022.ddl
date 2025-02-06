def Main = MQTT_Packet

def MQTT_Packet = {
    fixed_header: Fixed_Header
    variable_header: Variable_Header
    payload: Payload
}

def Fixed_Header = {
    control_field: uint8
    remaining_length: uint32
}

def Control_Field = {
    packet_type: uint4
    flags: uint4
}

def Variable_Header = {
    connect: Connect_Header
    connack: Connack_Header
    publish: Publish_Header
    puback: Puback_Header
    pubrec: Pubrec_Header
    pubrel: Pubrel_Header
    pubcomp: Pubcomp_Header
    subscribe: Subscribe_Header
    suback: Suback_Header
    unsubscribe: Unsubscribe_Header
    unsuback: Unsuback_Header
    disconnect: Disconnect_Header
    auth: Auth_Header
}

def Connect_Header = {
    protocol_name: UTF8_String
    protocol_version: uint8
    connect_flags: Connect_Flags
    keep_alive: uint16
    properties_length: uint32
    properties: Properties
}

def Connect_Flags = {
    username_flag: uint1
    password_flag: uint1
    will_retain: uint1
    will_qos: uint2
    will_flag: uint1
    clean_start: uint1
    reserved: uint1
}

def Connack_Header = {
    acknowledge_flags: uint8
    return_code: uint8
    properties_length: uint32
    properties: Properties
}

def Publish_Header = {
    topic_name: UTF8_String
    packet_identifier: uint16
    properties_length: uint32
    properties: Properties
}

def Puback_Header = {
    packet_identifier: uint16
    reason_code: uint8
    properties_length: uint32
    properties: Properties
}

def Pubrec_Header = {
    packet_identifier: uint16
    reason_code: uint8
    properties_length: uint32
    properties: Properties
}

def Pubrel_Header = {
    packet_identifier: uint16
    reason_code: uint8
    properties_length: uint32
    properties: Properties
}

def Pubcomp_Header = {
    packet_identifier: uint16
    reason_code: uint8
    properties_length: uint32
    properties: Properties
}

def Subscribe_Header = {
    packet_identifier: uint16
    properties_length: uint32
    properties: Properties
}

def Suback_Header = {
    packet_identifier: uint16
    properties_length: uint32
    properties: Properties
}

def Unsubscribe_Header = {
    packet_identifier: uint16
    properties_length: uint32
    properties: Properties
}

def Unsuback_Header = {
    packet_identifier: uint16
    properties_length: uint32
    properties: Properties
}

def Disconnect_Header = {
    reason_code: uint8
    properties_length: uint32
    properties: Properties
}

def Auth_Header = {
    reason_code: uint8
    properties_length: uint32
    properties: Properties
}

def Properties = {
    property_list: Property[]
}

def Property = {
    identifier: uint8
    value: Property_Value
}

def Property_Value = {
    byte_value: uint8
    two_byte_int: uint16
    four_byte_int: uint32
    utf8_string: UTF8_String
    binary_data: Binary_Data
    string_pair: String_Pair
}

def UTF8_String = {
    length: uint16
    data: bytes(length)
}

def Binary_Data = {
    length: uint16
    data: bytes(length)
}

def String_Pair = {
    key: UTF8_String
    value: UTF8_String
}

def Payload = {
    connect: Connect_Payload
    publish: Publish_Payload
    subscribe: Subscribe_Payload
    unsubscribe: Unsubscribe_Payload
}

def Connect_Payload = {
    client_id: UTF8_String
    will_properties: Properties
    will_topic: UTF8_String
    will_payload: Binary_Data
    username: UTF8_String
    password: Binary_Data
}

def Publish_Payload = {
    data: bytes
}

def Subscribe_Payload = {
    subscriptions: Subscription[]
}

def Subscription = {
    topic_filter: UTF8_String
    options: uint8
}

def Unsubscribe_Payload = {
    topic_filters: UTF8_String[]
}