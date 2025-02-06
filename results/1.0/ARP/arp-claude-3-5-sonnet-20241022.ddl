def HW_ETHERNET = 1

def ARP_REQUEST = 1
def ARP_REPLY = 2

def ARP = {
    hardware_type : uint(16);
    protocol_type : uint(16);
    hardware_length : uint(8);
    protocol_length : uint(8);
    operation : uint(16);
    sender_hardware_address : bytes(hardware_length);
    sender_protocol_address : bytes(protocol_length);
    target_hardware_address : bytes(hardware_length);
    target_protocol_address : bytes(protocol_length);
}

def Main = ARP