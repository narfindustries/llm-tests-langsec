def ARP = {
    hwtype: bits(16);
    ptype: bits(16);
    hwlen: bits(8);
    plen: bits(8);
    oper: bits(16);
    sha: bytes(hwlen);
    spa: bytes(plen);
    tha: bytes(hwlen);
    tpa: bytes(plen);
}

def Main = ARP