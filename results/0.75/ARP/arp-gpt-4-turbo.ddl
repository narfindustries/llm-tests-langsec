module ARP {

    import Network.IPv4
    import Network.Ethernet
    import Data.Bytes
    import Data.UInt

    // Define ARP Packet structure
    type ARPPacket = struct {
        htype       : UInt16be        // Hardware type (Ethernet is 1)
        ptype       : UInt16be        // Protocol type (IPv4 is 0x0800)
        hlen        : UInt8           // Hardware address length (Ethernet MAC address length is 6)
        plen        : UInt8           // Protocol address length (IPv4 address length is 4)
        oper        : UInt16be        // Operation (1 for request, 2 for reply)
        sha         : byte[6]         // Sender hardware address (MAC)
        spa         : IPv4            // Sender protocol address (IPv4)
        tha         : byte[6]         // Target hardware address (MAC)
        tpa         : IPv4            // Target protocol address (IPv4)
    }

    // Define Ethernet Frame for ARP
    type EthernetFrameARP = struct {
        header      : Ethernet.Header 
        payload     : ARPPacket       if header.etherType == 0x0806
        // Check Ethernet type for ARP
    }

    // Parser entry point
    let parse_arp = parse (EthernetFrameARP) from "input.pcap"
}