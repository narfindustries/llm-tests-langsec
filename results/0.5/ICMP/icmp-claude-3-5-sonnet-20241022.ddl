def Main = {
  def header = {
    type = uint8;
    code = uint8;
    checksum = uint16;
  }

  def echo_request = {
    id = uint16;
    seq = uint16;
    data = Many uint8;
  }

  def echo_reply = {
    id = uint16;
    seq = uint16;
    data = Many uint8;
  }

  def dest_unreachable = {
    unused = uint32;
    ip_header = Many uint8;
    ip_datagram = Many uint8;
  }

  def time_exceeded = {
    unused = uint32;
    ip_header = Many uint8;
    ip_datagram = Many uint8;
  }

  def parameter_problem = {
    pointer = uint8;
    unused = uint24;
    ip_header = Many uint8;
    ip_datagram = Many uint8;
  }

  def redirect = {
    gateway_addr = uint32;
    ip_header = Many uint8;
    ip_datagram = Many uint8;
  }

  hdr = header;
  payload = Switch hdr.type {
    0 => echo_reply,
    3 => dest_unreachable,
    4 => source_quench,
    5 => redirect,
    8 => echo_request,
    11 => time_exceeded,
    12 => parameter_problem,
    _ => Many uint8
  };

  def source_quench = {
    unused = uint32;
    ip_header = Many uint8;
    ip_datagram = Many uint8;
  }
}