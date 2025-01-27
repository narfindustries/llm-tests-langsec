domain mqtt {
  type byte = uint(8);
  type uint16 = uint(16);
  type uint32 = uint(32);
  type varint = uint(*);

  type connect_flags {
    username: bool @ bits 7,
    password: bool @ bits 6,
    will_retain: bool @ bits 5,
    will_qos: uint(2) @ bits [4, 3],
    will_flag: bool @ bits 2,
    clean_session: bool @ bits 1,
    reserved: bool @ bits 0
  }

  type connect {
    protocol_name: string(7) @ ascii,
    protocol_level: byte,
    connect_flags: connect_flags,
    keep_alive: uint16
  }

  type publish_flags {
    dup: bool @ bits 3,
    qos: uint(2) @ bits [2, 1],
    retain: bool @ bits 0
  }

  type publish {
    header: {
      flags: publish_flags,
      topic_id: varint
    },
    topic: string @ utf8,
    packet_id: uint16,
    payload: bytes @ length=*
  }

  grammar {
    ROOT: connect | publish;
    connect: 'connect' -> connect;
    publish: 'publish' -> publish;
  }
}