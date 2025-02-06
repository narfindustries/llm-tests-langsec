dns = {
    header: {
        transaction_id: u16,
        flags: {
            qr: bool,
            opcode: u4,
            aa: bool,
            tc: bool,
            rd: bool,
            ra: bool,
            z: u1,
            rcode: u4
        },
        question_count: u16,
        answer_count: u16,
        authority_count: u16,
        additional_count: u16
    },
    questions: [
        {
            name: [
                {
                    length: u8,
                    label: string(length)
                }
            ],
            type: enum u16 {
                A = 1,
                NS = 2,
                CNAME = 5,
                SOA = 6,
                PTR = 12,
                MX = 15,
                AAAA = 28
            },
            class: enum u16 {
                IN = 1,
                ANY = 255
            }
        }
    ],
    answers: [
        {
            name: [
                {
                    length: u8,
                    label: string(length)
                }
            ],
            type: enum u16 {
                A = 1,
                NS = 2,
                CNAME = 5,
                SOA = 6,
                PTR = 12,
                MX = 15,
                AAAA = 28
            },
            class: enum u16 {
                IN = 1,
                ANY = 255
            },
            ttl: u32,
            rdlength: u16,
            rdata: variant {
                A: [u8; 4],
                AAAA: [u8; 16],
                CNAME: [
                    {
                        length: u8,
                        label: string(length)
                    }
                ],
                MX: {
                    preference: u16,
                    exchange: [
                        {
                            length: u8,
                            label: string(length)
                        }
                    ]
                },
                NS: [
                    {
                        length: u8,
                        label: string(length)
                    }
                ],
                PTR: [
                    {
                        length: u8,
                        label: string(length)
                    }
                ],
                SOA: {
                    mname: [
                        {
                            length: u8,
                            label: string(length)
                        }
                    ],
                    rname: [
                        {
                            length: u8,
                            label: string(length)
                        }
                    ],
                    serial: u32,
                    refresh: u32,
                    retry: u32,
                    expire: u32,
                    minimum: u32
                }
            }
        }
    ],
    authorities: [
        {
            name: [
                {
                    length: u8,
                    label: string(length)
                }
            ],
            type: enum u16 {
                A = 1,
                NS = 2,
                CNAME = 5,
                SOA = 6,
                PTR = 12,
                MX = 15,
                AAAA = 28
            },
            class: enum u16 {
                IN = 1,
                ANY = 255
            },
            ttl: u32,
            rdlength: u16,
            rdata: variant {
                A: [u8; 4],
                AAAA: [u8; 16],
                CNAME: [
                    {
                        length: u8,
                        label: string(length)
                    }
                ],
                MX: {
                    preference: u16,
                    exchange: [
                        {
                            length: u8,
                            label: string(length)
                        }
                    ]
                },
                NS: [
                    {
                        length: u8,
                        label: string(length)
                    }
                ],
                PTR: [
                    {
                        length: u8,
                        label: string(length)
                    }
                ],
                SOA: {
                    mname: [
                        {
                            length: u8,
                            label: string(length)
                        }
                    ],
                    rname: [
                        {
                            length: u8,
                            label: string(length)
                        }
                    ],
                    serial: u32,
                    refresh: u32,
                    retry: u32,
                    expire: u32,
                    minimum: u32
                }
            }
        }
    ],
    additional: [
        {
            name: [
                {
                    length: u8,
                    label: string(length)
                }
            ],
            type: enum u16 {
                A = 1,
                NS = 2,
                CNAME = 5,
                SOA = 6,
                PTR = 12,
                MX = 15,
                AAAA = 28
            },
            class: enum u16 {
                IN = 1,
                ANY = 255
            },
            ttl: u32,
            rdlength: u16,
            rdata: variant {
                A: [u8; 4],
                AAAA: [u8; 16],
                CNAME: [
                    {
                        length: u8,
                        label: string(length)
                    }
                ],
                MX: {
                    preference: u16,
                    exchange: [
                        {
                            length: u8,
                            label: string(length)
                        }
                    ]
                },
                NS: [
                    {
                        length: u8,
                        label: string(length)
                    }
                ],
                PTR: [
                    {
                        length: u8,
                        label: string(length)
                    }
                ],
                SOA: {
                    mname: [
                        {
                            length: u8,
                            label: string(length)
                        }
                    ],
                    rname: [
                        {
                            length: u8,
                            label: string(length)
                        }
                    ],
                    serial: u32,
                    refresh: u32,
                    retry: u32,
                    expire: u32,
                    minimum: u32
                }
            }
        }
    ]
}