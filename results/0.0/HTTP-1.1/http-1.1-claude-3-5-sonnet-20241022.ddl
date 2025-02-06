def HTTP_Version = "HTTP/1.1"

def SP = " "
def CRLF = "\r\n"
def DIGIT = "[0-9]"
def ALPHA = "[a-zA-Z]"
def VCHAR = "[\u0021-\u007E]"
def OWS = "[ \t]*"
def RWS = "[ \t]+"
def token = "[\\!\\#\\$\\%\\&\\'\\*\\+\\-\\.\\^_\\`\\|\\~0-9a-zA-Z]+"
def quoted_string = "\"([^\"\\\\\\r\\n]|\\\\.)*\""

def Method = "GET" | "HEAD" | "POST" | "PUT" | "DELETE" | "CONNECT" | "OPTIONS" | "TRACE"

def Request_Line = {
    method: Method
    SP
    request_target: "[^ \r\n]+"
    SP
    version: HTTP_Version
    CRLF
}

def Status_Code = "[1-5][0-9][0-9]"

def Status_Line = {
    version: HTTP_Version
    SP
    status: Status_Code
    SP
    reason: "[^\r\n]*"
    CRLF
}

def field_name = token
def field_value = (VCHAR | SP | "\t")*

def Header_Field = {
    name: field_name
    ":"
    OWS
    value: field_value
    OWS
    CRLF
}

def Message_Body = "[\u0000-\u00FF]*"

def chunk_size = "[0-9a-fA-F]+"
def chunk_ext = (";" token ("=" (token | quoted_string))?)*

def Chunk = {
    size: chunk_size
    ext: chunk_ext
    CRLF
    chunk_data: !size
    CRLF
}

def Chunked_Body = {
    chunks: Chunk*
    last_chunk: {
        "0"
        chunk_ext
        CRLF
    }
    trailer_part: Header_Field*
    CRLF
}

def Request = {
    request_line: Request_Line
    headers: Header_Field*
    CRLF
    body: (Message_Body | Chunked_Body)?
}

def Response = {
    status_line: Status_Line
    headers: Header_Field*
    CRLF
    body: (Message_Body | Chunked_Body)?
}

def HTTP_Message = Request | Response

def Content_Type = {
    media_type: token
    "/"
    subtype: token
    parameters: (";" SP* token "=" (token | quoted_string))*
}

def Accept = {
    media_ranges: (
        {
            type: ("*" | token)
            "/"
            subtype: ("*" | token)
            parameters: (";" SP* token "=" (token | quoted_string))*
            q_value: (";" SP* "q=" "[0-1](\\.[0-9]{0,3})?")?
            accept_params: (";" SP* token "=" (token | quoted_string))*
        }
        ("," SP*)*
    )*
}

def Authorization = {
    scheme: token
    SP
    parameters: (token | "[^,\\s]+")*
}

def Cache_Control = {
    directives: (
        ("no-cache" | "no-store" | "no-transform" | "only-if-cached" |
         "must-revalidate" | "public" | "private" | "proxy-revalidate") |
        {
            name: ("max-age" | "max-stale" | "min-fresh" | "s-maxage")
            "="
            value: DIGIT+
        }
    )("," SP*)*
}

def Connection = {
    options: token ("," SP* token)*
}

def Content_Length = DIGIT+

def Date = {
    date: "[A-Za-z]+, [0-9]{2} [A-Za-z]+ [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT"
}

def ETag = {
    weak_tag: "W/"?
    entity_tag: quoted_string
}

def Host = {
    hostname: token
    port: (":" DIGIT+)?
}

def If_Match = {
    "*" |
    entity_tags: (quoted_string ("," SP* quoted_string)*)
}

def Range = {
    "bytes="
    ranges: (
        {
            start: DIGIT*
            "-"
            end: DIGIT*
        }
        ("," SP*)*
    )*
}

def User_Agent = {
    product: token
    version: ("/" token)?
    comments: (SP* "(" "[^)]*" ")")*
}

def Main = HTTP_Message