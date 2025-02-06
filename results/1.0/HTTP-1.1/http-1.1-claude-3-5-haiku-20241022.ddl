def HttpVersion:
    major: U8
    minor: U8

def RequestMethod:
    | GET
    | POST
    | PUT
    | DELETE
    | HEAD
    | OPTIONS
    | TRACE
    | CONNECT
    | PATCH

def Header:
    name: String
    value: String

def RequestLine:
    method: RequestMethod
    uri: String
    version: HttpVersion

def StatusLine:
    version: HttpVersion
    statusCode: U16
    reasonPhrase: String

def CacheControl:
    | Public
    | Private
    | NoCache
    | NoStore
    | MaxAge(U32)

def ConnectionType:
    | Close
    | KeepAlive
    | Upgrade

def AcceptHeader:
    mimeTypes: List(String)
    qualityValues: Option(List(F32))

def AuthorizationType:
    | Basic(String)
    | Digest(String)
    | Bearer(String)

def ContentType:
    mimeType: String
    charset: Option(String)
    boundary: Option(String)

def EncodingType:
    | Gzip
    | Compress
    | Deflate
    | Identity

def HttpMessage:
    method: RequestMethod
    uri: String
    version: HttpVersion
    headers: List(Header)
    body: Option(Buffer)

def HttpResponse:
    status: StatusLine
    headers: List(Header)
    body: Option(Buffer)

def ParseResult:
    | Success(HttpMessage)
    | ParseError(String)