def Main = HttpMessage

def HttpMessage =
    start_line RequestLine | StatusLine
    headers Header*
    body Body

def RequestLine =
    method Method
    SP
    request_uri URI
    SP
    version HttpVersion
    CRLF

def StatusLine =
    version HttpVersion
    SP
    status StatusCode
    SP
    reason ReasonPhrase
    CRLF

def Method =
    OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT

def OPTIONS = "OPTIONS"
def GET = "GET"
def HEAD = "HEAD"
def POST = "POST"
def PUT = "PUT"
def DELETE = "DELETE"
def TRACE = "TRACE"
def CONNECT = "CONNECT"

def HttpVersion =
    HTTP11

def HTTP11 = "HTTP/1.1"

def StatusCode =
    [0-9]{3}

def ReasonPhrase =
    (!CRLF ANY)*

def URI =
    (!SP ANY)*

def Header =
    name HeaderName
    COLON SP
    value HeaderValue
    CRLF

def HeaderName =
    Accept | AcceptCharset | AcceptEncoding | AcceptLanguage |
    AcceptRanges | Age | Allow | Authorization | CacheControl |
    Connection | ContentEncoding | ContentLanguage | ContentLength |
    ContentLocation | ContentMD5 | ContentRange | ContentType |
    Date | ETag | Expect | Expires | From | Host | IfMatch |
    IfModifiedSince | IfNoneMatch | IfRange | IfUnmodifiedSince |
    LastModified | Location | MaxForwards | Pragma | ProxyAuthenticate |
    ProxyAuthorization | Range | Referer | RetryAfter | Server |
    TE | TransferEncoding | Upgrade | UserAgent | Vary |
    Via | Warning | WWWAuthenticate

def Accept = "Accept"
def AcceptCharset = "Accept-Charset"
def AcceptEncoding = "Accept-Encoding"
def AcceptLanguage = "Accept-Language"
def AcceptRanges = "Accept-Ranges"
def Age = "Age"
def Allow = "Allow"
def Authorization = "Authorization"
def CacheControl = "Cache-Control"
def Connection = "Connection"
def ContentEncoding = "Content-Encoding"
def ContentLanguage = "Content-Language"
def ContentLength = "Content-Length"
def ContentLocation = "Content-Location"
def ContentMD5 = "Content-MD5"
def ContentRange = "Content-Range"
def ContentType = "Content-Type"
def Date = "Date"
def ETag = "ETag"
def Expect = "Expect"
def Expires = "Expires"
def From = "From"
def Host = "Host"
def IfMatch = "If-Match"
def IfModifiedSince = "If-Modified-Since"
def IfNoneMatch = "If-None-Match"
def IfRange = "If-Range"
def IfUnmodifiedSince = "If-Unmodified-Since"
def LastModified = "Last-Modified"
def Location = "Location"
def MaxForwards = "Max-Forwards"
def Pragma = "Pragma"
def ProxyAuthenticate = "Proxy-Authenticate"
def ProxyAuthorization = "Proxy-Authorization"
def Range = "Range"
def Referer = "Referer"
def RetryAfter = "Retry-After"
def Server = "Server"
def TE = "TE"
def TransferEncoding = "Transfer-Encoding"
def Upgrade = "Upgrade"
def UserAgent = "User-Agent"
def Vary = "Vary"
def Via = "Via"
def Warning = "Warning"
def WWWAuthenticate = "WWW-Authenticate"

def HeaderValue =
    (!CRLF ANY)*

def Body =
    ANY*

def SP = " "
def CRLF = "\r\n"
def COLON = ":"

def Token =
    [A-Za-z0-9!#$%&'*+.^_`|~-]+

def QuotedString =
    DQUOTE (!DQUOTE BSLASH ANY | !DQUOTE !BSLASH ANY)* DQUOTE

def DQUOTE = "\""
def BSLASH = "\\"

def HexNumber =
    [0-9A-Fa-f]+