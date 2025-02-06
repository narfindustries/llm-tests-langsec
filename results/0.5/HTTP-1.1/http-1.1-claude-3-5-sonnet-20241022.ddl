def Main = HTTP

def HTTP = Request | Response

def SP = " "
def CRLF = "\r\n"
def DIGIT = '[0-9]'
def ALPHA = '[A-Za-z]'
def CHAR = '[\x00-\x7F]'
def CTL = '[\x00-\x1F\x7F]'
def SEPARATOR = '[()<>@,;:\\"\/\[\]?={} \t]'
def TOKEN = '[^()<>@,;:\\"\/\[\]?={} \t\x00-\x1F\x7F]+'
def TEXT = '[^\x00-\x1F\x7F]+'

def HttpVersion = "HTTP/" DIGIT "." DIGIT

def Method = "OPTIONS" | "GET" | "HEAD" | "POST" | "PUT" | "DELETE" | "TRACE" | "CONNECT"

def RequestURI = "*" | "http://" TOKEN ("/" TOKEN)*

def StatusCode = '[1-5][0-9][0-9]'

def ReasonPhrase = TEXT

def HeaderName = TOKEN

def HeaderValue = TEXT

def Header = HeaderName ":" SP* HeaderValue CRLF

def MessageBody = CHAR*

def Request = Method SP RequestURI SP HttpVersion CRLF Header* CRLF MessageBody?

def Response = HttpVersion SP StatusCode SP ReasonPhrase CRLF Header* CRLF MessageBody?

def GeneralHeader = 
    ("Cache-Control" ":" SP ("no-cache" | "no-store" | 'max-age=[0-9]+' | "must-revalidate" | "public" | "private" | "no-transform" | "proxy-revalidate")) |
    ("Connection" ":" SP ("close" | "keep-alive")) |
    ("Date" ":" SP '[A-Za-z]+, [0-9]{2} [A-Za-z]+ [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT') |
    ("Pragma" ":" SP "no-cache") |
    ("Transfer-Encoding" ":" SP ("chunked" | "compress" | "deflate" | "gzip" | "identity"))

def EntityHeader =
    ("Allow" ":" SP Method ("," SP Method)*) |
    ("Content-Encoding" ":" SP ("gzip" | "compress" | "deflate" | "identity")) |
    ("Content-Language" ":" SP TOKEN ("," SP TOKEN)*) |
    ("Content-Length" ":" SP DIGIT+) |
    ("Content-Location" ":" SP TEXT) |
    ("Content-MD5" ":" SP '[A-Za-z0-9+\/=]{24}') |
    ("Content-Range" ":" SP "bytes" SP DIGIT+ "-" DIGIT+ "/" (DIGIT+ | "*")) |
    ("Content-Type" ":" SP TOKEN "/" TOKEN) |
    ("Expires" ":" SP '[A-Za-z]+, [0-9]{2} [A-Za-z]+ [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT') |
    ("Last-Modified" ":" SP '[A-Za-z]+, [0-9]{2} [A-Za-z]+ [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT')

def RequestHeader =
    ("Accept" ":" SP TOKEN ("," SP TOKEN)*) |
    ("Accept-Charset" ":" SP TOKEN ("," SP TOKEN)*) |
    ("Accept-Encoding" ":" SP ("gzip" | "compress" | "deflate" | "br" | "identity" | "*") ("," SP ("gzip" | "compress" | "deflate" | "br" | "identity" | "*"))*) |
    ("Accept-Language" ":" SP TOKEN ("," SP TOKEN)*) |
    ("Authorization" ":" SP ("Basic" | "Bearer" | "Digest") SP TEXT) |
    ("Expect" ":" SP "100-continue") |
    ("From" ":" SP '[^@]+@[^@]+') |
    ("Host" ":" SP TOKEN (":" DIGIT+)?) |
    ("If-Match" ":" SP ("\"" TEXT "\"" | "*")) |
    ("If-Modified-Since" ":" SP '[A-Za-z]+, [0-9]{2} [A-Za-z]+ [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT') |
    ("If-None-Match" ":" SP ("\"" TEXT "\"" | "*")) |
    ("If-Range" ":" SP ("\"" TEXT "\"" | '[A-Za-z]+, [0-9]{2} [A-Za-z]+ [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT')) |
    ("If-Unmodified-Since" ":" SP '[A-Za-z]+, [0-9]{2} [A-Za-z]+ [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT') |
    ("Max-Forwards" ":" SP DIGIT+) |
    ("Proxy-Authorization" ":" SP TEXT) |
    ("Range" ":" SP "bytes=" DIGIT+ "-" DIGIT+) |
    ("Referer" ":" SP TEXT) |
    ("User-Agent" ":" SP TEXT)

def ResponseHeader =
    ("Accept-Ranges" ":" SP ("bytes" | "none")) |
    ("Age" ":" SP DIGIT+) |
    ("ETag" ":" SP "\"" TEXT "\"") |
    ("Location" ":" SP TEXT) |
    ("Proxy-Authenticate" ":" SP TEXT) |
    ("Retry-After" ":" SP (DIGIT+ | '[A-Za-z]+, [0-9]{2} [A-Za-z]+ [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT')) |
    ("Server" ":" SP TEXT) |
    ("Vary" ":" SP ("*" | TOKEN ("," SP TOKEN)*)) |
    ("WWW-Authenticate" ":" SP TEXT)