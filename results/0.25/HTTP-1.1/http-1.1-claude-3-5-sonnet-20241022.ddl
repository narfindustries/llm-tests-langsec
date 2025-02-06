def HTTP_date = regex("[A-Za-z]+, [0-9]+ [A-Za-z]+ [0-9]+ [0-9]+:[0-9]+:[0-9]+ GMT")

def product = regex("[A-Za-z0-9!#$%&'*+-.^_`|~]+/[0-9.]+")
def comment = regex("\\([^)]*\\)")
def token = regex("[A-Za-z0-9!#$%&'*+-.^_`|~]+")
def quoted_string = regex("\"[^\"]*\"")
def entity_tag = regex("(?:W/)?\"[^\"]*\"")
def language_tag = regex("[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*")
def media_type = regex("[A-Za-z0-9!#$%&'*+-.^_`|~]+/[A-Za-z0-9!#$%&'*+-.^_`|~]+")
def qvalue = regex("0(\\.[0-9]{0,3})?|1(\\.[0]{0,3})?")
def uri = regex("[^ \\t\\r\\n<>{}|\\^`\"]+")
def chunk_size = regex("[0-9a-fA-F]+")

def CRLF = "\r\n"
def SP = " "

def general_header =
  "Cache-Control:" SP ("no-cache" | "no-store" | regex("max-age=[0-9]+") | regex("max-stale(=[0-9]+)?") | regex("min-fresh=[0-9]+") | "no-transform" | "only-if-cached" | "public" | "private" | "must-revalidate" | "proxy-revalidate" | regex("s-maxage=[0-9]+")) CRLF |
  "Connection:" SP ("close" | "keep-alive" | token) CRLF |
  "Date:" SP HTTP_date CRLF |
  "Pragma:" SP "no-cache" CRLF |
  "Transfer-Encoding:" SP ("chunked" | "compress" | "deflate" | "gzip" | "identity") CRLF |
  "Upgrade:" SP token CRLF |
  "Via:" SP regex("[0-9.]+") SP token CRLF |
  "Warning:" SP regex("[0-9]{3}") SP token SP quoted_string (SP HTTP_date)? CRLF

def request_header =
  "Accept:" SP (media_type | "*/*" | regex("[A-Za-z0-9!#$%&'*+-.^_`|~]+/\\*")) (";q=" qvalue)? CRLF |
  "Accept-Charset:" SP (token | "*") (";q=" qvalue)? CRLF |
  "Accept-Encoding:" SP ("gzip" | "compress" | "deflate" | "identity" | "*") (";q=" qvalue)? CRLF |
  "Accept-Language:" SP (language_tag | "*") (";q=" qvalue)? CRLF |
  "Authorization:" SP ("Basic" | "Digest") SP token CRLF |
  "Expect:" SP "100-continue" CRLF |
  "From:" SP regex("[^@]+@[^@]+") CRLF |
  "Host:" SP token (":" regex("[0-9]+"))? CRLF |
  "If-Match:" SP ("*" | entity_tag) CRLF |
  "If-Modified-Since:" SP HTTP_date CRLF |
  "If-None-Match:" SP ("*" | entity_tag) CRLF |
  "If-Range:" SP (entity_tag | HTTP_date) CRLF |
  "If-Unmodified-Since:" SP HTTP_date CRLF |
  "Max-Forwards:" SP regex("[0-9]+") CRLF |
  "Proxy-Authorization:" SP token CRLF |
  "Range:" SP "bytes=" regex("[0-9]*-[0-9]*") CRLF |
  "Referer:" SP uri CRLF |
  "TE:" SP token (";q=" qvalue)? CRLF |
  "User-Agent:" SP product (SP (product | comment))* CRLF

def response_header =
  "Accept-Ranges:" SP ("bytes" | "none") CRLF |
  "Age:" SP regex("[0-9]+") CRLF |
  "ETag:" SP entity_tag CRLF |
  "Location:" SP uri CRLF |
  "Proxy-Authenticate:" SP token SP token CRLF |
  "Retry-After:" SP (HTTP_date | regex("[0-9]+")) CRLF |
  "Server:" SP product (SP (product | comment))* CRLF |
  "Vary:" SP ("*" | token) CRLF |
  "WWW-Authenticate:" SP token SP token CRLF

def entity_header =
  "Allow:" SP ("GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "TRACE") CRLF |
  "Content-Encoding:" SP ("gzip" | "compress" | "deflate" | "identity") CRLF |
  "Content-Language:" SP language_tag CRLF |
  "Content-Length:" SP regex("[0-9]+") CRLF |
  "Content-Location:" SP uri CRLF |
  "Content-MD5:" SP regex("[A-Za-z0-9+/=]{24}") CRLF |
  "Content-Range:" SP "bytes" SP regex("[0-9]*-[0-9]*/[0-9]*") CRLF |
  "Content-Type:" SP media_type (";charset=" token)? (";boundary=" token)? CRLF |
  "Expires:" SP HTTP_date CRLF |
  "Last-Modified:" SP HTTP_date CRLF

def request_line = ("GET" | "POST" | "PUT" | "DELETE" | "HEAD" | "OPTIONS" | "TRACE") SP uri SP "HTTP/1.1" CRLF

def status_line = "HTTP/1.1" SP regex("[1-5][0-9]{2}") SP regex("[^\\r\\n]+") CRLF

def message_header = general_header | request_header | response_header | entity_header

def request = request_line message_header* CRLF

def response = status_line message_header* CRLF

def chunked_body = (chunk_size CRLF regex("[\\s\\S]{0,}") CRLF)* "0" CRLF CRLF

def http_message = request | response