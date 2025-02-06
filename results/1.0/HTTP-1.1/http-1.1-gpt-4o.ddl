HttpProtocol {
    message: Message
}

Message {
    start_line: StartLine,
    headers: Headers,
    body: Body &optional
}

StartLine = RequestLine | StatusLine

RequestLine {
    method: HttpMethod,
    uri: Uri,
    version: HttpVersion
}

StatusLine {
    version: HttpVersion,
    status_code: StatusCode,
    reason_phrase: ReasonPhrase
}

Headers {
    general_headers: GeneralHeaders &optional,
    request_headers: RequestHeaders &optional,
    response_headers: ResponseHeaders &optional,
    entity_headers: EntityHeaders &optional
}

GeneralHeaders {
    cache_control: CacheControlHeader &optional,
    connection: ConnectionHeader &optional,
    date: DateHeader &optional,
    pragma: PragmaHeader &optional,
    trailer: TrailerHeader &optional,
    transfer_encoding: TransferEncodingHeader &optional,
    upgrade: UpgradeHeader &optional,
    via: ViaHeader &optional
}

RequestHeaders {
    accept: AcceptHeader &optional,
    accept_charset: AcceptCharsetHeader &optional,
    accept_encoding: AcceptEncodingHeader &optional,
    accept_language: AcceptLanguageHeader &optional,
    authorization: AuthorizationHeader &optional,
    expect: ExpectHeader &optional,
    from: FromHeader &optional,
    host: HostHeader,
    if_match: IfMatchHeader &optional,
    if_modified_since: IfModifiedSinceHeader &optional,
    if_none_match: IfNoneMatchHeader &optional,
    if_range: IfRangeHeader &optional,
    if_unmodified_since: IfUnmodifiedSinceHeader &optional,
    max_forwards: MaxForwardsHeader &optional,
    proxy_authorization: ProxyAuthorizationHeader &optional,
    range: RangeHeader &optional,
    referer: RefererHeader &optional,
    te: TEHeader &optional,
    user_agent: UserAgentHeader &optional
}

ResponseHeaders {
    accept_ranges: AcceptRangesHeader &optional,
    age: AgeHeader &optional,
    etag: ETagHeader &optional,
    location: LocationHeader &optional,
    proxy_authenticate: ProxyAuthenticateHeader &optional,
    retry_after: RetryAfterHeader &optional,
    server: ServerHeader &optional,
    vary: VaryHeader &optional,
    www_authenticate: WWWAuthenticateHeader &optional
}

EntityHeaders {
    allow: AllowHeader &optional,
    content_encoding: ContentEncodingHeader &optional,
    content_language: ContentLanguageHeader &optional,
    content_length: ContentLengthHeader &optional,
    content_location: ContentLocationHeader &optional,
    content_md5: ContentMD5Header &optional,
    content_range: ContentRangeHeader &optional,
    content_type: ContentTypeHeader &optional,
    expires: ExpiresHeader &optional,
    last_modified: LastModifiedHeader &optional
}

HttpMethod = "OPTIONS" | "GET" | "HEAD" | "POST" | "PUT" | "DELETE" | "TRACE" | "CONNECT"
Uri = String
HttpVersion = "HTTP/1.1"
StatusCode = Integer
ReasonPhrase = String

CacheControlHeader = CacheControlDirective[] &separator=","
CacheControlDirective = "public" | "private" | "no-cache" | "no-store" | "max-age=" Integer | "must-revalidate" | "proxy-revalidate"

ConnectionHeader = ConnectionOption[] &separator=","
ConnectionOption = "keep-alive" | "close" | Token
Token = String

DateHeader = DateValue
DateValue = String

PragmaHeader = "no-cache"
TrailerHeader = HeaderFieldName[]
HeaderFieldName = String
TransferEncodingHeader = TransferCoding[] &separator=","
TransferCoding = "chunked" | "compress" | "deflate" | "gzip"

UpgradeHeader = ProtocolName[] &separator=","
ProtocolName = String
ViaHeader = ViaInfo[]
ViaInfo = String

AcceptHeader = MediaType[] &separator=","
MediaType = Type "/" Subtype
Type = String
Subtype = String

AcceptCharsetHeader = Charset[] &separator=","
Charset = String

AcceptEncodingHeader = ContentCoding[] &separator=","
ContentCoding = String

AcceptLanguageHeader = LanguageTag[] &separator=","
LanguageTag = String

AuthorizationHeader = Credentials
Credentials = "Basic" SP Base64Token
Base64Token = String

ExpectHeader = "100-continue"
FromHeader = EmailAddress
EmailAddress = String

HostHeader = Host (":" Port)?
Host = String
Port = Integer

IfMatchHeader = EntityTag[] &separator=","
EntityTag = String

IfModifiedSinceHeader = DateHeader
IfNoneMatchHeader = EntityTag[] &separator=","
IfRangeHeader = EntityTag | DateHeader
IfUnmodifiedSinceHeader = DateHeader

MaxForwardsHeader = Integer

ProxyAuthorizationHeader = Credentials
RangeHeader = "bytes=" ByteRangeSet[]
ByteRangeSet = ByteRangeSpec[]
ByteRangeSpec = FirstBytePos "-" LastBytePos?
FirstBytePos = Integer
LastBytePos = Integer

RefererHeader = Uri
TEHeader = TEncoding[] &separator=","
TEncoding = TransferCoding | "trailers"

UserAgentHeader = Product[] &separator=" "
Product = String

AcceptRangesHeader = "bytes" | "none"
AgeHeader = Integer
ETagHeader = EntityTag
LocationHeader = Uri

ProxyAuthenticateHeader = Challenge[] &separator=","
Challenge = String

RetryAfterHeader = DateHeader | Integer

ServerHeader = Product[] &separator=" "

VaryHeader = HeaderFieldName[] &separator=","

WWWAuthenticateHeader = Challenge[] &separator=","

AllowHeader = HttpMethod[] &separator=","

ContentEncodingHeader = ContentCoding[] &separator=","
ContentLanguageHeader = LanguageTag[] &separator=","

ContentLengthHeader = Integer

ContentLocationHeader = Uri
ContentMD5Header = Base64Token

ContentRangeHeader = "bytes " ContentRangeSpec
ContentRangeSpec = ByteRangeSet "/" (InstanceLength | "*")
InstanceLength = Integer

ContentTypeHeader = MediaType

ExpiresHeader = DateHeader
LastModifiedHeader = DateHeader

Body = String

Integer = [0-9]+
String = /[^\r\n]+/
SP = " "