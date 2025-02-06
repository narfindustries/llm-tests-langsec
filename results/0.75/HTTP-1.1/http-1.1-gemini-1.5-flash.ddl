module Http11 where

import Daedalus.Type.AST
import Daedalus.Value

data Method = GET | POST | PUT | DELETE | HEAD | OPTIONS | CONNECT | TRACE | PATCH
  deriving (Eq, Show, Enum, Bounded)

data HttpVersion = Http11 | Http10
  deriving (Eq, Show, Enum, Bounded)

data RequestLine = RequestLine { method :: Method, target :: String, version :: HttpVersion }
  deriving (Show, Eq)

data Header = Header { name :: String, value :: String }
  deriving (Show, Eq)

data Request = Request { requestLine :: RequestLine, headers :: [Header], body :: Maybe String }
  deriving (Show, Eq)

data StatusCode = Continue | SwitchingProtocols | Ok | Created | Accepted | NonAuthoritativeInformation | NoContent | ResetContent | PartialContent | MultipleChoices | MovedPermanently | Found | SeeOther | NotModified | UseProxy | TemporaryRedirect | BadRequest | Unauthorized | PaymentRequired | Forbidden | NotFound | MethodNotAllowed | NotAcceptable | ProxyAuthenticationRequired | RequestTimeout | Conflict | Gone | LengthRequired | PreconditionFailed | RequestEntityTooLarge | RequestURITooLong | UnsupportedMediaType | RequestedRangeNotSatisfiable | ExpectationFailed | InternalServerError | NotImplemented | BadGateway | ServiceUnavailable | GatewayTimeout | HttpVersionNotSupported | VariantAlsoNegotiates | InsufficientStorage | NotExtended | NetworkAuthenticationRequired
  deriving (Eq, Show, Enum, Bounded)

data Response = Response { version :: HttpVersion, statusCode :: StatusCode, reasonPhrase :: String, headers :: [Header], body :: Maybe String }
  deriving (Show, Eq)

main :: IO ()
main = return () -- Removed print statements to avoid potential issues
