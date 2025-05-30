{-# Language OverloadedStrings, BlockArguments #-}
import Hl7-v2-gemini-1.5-flash(pMain)
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding(encodeUtf8)
import qualified Data.List.NonEmpty as NE
import Daedalus.RTS.Input(newInput)
import RTS.Parser(runParser)
import qualified Daedalus.RTS.JSON as JS
import qualified RTS.ParseError as RTS
import RTS

main :: IO ()
main =
  do args <- getArgs
     (nm,bs) <- case args of
                   [] -> pure ("(empty)", BS.empty)
                   file : _ ->
                     do bs <- BS.readFile file
                        pure (file, bs)
     let input = newInput (encodeUtf8 (Text.pack nm)) bs
     BS8.putStrLn $
      JS.jsonToBytes
        case runParser pMain RTS.SingleError input of
          NoResults err -> JS.toJSON err
          Results rs -> JS.jsArray
                       $ map (JS.toJSON . fst)
                       $ NE.toList rs


