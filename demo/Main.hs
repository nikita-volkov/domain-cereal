module Main where

import qualified Data.ByteString as ByteString
import qualified Data.Serialize as Cereal
import DomainCereal.Demo.Model
import Prelude

main :: IO ()
main = do
  let value =
        NetworkServiceAddress $
          NetworkAddress
            TcpTransportProtocol
            (IpHost (V4Ip 123))
            456
  let encoded = Cereal.encode value
  putStrLn $ "Encoded size: " <> show (ByteString.length encoded)
  case Cereal.decode encoded of
    Left err -> fail err
    Right res ->
      if value == res
        then putStrLn "Success! Decoded value does equal the original"
        else fail "Failure! Decoded valued doesn't equal the original"
