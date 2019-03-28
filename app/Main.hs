module Main where

import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString (..))
import UnliftIO

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, hContentLength)

import Lib

main :: IO ()
main = runIORefMonad (0 :: Int) $ withRunInIO $ \runner ->
  runEnv 8080 $ \_req resp ->
    runner (modify succ *> get) >>= \counter -> do
      let counterLBS = toLBS counter
      resp $ responseLBS status200 [(hContentLength, toLBS $ LBS.length counterLBS)] counterLBS
  where
    toLBS :: (Show a, IsString s) => a -> s
    toLBS = fromString . show
