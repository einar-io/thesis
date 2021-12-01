{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}

module Json ( json2series ) where

import Types hiding (Dataset)
import Data.Aeson hiding (Series)
import Data.Aeson.Types (Parser)
import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BLU
--import qualified Data.Text.Lazy.IO as T
--import qualified Data.Text.Lazy.Encoding as T
--import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Flow

data Runtimes = Runtimes { runtimes :: [Int] }    deriving (Show, Generic, ToJSON, FromJSON)
data Dataset  = Dataset  { dataset  :: Runtimes } deriving (Show, Generic)
data Datasets = Datasets { datasets :: Dataset}   deriving (Show, Generic, FromJSON)
data Filefut  = Filefut  { filefut  :: Datasets } deriving (Show, Generic)

parseSet :: Value -> Parser Dataset
parseSet =
  withObject "Set" $ \obj ->
    head (HM.toList obj) |> \(_datasetName, datasetObj) -> do
      thisObj <- parseJSON datasetObj
      return $ Dataset { dataset = thisObj }

instance FromJSON Dataset where
  parseJSON = parseSet

parseFutfile :: Value -> Parser Filefut
parseFutfile =
  withObject "Filefut" $ \obj ->
    head (HM.toList obj) |> \(_datasetName, datasetObj) -> do
      thisObj <- parseJSON datasetObj
      return $ Filefut { filefut = thisObj }

instance FromJSON Filefut where
  parseJSON = parseFutfile

obj1 :: BS.ByteString
obj1 = "{\"runtimes\":[2,3,4,5]}"
obj2 :: BS.ByteString
obj2 = "{\"#gge9353sdf\": { \"runtimes\": [2, 3, 4, 5] }}" -- :: BS.ByteString
obj3 :: BS.ByteString
obj3 = "{\"datasets\":{\"#gge9353sdf\": { \"runtimes\": [2, 3, 4, 5] }}}" -- :: BS.ByteString
obj4 :: BS.ByteString
obj4 = "{\"einartesttest.fut\":{\"datasets\":{\"#gge9353sdf\": { \"runtimes\": [2, 3, 4, 5] }}}}" -- :: BS.ByteString

getObj1:: Maybe Runtimes
getObj1 = decode obj1 :: Maybe Runtimes
getObj2 :: Maybe Dataset
getObj2 = decode obj2 :: Maybe Dataset
getObj3 :: Maybe Datasets
getObj3 = decode obj3 :: Maybe Datasets
getObj4 :: Maybe Filefut
getObj4 = decode obj4 :: Maybe Filefut

json2series :: Json -> IO Series
json2series jsobj = do
  {-
  print $ "\n\nFILENAME IS: " ++ filename ++ "\n\n"
  jsobj <- BS.readFile <| "build/" ++ filename ++ ".json"
  -}
  let jsobjLazy = BLU.fromString jsobj
  case eitherDecode jsobjLazy of
    Left _ -> return []
    Right fut -> fut
              |> filefut
              |> datasets
              |> dataset
              |> runtimes
              |> map fromIntegral
              |> return
