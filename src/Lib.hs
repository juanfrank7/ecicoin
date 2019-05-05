{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Lib where

import qualified Crypto.Hash.SHA256    as S
import           Data.Aeson
import qualified Data.ByteArray        as BA
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B
import           GHC.Generics

data Trans = Trans { sender   :: String
                   , receiver :: String
                   , amount   :: Float
                   } deriving (Show, Generic)

data Block = Block { index     :: Int
                   , prevHash  :: B.ByteString
                   , blockData :: [Trans]
                   , nonce     :: Int
                   , blockHash :: B.ByteString
                   } deriving (Show, Generic)

type Blockchain = [Block]

instance ToJSON Trans
instance FromJSON Trans

hashBlock :: Block -> B.ByteString
hashBlock (Block idx pre dat nce _) = S.hash $ BS.concat [B.pack (show idx), pre, B.pack trans, B.pack (show nce)]
    where trans = foldr ((++) . show) "" dat

genesisBlock :: Block
genesisBlock = Block index prevHash blockData nonce blockHash
    where index     = 0
          prevHash  = ("0" :: B.ByteString)
          blockData = [Trans "Alice" "Bob" 10]
          nonce     = 0
          blockHash = ("00000000000000000" :: B.ByteString)

mineBlock :: Block -> Block
mineBlock prevBlock = 
    let idx = index prevBlock + 1
        preHash = blockHash prevBlock
        dat = []
        nce = 0 
    in pow idx preHash dat nce

pow :: Int -> B.ByteString -> [Trans] -> Int -> Block 
pow idx pre dat nce = case head hsh of 
      '0' -> Block idx pre dat nce $ B.pack hsh
      _   -> pow idx pre dat (nce + 1)
    where hsh = B.unpack $ hashBlock (Block idx pre dat nce "0")

createChain :: Blockchain
createChain = []

addBlock :: Block -> Blockchain -> Blockchain
addBlock block chain = if null chain 
    then [genesisBlock]
    else block : chain 

