module FFI.Data.ByteString where

{-# FOREIGN GHC import qualified Data.ByteString #-}

postulate ByteString : Set
{-# COMPILE GHC ByteString = type Data.ByteString.ByteString #-}

