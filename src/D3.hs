module D3 (
    fabricSize
  , Claim(..)
) where

data Claim =
    Claim {
        claimId    :: String
      , leftShift  :: Int
      , topShift   :: Int
      , width      :: Int
      , height     :: Int
    } deriving(Show, Eq)

fabricSize = 1000 :: Int
