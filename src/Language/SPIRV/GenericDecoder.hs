module Language.SPIRV.GenericDecoder(
  -- primitive types
    D, runD  -- , DSt(..)
  -- user state
  , dModifyUserState, dGetUserState
  -- error handling
  , dError, dErrorAt

  -- primitive builders
  , dSkip, dBytes, dPeek, dOffset
  --
  , dTrace
  -- canned 32b decoders
  , dX32_LE, dX32_BE
  ) where


import Control.Applicative
import Data.Bits
import Data.Word
import Debug.Trace
import Text.Printf
import qualified Control.Monad.State.Strict as CMS
import qualified Control.Monad.Except as CME
import qualified Data.ByteString as S


type D s a = CMS.StateT (DSt s) (CME.Except String) a
data DSt s =
  DSt {
    dsBase :: !Int -- relative offset (e.g. if this is a subview)
  , dsLength :: !Int -- total length of initial buffer
  , dsBytes :: !S.ByteString
  , dsUserState :: !s
  }


runD :: D s a -> s -> S.ByteString -> Either String a
runD da s bs =
    case CME.runExcept (CMS.runStateT da (DSt 0 (S.length bs) bs s)) of
      Left err -> Left err
      Right (b,ds) -> Right b


dModifyUserState :: (s -> s) -> D s ()
dModifyUserState f = CMS.modify (\ds -> ds{dsUserState = f (dsUserState ds)})

dGetUserState :: D s s
dGetUserState = CMS.gets dsUserState

-- dGetState :: D s DSt
-- dGetState = CMS.get
-- dModifyState :: (DSt -> DSt) -> D s DSt
-- dModifyState = CMS.modify

-----------------------------

dError :: String -> D s a
dError msg = do {off <- dOffset; dErrorAt off msg}
dErrorAt :: Int -> String -> D s a
dErrorAt off msg = CME.throwError (printf "at offset 0x%X" off ++ ". " ++ msg)

-----------------------------

dSkip :: Int -> D s ()
dSkip n = CMS.modify $ \ds -> ds{dsBytes = S.drop n (dsBytes ds)}

dBytes :: D s S.ByteString
dBytes = CMS.gets dsBytes

dPeek :: D s a -> D s a
dPeek da = do
  ds <- CMS.get
  a <- da
  CMS.put ds
  return a

dOffset :: D s Int
dOffset = do
  ds <- CMS.get
  return $ dsLength ds - S.length (dsBytes ds)

dTrace :: String -> D s ()
dTrace msg = do
  off <- dOffset
  trace (printf "0x%X: " off ++ msg) (return ())

-----------------------------
dX32_LE :: String -> (Word32 -> a) -> D s a
dX32_LE = dX32_G (0,8,16,24)
dX32_BE :: String -> (Word32 -> a) -> D s a
dX32_BE = dX32_G (24,16,8,0)
dX32_G :: (Int,Int,Int,Int) -> String -> (Word32 -> a) -> D s a
dX32_G (s0,s1,s2,s3) what cons = do
  ds <- CMS.get
  case S.splitAt 4 (dsBytes ds) of
    (bs_pfx,bs_sfx)
      | S.length bs_pfx < 4 -> dError ("underflow reading " ++ what ++ ")")
      | otherwise -> CMS.put (ds{dsBytes = bs_sfx}) >> return (cons w32)
      where [b0,b1,b2,b3] = S.unpack bs_pfx
            w32 =
              (fromIntegral b0 `shiftL` s0) .|.
              (fromIntegral b1 `shiftL` s1) .|.
              (fromIntegral b2 `shiftL` s2) .|.
              (fromIntegral b3 `shiftL` s3)


dX16_LE :: String -> (Word16 -> a) -> D s a
dX16_LE = dX16_G (0,8)
dX16_BE :: String -> (Word16 -> a) -> D s a
dX16_BE = dX16_G (8,0)
dX16_G :: (Int,Int) -> String -> (Word16 -> a) -> D s a
dX16_G (s0,s1) what cons = do
  ds <- CMS.get
  case S.splitAt 2 (dsBytes ds) of
    (bs_pfx,bs_sfx)
      | S.length bs_pfx < 4 -> dError ("underflow reading " ++ what ++ ")")
      | otherwise -> CMS.put (ds{dsBytes = bs_sfx}) >> return (cons w16)
      where [b0,b1] = S.unpack bs_pfx
            w16 =
              (fromIntegral b0 `shiftL` s0) .|.
              (fromIntegral b1 `shiftL` s1)


dX8 :: String -> (Word8 -> a) -> D s a
dX8 what cons = do
  ds <- CMS.get
  case S.uncons (dsBytes ds) of
    Nothing -> dError ("underflow reading " ++ what ++ ")")
    Just (w8,bs_sfx) -> CMS.put (ds{dsBytes = bs_sfx}) >> return (cons w8)


