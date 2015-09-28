module Implicit.Atom where

import Lava.Bit
import Lava.Vector
import Lava.Prelude

import Data.Bits

data Atom a =
    Data Integer Deleted

  -- branching
  | Case Integer Deleted
  | Arm Integer Deleted
  | Arrow Integer Deleted
  | UnCase Integer Deleted

  -- let expressions
  | Let Integer Deleted
  | In Deleted
  | LetRef Integer Deleted
  | LetRefPadding Deleted
  | UnLet Integer Deleted

  deriving Show

type Deleted = Bool

atomsToWord :: (N n) => [Atom n] -> Word n
atomsToWord = foldr (\atm rest -> delay (fromInteger . atomToInteger $ atm) rest) 0

atomToInteger :: Atom a -> Integer
atomToInteger atom = 0 .|. (typeEncoding atom `shiftL` 1) .|. (atomContents atom `shiftL` 5)

atomContents :: Atom a -> Integer
atomContents (Data val _)   = val
atomContents (Case ref _)   = ref
atomContents (Arm arm _)    = arm
atomContents (Arrow ref _)  = ref
atomContents (UnCase ref _) = ref
atomContents (Let ref _)    = ref
atomContents (In _)         = 0
atomContents (LetRef ref _) = ref
atomContents (LetRefPadding _) = 0
atomContents (UnLet ref _)  = ref

typeEncoding :: Atom a -> Integer
typeEncoding (Data _ _)   = 0
typeEncoding (Case _ _)   = 1
typeEncoding (Arm _ _)    = 2
typeEncoding (Arrow _ _)  = 3
typeEncoding (UnCase _ _) = 4
typeEncoding (Let _ _)    = 5
typeEncoding (In _)       = 6
typeEncoding (LetRef _ _) = 7
typeEncoding (LetRefPadding _) = 8
typeEncoding (UnLet _ _)  = 9

typeBits :: (N n) => Word (S (S (S (S (S n))))) -> Word N4
typeBits = vtake n4 . vdrop n1

contentBits :: (N n) => Word (S (S (S (S (S n))))) -> Word n
contentBits = vdrop n5

wordToAtom :: (N n) => Word (S (S (S (S (S n))))) -> Atom n
wordToAtom w
  | bitToBool (isData w) = Data (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isCase w) = Case (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isArm w) = Arm (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isArrow w) = Arrow (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isUnCase w) = UnCase (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isLet w) = Let (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isIn w) = In (isDeleted w)
  | bitToBool (isLetRef w) = LetRef (wordToInt . contentBits $ w) (isDeleted w)
  | bitToBool (isLetRefPadding w) = LetRefPadding (isDeleted w)
  | bitToBool (isUnLet w) = UnLet (wordToInt . contentBits $ w) (isDeleted w)

isData :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isData = (=== 0) . typeBits

isCase :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isCase = (=== 1) . typeBits

isArm :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isArm = (=== 2) . typeBits

isArrow :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isArrow = (=== 3) . typeBits

isUnCase :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isUnCase = (=== 4) . typeBits

isLet :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isLet = (=== 5) . typeBits

isIn :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isIn = (=== 6) . typeBits

isLetRef :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isLetRef = (=== 7) . typeBits

isLetRefPadding :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isLetRefPadding = (=== 8) . typeBits

isUnLet :: (N n) => Word (S (S (S (S (S n))))) -> Bit
isUnLet = (=== 9) . typeBits

isDeleted :: Word (S n) -> Bool
isDeleted = bitToBool . vhead

markDelete :: (N n) => Word (S n) -> Word (S n)
markDelete w = high +> vtail w