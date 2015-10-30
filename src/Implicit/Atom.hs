module Implicit.Atom where
  
import Prelude hiding (Word)

import Lava.Bit
import Lava.Word
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

atomsToWord :: (N n) => [Atom n] -> Word d n
atomsToWord = Word . foldr (\atm rest -> delay (fromInteger . atomToInteger $ atm) rest) 0

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

typeBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d N4
typeBits = Word . vtake n4 . vdrop n1 . unWord

contentBits :: (N n) => Word d (S (S (S (S (S n))))) -> Word d n
contentBits = Word . vdrop n5 . unWord

wordToAtom :: (N n) => Word d (S (S (S (S (S n))))) -> Atom n
wordToAtom w
  | bitToBool (isData w) = Data (head . wordToInts . contentBits $ w) (isDeleted w)
  | bitToBool (isCase w) = Case (head . wordToInts . contentBits $ w) (isDeleted w)
  | bitToBool (isArm w) = Arm (head . wordToInts . contentBits $ w) (isDeleted w)
  | bitToBool (isArrow w) = Arrow (head . wordToInts . contentBits $ w) (isDeleted w)
  | bitToBool (isUnCase w) = UnCase (head . wordToInts . contentBits $ w) (isDeleted w)
  | bitToBool (isLet w) = Let (head . wordToInts . contentBits $ w) (isDeleted w)
  | bitToBool (isIn w) = In (isDeleted w)
  | bitToBool (isLetRef w) = LetRef (head . wordToInts . contentBits $ w) (isDeleted w)
  | bitToBool (isLetRefPadding w) = LetRefPadding (isDeleted w)
  | bitToBool (isUnLet w) = UnLet (head . wordToInts . contentBits $ w) (isDeleted w)

isData :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isData = (=== 0) . unWord . typeBits

isCase :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isCase = (=== 1) . unWord . typeBits

isArm :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isArm = (=== 2) . unWord . typeBits

isArrow :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isArrow = (=== 3) . unWord . typeBits

isUnCase :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isUnCase = (=== 4) . unWord . typeBits

isLet :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isLet = (=== 5) . unWord . typeBits

isIn :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isIn = (=== 6) . unWord . typeBits

isLetRef :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isLetRef = (=== 7) . unWord . typeBits

isLetRefPadding :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isLetRefPadding = (=== 8) . unWord . typeBits

isUnLet :: (N n) => Word d (S (S (S (S (S n))))) -> Bit
isUnLet = (=== 9) . unWord . typeBits

isDeleted :: Word d (S n) -> Bool
isDeleted = bitToBool . vhead . unWord

markDelete :: (N n) => Word d (S n) -> Word d (S n)
markDelete (Word w) = Word $ high +> vtail w