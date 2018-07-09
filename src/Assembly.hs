{-# LANGUAGE
    DataKinds,
    GADTs,
    KindSignatures,
    Rank2Types,
    TemplateHaskell
#-}

module Assembly (
    Asm,
    execute, exit, (#),
    r0, r1, r2, r3, r4, r5, r6, r7, r8,
    r9, r10, r11, r12, r13, r14, r15,
    regs, status, mem,
    initialState, reg, addr,
    Assembly,
    label,
    cmp, mov,
    add, sub, mul, idv,
    land, lor, lxor, lsl, asr,
    neg, inc, dec, lnot,
    ldr, str,
    b, blt, beq, bgt, ble, bge
  ) where

import Control.Lens hiding ((#))
import Control.Monad.State
import qualified Data.IntMap as M
import Data.Bits
import Data.Maybe (fromMaybe)
import Unsafe.Coerce

data Result a = Exit | Label | Value a
newtype Asm s a = Asm { unwrap :: State s (Result a) }

instance Functor (Asm s) where
    fmap = liftM

instance Applicative (Asm s) where
    pure = Asm . pure . Value
    (<*>) = ap

instance Monad (Asm s) where
    Asm m >>= f = Asm $ do
        x <- m
        unwrap $ case x of
            Exit    -> exit
            Label   -> fix (f . unsafeCoerce)
            Value a -> f a

instance MonadFix (Asm s) where
    mfix = ($ unsafeCoerce exit)

execute :: Asm s a -> s -> s
execute m = execState (unwrap m)

exit :: Asm s a
exit = Asm $ return Exit

data ValType = Reg | Lit
data Val (a :: ValType) where
    R :: Int -> Val Reg
    L :: Int -> Val Lit

(#) :: (Val Lit -> a) -> Int -> a
(#) = (. L)
infixl #

r0, r1, r2, r3, r4, r5, r6, r7, r8 :: Val Reg
r9, r10, r11, r12, r13, r14, r15 :: Val Reg
r0  = R  0; r1  = R  1; r2  = R  2; r3  = R  3
r4  = R  4; r5  = R  5; r6  = R  6; r7  = R  7
r8  = R  8; r9  = R  9; r10 = R 10; r11 = R 11
r12 = R 12; r13 = R 13; r14 = R 14; r15 = R 15

data St = St { regs   :: [Int],
               status :: Maybe Ordering,
               mem    :: M.IntMap Int }

makeLensesFor
    [ ("regs",   "_regs"),
      ("status", "_status"),
      ("mem",    "_mem") ]
    ''St

initialState :: St
initialState = St (replicate 16 0) Nothing M.empty

reg :: Val Reg -> St -> Int
reg (R n) = (!! n) . regs

addr :: Int -> St -> Int
addr n = fromMaybe 0 . M.lookup n . mem

type Assembly = Asm St

type Unop = Val Reg -> Assembly ()
type Binop = forall a. Val Reg -> Val a -> Assembly ()

asm :: State St a -> Assembly a
asm = Asm . fmap Value

val :: Val a -> Assembly Int
val (R n) = asm . gets $ (!! n) . regs
val (L n) = pure n

label :: Assembly (Assembly ())
label = Asm . pure $ Label

cmp :: Val a -> Val b -> Assembly ()
cmp a b = do
    x <- val a
    y <- val b
    asm $ _status .= Just (compare x y)

binop :: (Int -> Int -> Int) -> Binop
binop f r a = do
    x <- val a
    unop (`f` x) r

mov, add, sub, mul, idv :: Binop
mov = binop (flip const)
add = binop (+)
sub = binop (-)
mul = binop (*)
idv = binop div

land, lor, lxor, lsl, asr :: Binop
land = binop (.&.)
lor  = binop (.|.)
lxor = binop xor
lsl  = binop shiftL
asr  = binop shiftR

unop :: (Int -> Int) -> Unop
unop f (R n) = asm $ _regs . ix n %= f 

neg, inc, dec, lnot :: Unop
neg  = unop negate
inc  = unop succ
dec  = unop pred
lnot = unop complement

ldr :: Binop
ldr (R n) a = do
    i <- val a
    x <- asm . use $ _mem . at i
    asm $ _regs . ix n .= fromMaybe 0 x

str :: Val a -> Val b -> Assembly ()
str a b = do
    x <- val a
    i <- val b
    asm $ _mem . at i .= Just x

branch :: (Maybe Ordering -> Bool) -> Assembly () -> Assembly ()
branch f m = asm (use _status) >>= flip when m . f

b, blt, beq, bgt, ble, bge :: Assembly () -> Assembly ()
b     = id
blt   = branch (Just LT ==)
beq   = branch (Just EQ ==)
bgt   = branch (Just GT ==)
ble m = blt m >> beq m
bge m = beq m >> bgt m

