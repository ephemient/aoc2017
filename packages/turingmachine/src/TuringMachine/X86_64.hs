{-# LANGUAGE CApiFFI, CPP, FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, GeneralizedNewtypeDeriving, MultiParamTypeClasses, MultiWayIf, NamedFieldPuns, NondecreasingIndentation, RecordWildCards, ScopedTypeVariables, TypeApplications, ViewPatterns #-}
module TuringMachine.X86_64 (Instruction(..), Move(..), Table, Tape, TuringMachine, createTape, createTuringMachine, destroyTape, destroyTuringMachine, foldTape, moveLeftTape, moveRightTape, readTape, runTuringMachine, stepTuringMachine, withTape, withTuringMachine, writeTape) where

import Control.Exception (bracket)
import Control.Monad (foldM, forM_, when)
import Data.Array.IArray (IArray)
import Data.Array.Unboxed (UArray)
import Data.Bits (Bits, FiniteBits, (.|.))
import Data.Ix (Ix)
import Data.Word (Word8)
import Foreign (ForeignPtr, FunPtr, Ptr, Storable(..), castPtr, mallocForeignPtr, nullPtr, withForeignPtr)
import Foreign.C (CInt(..), CSize(..), throwErrnoIf, throwErrnoIfMinus1_)
import GHC.ForeignPtr (addForeignPtrConcFinalizer)
import System.Posix.Types (COff(..), Fd(..))
import qualified TuringMachine.Base as Base (Tape, TuringMachine)
import TuringMachine.Base (Instruction(..), Move(..), Table, createTape, createTuringMachine, destroyTape, destroyTuringMachine, moveLeftTape, moveRightTape, readTape, runTuringMachine, stepTuringMachine, writeTape)
import TuringMachine.Internal (FlatTable(..), flatTable)

newtype CProt = CProt CInt
  deriving (Bits, Bounded, Enum, Eq, FiniteBits, Integral, Num, Ord, Read, Real, Show, Storable)
foreign import capi "sys/mman.h value PROT_READ" protREAD :: CProt
foreign import capi "sys/mman.h value PROT_WRITE" protWRITE :: CProt
foreign import capi "sys/mman.h value PROT_EXEC" protEXEC :: CProt
foreign import capi "sys/mman.h value PROT_NONE" protNONE :: CProt

newtype CMapFlag = CMapFlag CInt
  deriving (Bits, Bounded, Enum, Eq, FiniteBits, Integral, Num, Ord, Read, Real, Show, Storable)
foreign import capi "sys/mman.h value MAP_SHARED" mapSHARED :: CMapFlag
foreign import capi "sys/mman.h value MAP_PRIVATE" mapPRIVATE :: CMapFlag
foreign import capi "sys/mman.h value MAP_ANONYMOUS" mapANONYMOUS :: CMapFlag

foreign import capi "sys/mman.h value MAP_FAILED" mapFAILED :: Ptr ()

foreign import ccall "mmap" mmap ::
    Ptr () -> CSize -> CProt -> CMapFlag -> Fd -> COff -> IO (Ptr ())
foreign import ccall "mprotect" mprotect :: Ptr () -> CSize -> CProt -> IO CInt
foreign import ccall "munmap" munmap :: Ptr () -> CSize -> IO CInt

foreign import ccall "&turingmachine_setup" tmSETUP :: Ptr Word8
foreign import ccall "&turingmachine_setup_length" tmSETUP_LENGTH :: Ptr CInt
foreign import ccall "&turingmachine_setup_mask" tmSETUP_MASK :: Ptr CInt
foreign import ccall "dynamic" invokeNative ::
    FunPtr (Ptr (Ptr ()) -> Ptr Int -> Ptr state -> Int -> Int  -> IO Int) ->
    Ptr (Ptr ()) -> Ptr Int -> Ptr state -> Int -> Int -> IO Int

data CTape a = CTape {ctapePage :: Ptr a, ctapePos :: CInt}

data Tape a = Tape
  { tapePtr :: ForeignPtr (CTape a)
  , tapeDefault :: a
  }

data TuringMachine state symbol = TuringMachine
  { turingMachineCode :: ForeignPtr ()
  , turingMachineCodeSize :: CSize
  }

instance Storable (CTape a) where
    sizeOf ctape@ ~CTape {..} =
        paddedSizeOf ctapePage + paddedSizeOf ctapePos %: alignment ctape
    alignment ~CTape {..} = max (alignment ctapePage) (alignment ctapePos)
    peek ptr = CTape <$> peekByteOff ptr 0 <*>
        peekByteOff ptr (paddedSizeOf ctapePage) where
        ~CTape {ctapePage} = undefined :: CTape a
    poke ptr CTape {..} = do
        pokeByteOff ptr 0 ctapePage
        pokeByteOff ptr (paddedSizeOf ctapePage) ctapePos

instance (Storable a) => Base.Tape IO a (Tape a) where
    createTape tapeDefault = do
        tapePtr <- mallocForeignPtr
        withForeignPtr tapePtr initCTape
        addForeignPtrConcFinalizer tapePtr $ withForeignPtr tapePtr destroyCTape
        return Tape {..}
    destroyTape Tape {tapePtr} = withForeignPtr tapePtr destroyCTape
    readTape Tape {..} = withForeignPtr tapePtr $ \ptr -> do
        CTape {..} <- ensureCTape tapeDefault ptr
        if ctapePage == nullPtr
        then return tapeDefault
        else peekElemOff ctapePage $ fromIntegral ctapePos
    writeTape Tape {..} symbol = withForeignPtr tapePtr $ \ptr -> do
        CTape {..} <- ensureCTape tapeDefault ptr
        pokeElemOff ctapePage (fromIntegral ctapePos) symbol
    moveLeftTape Tape {tapePtr} = withForeignPtr tapePtr $ \ptr -> do
        let ~CTape {ctapePage} = undefined :: CTape a
        CInt pos <- peekByteOff ptr $ paddedSizeOf ctapePage
        pokeByteOff ptr (paddedSizeOf ctapePage) . CInt $ pos - 1
    moveRightTape Tape {tapePtr} = withForeignPtr tapePtr $ \ptr -> do
        let ~CTape {ctapePage} = undefined :: CTape a
        CInt pos <- peekByteOff ptr $ paddedSizeOf ctapePage
        pokeByteOff ptr (paddedSizeOf ctapePage) . CInt $ pos + 1

instance (Storable state, FiniteBits symbol, Integral symbol, Storable symbol) =>
    Base.TuringMachine IO state symbol (Tape symbol)
    (TuringMachine state symbol) where
    createTuringMachine table = do
        let FlatTable {..} = flatTable @UArray @Int table
        return TuringMachine {}  -- FIXME
    destroyTuringMachine _ = return ()  -- FIXME

(%:) :: Int -> Int -> Int
a %: b = (a + b - 1) `div` b * b
infixl 7 %:

paddedSizeOf :: (Storable a) => a -> Int
paddedSizeOf x = sizeOf x %: alignment x

ctapePageSize, ctapePrevOffset, ctapeNextOffset :: Int
ctapePageSize = 4096
ctapePrevOffset = ctapePageSize - paddedSizeOf (undefined :: Ptr a)
ctapeNextOffset = ctapePrevOffset - paddedSizeOf (undefined :: Ptr a)

ctapeElements :: (Storable a) => a -> Int
ctapeElements x = ctapeNextOffset `div` paddedSizeOf x

initCTape :: (Storable a) => Ptr (CTape a) -> IO ()
initCTape ptr = poke ptr CTape {ctapePage = nullPtr, ctapePos = 0}

{-# INLINE ensureCTape #-}
ensureCTape :: forall a. (Storable a) => a -> Ptr (CTape a) -> IO (CTape a)
ensureCTape tapeDefault ptr = do
    CTape {ctapePos = (fromIntegral -> ctapePos), ..} <- peek ptr
    let pos = fromIntegral $ ctapePos `mod` ctapeElements (undefined :: a)
        mallocPage prev next = do
            page <- fmap castPtr . throwErrnoIf (== mapFAILED) "mmap" $
                mmap nullPtr (fromIntegral ctapePageSize)
                (protREAD .|. protWRITE) (mapPRIVATE .|. mapANONYMOUS) (-1) 0
            forM_ [0 .. ctapeElements (undefined :: a) - 1] $
                pokeElemOff page `flip` tapeDefault
            pokeByteOff page ctapeNextOffset next
            pokeByteOff page ctapePrevOffset prev
            when (next /= nullPtr) $ pokeByteOff next ctapePrevOffset page
            when (prev /= nullPtr) $ pokeByteOff prev ctapeNextOffset page
            poke ptr CTape {ctapePage = page, ctapePos = pos}
            return page
    page <- if
      | ctapePage == nullPtr -> mallocPage nullPtr nullPtr
      | ctapePos < 0 -> do
            prev <- peekByteOff ctapePage ctapePrevOffset
            if prev == nullPtr
            then mallocPage nullPtr ctapePage
            else prev <$ poke ptr CTape {ctapePage = prev, ctapePos = pos}
      | ctapePos >= ctapeElements (undefined :: a) -> do
            next <- peekByteOff ctapePage ctapeNextOffset
            if next == nullPtr
            then mallocPage ctapePage nullPtr
            else next <$ poke ptr CTape {ctapePage = next, ctapePos = pos}
      | otherwise -> return ctapePage
    return CTape {ctapePage = page, ctapePos = pos}

destroyCTape :: (Storable a) => Ptr (CTape a) -> IO ()
destroyCTape ptr = do
    CTape {ctapePage} <- peek ptr
    initCTape ptr
    let freeChain offset ptr = when (ptr /= nullPtr) $ do
            ptr' <- peekByteOff ptr offset
            throwErrnoIfMinus1_ "munmap" . munmap (castPtr ptr) $
                fromIntegral ctapePageSize
            freeChain offset ptr'
    when (ctapePage /= nullPtr) $
        freeChain ctapePrevOffset =<< peekByteOff ctapePage ctapePrevOffset
    freeChain ctapeNextOffset ctapePage

foldTape :: forall k a. (Storable a) => (k -> a -> IO k) -> k -> Tape a -> IO k
foldTape f z Tape {tapePtr} = withForeignPtr tapePtr $ \ptr -> do
    CTape {ctapePage} <- peek ptr
    if ctapePage == nullPtr then return z else do
    let foldChain offset ptr z = if ptr == nullPtr then return z else do
            z' <- foldM (\k i -> peekElemOff ptr i >>= f k) z
                [0 .. ctapeElements (undefined :: a) - 1]
            ptr' <- peekByteOff ptr offset
            foldChain offset ptr' z'
    peekByteOff ctapePage ctapePrevOffset >>=
        foldChain ctapePrevOffset `flip` z >>=
        foldChain ctapeNextOffset ctapePage

withTape :: (Storable a) => a -> (Tape a -> IO b) -> IO b
withTape zero = bracket (createTape zero) destroyTape

withTuringMachine ::
    (IArray arr (Instruction state symbol), Ix state, Storable state,
        FiniteBits symbol, Integral symbol, Ix symbol, Storable symbol) =>
    Table arr state symbol -> (TuringMachine state symbol -> IO a) -> IO a
withTuringMachine table =
    bracket (createTuringMachine table) destroyTuringMachine
