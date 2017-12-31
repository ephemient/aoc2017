{-# LANGUAGE CPP #-}
#include "ghcconfig.h"
module TuringMachine (module TM) where

import TuringMachine.Base as TM (Instruction(..), Move(..), Table, Tape(..), TuringMachine(..))
#if linux_TARGET_OS && x86_64_TARGET_ARCH
import TuringMachine.X86_64 as TM hiding (Tape, TuringMachine)
#else
import TuringMachine.Generic as TM hiding (Tape, TuringMachine)
#endif
