module TuringMachine (module TM) where

import TuringMachine.Base as TM (Instruction(..), Move(..), Table, Tape(..), TuringMachine(..))
import TuringMachine.Generic as TM hiding (Tape, TuringMachine)
