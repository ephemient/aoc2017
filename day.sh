#!/bin/bash
. "$(git --exec-path)/git-sh-setup" || exit $?
set -euxo pipefail
DAY=${1}
grep -F -e "day${DAY}" -e "Day${DAY}" -q aoc2017.cabal || sed -i -e "
    /^data-files:/ s/\$/, day${DAY}.txt/
    /^library/,/^\$/ {
        /^  exposed-modules:/ s/\$/, Day${DAY}/
    }
    /^test/,/^\$/ {
        /^  other-modules:/ s/\$/, Day${DAY}Spec/
    }
" aoc2017.cabal
grep -F -e "Day${DAY}" -q app/Main.lhs || sed -i -e "
    /^## /,/^---/ {
        /^\$/ i \\
## [Day ${DAY}: ](/src/Day${DAY}.hs)\\
\`\`\`haskell\\
import Day${DAY} (day${DAY}a, day${DAY}b)\\
\`\`\`
    }
    /^main/,\$ {
        /^\`\`\`/ i \\    run getDayInput print [day${DAY}a, day${DAY}b]
    }
" app/Main.lhs
[[ -e "src/Day${DAY}.hs" ]] || cat >"src/Day${DAY}.hs" <<EOF
{-|
Module:         Day${DAY}
Description:    <http://adventofcode.com/2017/day/${DAY} Day ${DAY}: >
-}
{-# OPTIONS_HADDOCK ignore-exports #-}
module Day${DAY} (day${DAY}a, day${DAY}b) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import Data.Bits
import Data.Bool
import Data.Char
import Data.Either
import Data.Function
import Data.Functor
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Tuple
import Data.Word
import Debug.Trace
import Text.Printf

day${DAY}a :: String -> Int
day${DAY}a = const 0

day${DAY}b :: String -> Int
day${DAY}b = const 0
EOF
[[ -e "test/Day${DAY}Spec.hs" ]] || cat >"test/Day${DAY}Spec.hs" <<EOF
module Day${DAY}Spec (spec) where

import Day${DAY} (day${DAY}a, day${DAY}b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" \$
        it "examples" \$
            day${DAY}a "" \`shouldBe\` 0
    describe "part 2" \$
        it "examples" \$
            day${DAY}b "" \`shouldBe\` 0
EOF
[[ -e "day${DAY}.txt" ]] || touch "day${DAY}.txt"
git add -N "src/Day${DAY}.hs" "test/Day${DAY}Spec.hs" "day${DAY}.txt"
gvim -p aoc2017.cabal app/Main.lhs bench/Main.hs "src/Day${DAY}.hs" "test/Day${DAY}Spec.hs" "day${DAY}.txt" +4tabn
stack ghci aoc2017:lib
