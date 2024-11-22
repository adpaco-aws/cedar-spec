/-
 Copyright Cedar Contributors

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

      https://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
-/

import Cedar.Data.Int64

/-! This file defines Cedar datetime values and functions. -/

namespace Cedar.Spec.Ext

open Cedar.Data

/--
  A duration value is measured in milliseconds and constructed from a duration string.
  A duration string is a concatenated sequence of quantity-unit pairs where the quantity
  is a positive integer and unit is one of the following:
    - `d` (for days)
    - `h` (for hours)
    - `m` (for minutes)
    - `s` (for seconds)
    - `ms` (for milliseconds)

  Duration strings are required to be ordered from largest unit to smallest
  unit, and contain one quantity per unit. Units with zero quantity may be
  omitted.

  A duration may be negative. Negative duration strings must begin with `-`.
-/
abbrev Duration := Int64

namespace Datetime

def MILLISECONDS_PER_SECOND: Int := 1000
def MILLISECONDS_PER_MINUTE: Int := 60000
def MILLISECONDS_PER_HOUR: Int := 360000
def MILLISECONDS_PER_DAY: Int := 86400000

----- Definitions -----

def duration? (i : Int) : Option Duration :=
  Int64.mk? i

def durationUnits? (n: Nat) (suffix: String) : Option Duration :=
  let num := Int64.mk? n
  match num with
  | none => none
  | some i =>
    match suffix with
    | "ms" => duration? i
    | "s" => duration? (i * MILLISECONDS_PER_SECOND)
    | "m" => duration? (i * MILLISECONDS_PER_MINUTE)
    | "h" => duration? (i * MILLISECONDS_PER_HOUR)
    | "d" => duration? (i * MILLISECONDS_PER_DAY)
    | _ => none

def unitsToMilliseconds (days hours minutes second milliseconds: Int) : Int :=
  days * MILLISECONDS_PER_DAY +
  hours * MILLISECONDS_PER_HOUR +
  minutes * MILLISECONDS_PER_MINUTE +
  second * MILLISECONDS_PER_SECOND +
  milliseconds

def isNegativeDuration (str: String) : Bool × String :=
  match str.front with
  | '-' => (true, str.drop 1)
  | _   => (false, str)

def addOptionDurations? (a b : Option Duration) : Option Duration :=
  match a, b with
  | some durationA, some durationB =>
    match Int64.add? durationA durationB with
    | none => none
    | some int => some int
  | some _, none => none
  | none, some _ => none
  | none, none => none

def parseUnit? (str : String) (suffix: String) : Option Duration :=
  if str.endsWith suffix
  then
    let newStr := str.dropRight suffix.length
    let newStrList := newStr.toList
    let digits := ((newStrList.reverse).takeWhile Char.isDigit).reverse
    if digits.isEmpty
    then none
    else
      let unitNum := String.toNat? (String.mk digits)
      match unitNum with
      | none => none
      | some num => durationUnits? num suffix
  else duration? 0

def dropUnit (str : String) (suffix: String) : String :=
    if str.endsWith suffix
    then
      let newStr := str.dropRight suffix.length
      let newStrList := newStr.toList
      String.mk ((newStrList.reverse).dropWhile Char.isDigit).reverse
    else str

def parseUnsignedDuration? (str : String) : Option Duration := do
  if str.isEmpty then failure
  let milliseconds <- parseUnit? str "ms"
  let restStr := dropUnit str "ms"
  let seconds <- parseUnit? restStr "s"
  let restStr := dropUnit restStr "s"
  let minutes <- parseUnit? restStr "m"
  let restStr := dropUnit restStr "m"
  let hours <- parseUnit? restStr "h"
  let restStr := dropUnit restStr "h"
  let days <- parseUnit? restStr "d"
  let restStr := dropUnit restStr "d"
  if restStr.isEmpty
  then
    let durations := [days, hours, minutes, seconds, milliseconds].map some
    durations.foldl (addOptionDurations? · ·) (duration? 0)
  else none

def parse (str : String) : Option Duration :=
  let (isNegative, restStr) := isNegativeDuration str
  match parseUnsignedDuration? restStr with
  | some duration =>
    if isNegative
    then Int64.neg? duration
    else some duration
  | none => none

deriving instance Repr for Duration

abbrev duration := parse

end Datetime
