module Helpers
    (set, get, print, randomFloat, Error(NoLocalStorage)) where

{-|

Utility functions

# Save in localStorage
@docs set

# Retrieve from localStorage
@docs get

# Error
@docs Error

# Print
@docs print

# Random float
@docs randomFloat
-}

import Native.Helpers
import Task exposing (Task)

{-|
Error indicating no storage support
-}
type Error = NoLocalStorage

{-| Save in localStorage
set
-}
set : String -> a -> Task Error (Maybe a)
set = Native.Helpers.set

{-| Retrieve from localStorage
get
-}
get : String -> Task Error (Maybe a)
get = Native.Helpers.get

{-| log
-}
print : String -> Task x ()
print = Native.Helpers.print


{-| Calls Math.random()
-}
randomFloat : () -> Task x Float
randomFloat = Native.Helpers.randomFloat
