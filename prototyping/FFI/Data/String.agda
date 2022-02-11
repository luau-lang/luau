module FFI.Data.String where

import Agda.Builtin.String

String = Agda.Builtin.String.String

infixr 5 _++_
_++_ = Agda.Builtin.String.primStringAppend
