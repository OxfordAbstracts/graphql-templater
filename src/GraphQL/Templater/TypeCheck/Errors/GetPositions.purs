module GraphQL.Templater.TypeCheck.Errors.GetPositions where


import GraphQL.Templater.TypeCheck.Errors (ArgTypeError(..), TypeError(..), TypeErrorWithPath(..))

getPositions :: forall a. TypeErrorWithPath a -> a
getPositions (TypeErrorWithPath typeError _ pos) =
  case typeError of
    ArgTypeError argTypeError ->
      case argTypeError of
        ArgUnknown _ pos' -> pos'
        ArgTypeMismatch _ pos' -> pos'
        _ -> pos
    _ ->
      pos