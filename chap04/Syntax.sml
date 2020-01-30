structure Syntax =
struct 
  datatype term =
      TmTrue
    | TmFalse
    | TmIf of term * term * term
    | TmZero
    | TmSucc of term
    | TmPred of term
    | TmIsZero of term

  fun toString t =
    case t of
        TmTrue            => "true"
      | TmFalse           => "false"
      | TmIf (t1, t2, t3) => "if " ^ toString t1 ^
                             "then " ^ toString t2 ^
                             "else " ^ toString t3
      | TmZero            => "0"
      | TmSucc t          => "succ (" ^ toString t ^ ")"
      | TmPred t          => "pred (" ^ toString t ^ ")"
      | TmIsZero t        => "pred (" ^ toString t ^ ")"
end
