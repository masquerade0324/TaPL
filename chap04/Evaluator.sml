structure Evaluator =
struct
  open Syntax

  exception NoRuleApplies

  fun isNumVal TmZero     = true
    | isNumVal (TmSucc t) = isNumVal t
    | isNumVal _          = false

  fun isVal TmTrue  = true
    | isVal TmFalse = true
    | isVal t       = if isNumVal t then true else false

  fun eval1 t =
    case t of
        TmIf (TmTrue, t2, t3)  => t2
      | TmIf (TmFalse, t2, t3) => t3
      | TmIf (t1, t2, t3)      => TmIf (eval1 t1, t2, t3)
      | TmSucc t1              => TmSucc (eval1 t1)
      | TmPred TmZero          => TmZero
      | TmPred (TmSucc t1)     => if isNumVal t1 then t1
                                  else TmPred (eval1 (TmSucc t1))
      | TmPred t1              => TmPred (eval1 t1)
      | TmIsZero TmZero        => TmTrue
      | TmIsZero (TmSucc t1)   => if isNumVal t1 then TmFalse
                                  else TmIsZero (eval1 (TmSucc t1))
      | TmIsZero t1            => TmIsZero (eval1 t1)
      | _                      => raise NoRuleApplies

  fun eval t = eval (eval1 t) handle NoRuleApplies => t
end
