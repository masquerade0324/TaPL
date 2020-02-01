structure Evaluator =
struct
  open Syntax

  exception NoRuleApplies

  fun isVal ctx (TmAbs _) = true
    | isVal ctx _         = false

  fun eval1 ctx t =
    case t of
        TmApp (TmAbs (x, t12), t2) => if isVal ctx t2 then substTop t2 t12
                                      else TmApp (TmAbs (x, t12), eval1 ctx t2)
      | TmApp (t1, t2)             => TmApp (eval1 ctx t1, t2)
      | _                          => raise NoRuleApplies

  fun eval (ctx : context) (t : term) =
    eval ctx (eval1 ctx t) handle NoRuleApplies => t
end

