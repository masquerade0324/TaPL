structure Evaluator =
struct
  open Syntax

  exception NoRuleApplies
  exception TypeError

  fun isVal ctx t =
    case t of
        TmAbs _ => true
      | TmTrue  => true
      | TmFalse => true
      | _       => false

  fun eval1 ctx t =
    case t of
        TmApp (TmAbs (x, typ, t12), t2) =>
          if isVal ctx t2 then substTop t2 t12
          else TmApp (TmAbs (x, typ, t12), eval1 ctx t2)
      | TmApp (t1, t2)         => TmApp (eval1 ctx t1, t2)
      | TmIf (TmTrue, t2, t3)  => t2
      | TmIf (TmFalse, t2, t3) => t3
      | TmIf (t1, t2, t3)      => TmIf (eval1 ctx t1, t2, t3)
      | _                      => raise NoRuleApplies

  fun eval (ctx : context) (t : term) =
    eval ctx (eval1 ctx t) handle NoRuleApplies => t

  fun typeof ctx t =
    case t of
        TmVar (x, n) => getTypeFromCtx ctx x
      | TmAbs (x, typ, t1) =>
          let
            val ctx' = addVar ctx x typ
            val typ' = typeof ctx' t1
          in
            TyFun (typ, typ')
          end
      | TmApp (t1, t2) =>
          let
            val typ1 = typeof ctx t1
            val typ2 = typeof ctx t2
          in
            case typ1 of
                TyFun (typ11, typ12) =>
                  if typ11 = typ2 then typ12
                  else raise TypeError
              | _ => raise TypeError
          end
      | TmTrue  => TyBool
      | TmFalse => TyBool
      | TmIf (t1, t2, t3) =>
          if typeof ctx t1 = TyBool then
            let
              val typ2 = typeof ctx t2
              val typ3 = typeof ctx t3
            in
              if typ2 = typ3 then typ2 else raise TypeError
            end
          else raise TypeError
end


