module Interpreter where

import AbsLI
import Prelude hiding (lookup)


type ErrorMessage = String

{- Dica: somente o tipo de executeP precisa mudar conforme a sugestao abaixo, 
   mas a sua definicao (corpo) pode ficar a mesma
   executeP :: RContext -> Program  -> Either ErrorMessage RContext
-}
executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm
   

{- Dica: o tipo de execute deve mudar para 
 execute :: RContext -> Stm -> Either ErrorMessage RContext   
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos 
 serao afetados
-}
execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
    SAss id exp -> case eval context exp of 
                        Right evalVal -> Right (update context (getStr id) evalVal)
                        Left msg -> Left msg 
    SBlock [] -> Right context
    SBlock (s:stms) -> case execute context s of
                        Right executeVal -> execute executeVal (SBlock stms)
                        Left msg -> Left msg
    SWhile exp stm -> case eval context exp of
                        Right evalVal -> if i evalVal /= 0
                                        then case execute context stm of
                                            Right executeVal ->  execute executeVal (SWhile exp stm)
                                            Left msg -> Left msg
                                        else Right context
                        Left msg -> Left msg
    SdoWhile stm exp -> case execute context stm of
                            Right executeVal ->  execute executeVal (SWhile exp stm)
                            Left msg -> Left msg
    STry (ctxTHead:ctxTTail) (ctxCHead:ctxCTail) (ctxFHead:ctxFTail) -> 
        case execute context ctxTHead of
        Right executeVal0 -> case execute executeVal0 (SBlock ctxTTail) of 
                            Right executeVal1 -> case execute executeVal1 ctxFHead of
                                                Right executeVal -> execute executeVal (SBlock ctxFTail)
                                                Left msg -> Left msg
                            Left msg -> case execute executeVal0 ctxCHead of
                                        Right executeVal2 -> case execute executeVal2 (SBlock ctxCTail) of 
                                                            Right executeVal3-> case execute executeVal3 ctxFHead of
                                                                        Right executeVal4 -> execute executeVal4 (SBlock ctxFTail)
                                                                        Left msg -> Left msg
                                                            Left msg -> Left msg
                                        Left msg -> Left msg
        Left msg -> case execute context ctxCHead of
                    Right executeVal -> case execute executeVal (SBlock ctxCTail) of 
                                        Right executeVal -> case execute executeVal ctxFHead of
                                                Right executeVal -> execute executeVal (SBlock ctxFTail)
                                                Left msg -> Left msg
                                        Left msg -> Left msg
                    Left msg -> Left msg


data Valor   = ValorStr String |
               ValorInt Integer |
               ValorBool Bool
-- note que ja foi adicionado um novo contrutor de tipo para valor booleano

s :: Valor -> String
s (ValorStr str) = str
i :: Valor -> Integer
i (ValorInt vint) = vint
-- a funcao "b" abaixo recupera o valor booleano dentro de um valor
b :: Valor -> Bool
b (ValorBool vbool) = vbool


instance Show Valor where
   show (ValorInt vint) = show vint
   show (ValorStr vstr) = vstr
   show (ValorBool vb) = show vb

-- precisamos que Valor esteja em Eq para podermos especificar os casos de teste em Testes.hs
instance Eq Valor where
   (ValorInt i1) == (ValorInt i2) =  i1 == i2
   (ValorStr s1) == (ValorStr s2) =  s1 == s2
   (ValorBool b1) == (ValorBool b2) = b1 == b2



{- Dica: o tipo de eval deve mudar para
   eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp -> Either ErrorMessage Valor
eval context x = case x of
    EAdd exp0 exp -> case eval context exp0 of 
                    Right vexp0 -> case eval context exp of 
                                    Right vexp -> Right (ValorInt(i vexp0  + i vexp ))
                                    Left msg -> Left msg  
                    Left msg -> Left msg
    ESub exp0 exp -> case eval context exp0 of 
                    Right vexp0 -> case eval context exp of 
                                    Right vexp -> Right (ValorInt(i vexp0  - i vexp )) 
                                    Left msg -> Left msg  
                    Left msg -> Left msg
    EMul exp0 exp -> case eval context exp0 of 
                    Right vexp0 -> case eval context exp of 
                                    Right vexp -> Right (ValorInt(i vexp0  * i vexp ))
                                    Left msg -> Left msg  
                    Left msg -> Left msg
    EDiv exp0 exp -> case eval context exp0 of 
                    Right vexp0 -> case eval context exp of 
                                    Right vexp -> if i vexp == 0
                                                then Left "divisao por 0" 
                                                else Right (ValorInt(i vexp0  `div` i vexp ))
                                    Left msg -> Left msg  
                    Left msg -> Left msg
    ECon exp0 exp -> case eval context exp0 of 
                     Right vexp0 -> case eval context exp of 
                                    Right vexp -> Right (ValorStr(s vexp0  ++ s vexp ))
                                    Left msg -> Left msg  
                     Left msg -> Left msg
                     --ValorStr ( s(eval context exp0) ++ s(eval context exp))
    EInt n  -> Right (ValorInt n)
    EVar id  -> Right (lookup context (getStr id))
    EStr str -> Right (ValorStr str)
    -- adicione abaixo um padrao e comportamento associado a expressao Or
    EOr exp0 exp -> case eval context exp0 of 
                    Right vexp0 -> case eval context exp of 
                                    Right vexp -> Right (ValorBool(b vexp0  || b vexp ))
                                    Left msg -> Left msg  
                    Left msg -> Left msg
                    --Right (ValorBool ( b(eval context exp0) || b(eval context exp)))
    -- adicione abaixo um padrao e comportamento associado a expressao And
    EAnd exp0 exp -> case eval context exp0 of 
                     Right vexp0 -> case eval context exp of 
                                    Right vexp -> Right (ValorBool(b vexp0  && b vexp ))
                                    Left msg -> Left msg  
                     Left msg -> Left msg
                     --ValorBool ( b(eval context exp0) && b(eval context exp))
    -- adicione abaixo um padrao e comportamento associado a expressao Not
    ENot exp -> case eval context exp of 
                                    Right vexp -> Right (ValorBool(not (b vexp)))
                                    Left msg -> Left msg  
                
        
        
        
        
        --ValorBool ( not (b(eval context exp)))
    -- adicione abaixo um padrao e comportamento associado ao literal true
    ETrue -> Right (ValorBool True)
    -- adicione abaixo um padrao e comportamento associado ao literal false
    EFalse -> Right (ValorBool False)
type RContext = [(String,Valor)]

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
   | i == s = (i,nv):cs
   | otherwise = (i,v) : update cs s nv
