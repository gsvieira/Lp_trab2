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
                        Right evalVal -> if evalVal /= 0
                                       then case execute context stm of
                                          Right executeVal ->  execute executeVal (SWhile exp stm)
                                          Left msg -> Left msg
                                       else Right context
                        Left msg -> Left msg
                     


{- Dica: o tipo de eval deve mudar para
   eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp -> Either ErrorMessage Integer
eval context x = case x of
   EAdd exp0 exp -> case eval context exp0 of 
                  Right vexp0 -> case eval context exp of 
                                 Right vexp -> Right (vexp0 + vexp) 
                                 Left msg -> Left msg  
                  Left msg -> Left msg
   ESub exp0 exp -> case eval context exp0 of 
                  Right vexp0 -> case eval context exp of 
                                 Right vexp -> Right (vexp0 - vexp) 
                                 Left msg -> Left msg  
                  Left msg -> Left msg
   EMul exp0 exp -> case eval context exp0 of 
                  Right vexp0 -> case eval context exp of 
                                 Right vexp -> Right (vexp0 * vexp) 
                                 Left msg -> Left msg  
                  Left msg -> Left msg
   EDiv exp0 exp -> case eval context exp0 of 
                  Right vexp0 -> case eval context exp of 
                                 Right vexp -> if vexp == 0
                                                then Left "divisao por 0" 
                                                else Right (vexp0 `div` vexp)
                                 Left msg -> Left msg  
                  Left msg -> Left msg
   EInt n  -> Right n
   EVar id  -> Right (lookup context (getStr id))

-- Dica: voce nao precisa mudar o codigo a partir daqui
type RContext = [(String,Integer)]

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
   | i == s = (i,nv):cs
   | otherwise = (i,v) : update cs s nv
