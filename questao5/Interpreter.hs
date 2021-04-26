module Interpreter where

import AbsLI
import Prelude hiding (lookup)


type ErrorMessage = String

---- Aqui mudamos o tipo retornado por executeP para aportar a possibilidade de um erro durante a execução
---- de um Progama, como por exemplo divisão por 0, que seria identificada nos níveis mais baixo de execução e propagada até aqui 

executeP :: RContext -> Program -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

---- Num nível mais baixo, a função execute
---- deve também poder engatilhar e propragar um erro, dadas certas condições

---- Foi utilizada amplamente a ferramenta 'case' para separar a execução de sucesso
---- de uma execução que apresentou falhas

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
   ----- O 'do while' difere do 'while' pois executa o bloco de código antes de testar uma condição
   ----- Foi utizado o próprio SWhile para compor o SdoWhile
   SdoWhile stm exp -> case execute context stm of
                           Right executeVal ->  execute executeVal (SWhile exp stm)
                           Left msg -> Left msg
   ---- O STry funciona como pedido na questão, e propaga o contexto mais recente caso haja erro no try, para o bloco do catch
   ---- mas com ou sem erro, o bloco finally é sempre executado. O bloco catch, somente caso haja erro no bloco try
   STry (ctxTHead:ctxTTail) ctxC ctxF -> 
      case execute context ctxTHead of
      Right newCtx0 -> case execute newCtx0 (SBlock ctxTTail) of 
                           Right newCtx1 -> execute newCtx1 (SBlock ctxF)
                           Left msg -> case execute newCtx0 (SBlock ctxC) of
                                       Right newCtx2 -> execute newCtx2 (SBlock ctxF)
                                       Left msg -> Left msg
      Left msg -> case execute context (SBlock ctxC) of
                  Right newCtx -> execute newCtx (SBlock ctxF)
                  Left msg -> Left msg


data Valor   = ValorStr String |
               ValorInt Integer |
               ValorBool Bool

s :: Valor -> String
s (ValorStr str) = str
i :: Valor -> Integer
i (ValorInt vint) = vint
b :: Valor -> Bool
b (ValorBool vbool) = vbool


instance Show Valor where
   show (ValorInt vint) = show vint
   show (ValorStr vstr) = vstr
   show (ValorBool vb) = show vb

instance Eq Valor where
   (ValorInt i1) == (ValorInt i2) =  i1 == i2
   (ValorStr s1) == (ValorStr s2) =  s1 == s2
   (ValorBool b1) == (ValorBool b2) = b1 == b2

---- Ao avaliar uma expressão, claro, é possível que surja um erro
---- É fundamental, portanto, que a tipagem de eval reflita essa realidade
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
   ----- Aqui averiguamos o caso da divisão por 0, que desencadeia um erro que se propaga.
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
   EInt n  -> Right (ValorInt n)
   EVar id  -> Right (lookup context (getStr id))
   EStr str -> Right (ValorStr str)
   EOr exp0 exp -> case eval context exp0 of 
                  Right vexp0 -> case eval context exp of 
                                 Right vexp -> Right (ValorBool(b vexp0  || b vexp ))
                                 Left msg -> Left msg  
                  Left msg -> Left msg
   EAnd exp0 exp -> case eval context exp0 of 
                  Right vexp0 -> case eval context exp of 
                                 Right vexp -> Right (ValorBool(b vexp0  && b vexp ))
                                 Left msg -> Left msg  
                  Left msg -> Left msg
   ENot exp -> case eval context exp of 
                                 Right vexp -> Right (ValorBool(not (b vexp)))
                                 Left msg -> Left msg  
   ETrue -> Right (ValorBool True)
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
