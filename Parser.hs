module Parser (
  parseExpr
) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr   as Ex

import           Lexer
import           Pretty
import           Syntax

-------------------------------------------------------------------------------
-- Expression
-------------------------------------------------------------------------------

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- natural
  return (Lit (LInt (fromIntegral n)))

addition :: Parser Expr
addition = do
  reservedOp "+"
  e <- term
  e' <- term
  return (Add e e')

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp ":"
  t <- type'
  reservedOp "."
  e <- expr
  return (Lam x t e)

derive' :: Expr -> Expr
derive' (Var name) = Var ("d" ++ name)
derive' (Add x y) = Add (Add x y) (Add (derive' x) (derive' y))
derive' (App x y) = App (App (derive' x) y) (derive' y)
derive' (Lam n t e) = (Lam n t (Lam ("d" ++ n) t (derive' e))) -- todo: derive type t
derive' x = x

-- Lift non-derivative terms to the top of the lambda stack
liftNonDerivativeTerms' :: Expr -> Maybe String -> [Expr] -> [Expr] -> Expr
liftNonDerivativeTerms' x@(Lam n t e) ms ts dts =
  case ms of
    Just prevN | ("d" ++ prevN) == n -> liftNonDerivativeTerms' e Nothing ts (dts ++ [x])
    _ -> liftNonDerivativeTerms' e (Just n) (ts ++ [x]) dts
liftNonDerivativeTerms' e _ ts dts = reconstructLambdas ts dts e

-- term lambdas, derivate term lambdas, final expression
reconstructLambdas :: [Expr] -> [Expr] -> Expr -> Expr
reconstructLambdas ((Lam n t _):ds) dts e = (Lam n t (reconstructLambdas ds dts e))
reconstructLambdas [] ((Lam n t _):dts) e = (Lam n t (reconstructLambdas [] dts e))
reconstructLambdas [] [] e = e

liftNonDerivativeTerms :: Expr -> Expr
liftNonDerivativeTerms e = liftNonDerivativeTerms' e Nothing [] []

derive :: Parser Expr
derive = do
  reservedOp "derive"
  e <- expr
  return (liftNonDerivativeTerms $ derive' e)

bool :: Parser Expr
bool =  (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

term :: Parser Expr
term =  parens expr
    <|> bool
    <|> number
    <|> variable
    <|> addition
    <|> lambda
    <|> derive

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

tyatom :: Parser Type
tyatom = tylit <|> (parens type')

tylit :: Parser Type
tylit = (reservedOp "Bool" >> return TBool)
  <|> (reservedOp "Int" >> return TInt)
  <|> (reservedOp "+" >> (return $ TArr (TArr TInt TInt) TInt))

type' :: Parser Type
type' = Ex.buildExpressionParser tyops tyatom
  where
    infixOp x f = Ex.Infix (reservedOp x >> return f)
    tyops = [
        [infixOp "->" TArr Ex.AssocRight]
      ]

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input
