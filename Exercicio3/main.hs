import Control.Applicative (empty)
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)
import Text.Parsec.String (Parser)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  | Lit Double
  | Lt Expr Expr
  | Gt Expr Expr
  | Le Expr Expr
  | Ge Expr Expr
  | Eq Expr Expr
  | Ne Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  deriving (Show)

eval :: Expr -> Either String Double
eval (Lit n) = Right n
eval (Add e1 e2) = evalBinOp (+) e1 e2
eval (Sub e1 e2) = evalBinOp (-) e1 e2
eval (Mul e1 e2) = evalBinOp (*) e1 e2
eval (Div e1 e2) = case eval e2 of
  Right 0 -> Left "Erro: divisão por zero"
  Right v2 -> fmap (/ v2) (eval e1)
  Left err -> Left err
eval (Neg e) = fmap negate (eval e)
eval (Lt e1 e2) = evalRelOp (<) e1 e2
eval (Gt e1 e2) = evalRelOp (>) e1 e2
eval (Le e1 e2) = evalRelOp (<=) e1 e2
eval (Ge e1 e2) = evalRelOp (>=) e1 e2
eval (Eq e1 e2) = evalRelOp (==) e1 e2
eval (Ne e1 e2) = evalRelOp (/=) e1 e2
eval (And e1 e2) = evalLogicOp (&&) e1 e2
eval (Or e1 e2) = evalLogicOp (||) e1 e2
eval (Not e) = fmap (\v -> if v == 0 then 1 else 0) (eval e)

evalBinOp :: (Double -> Double -> Double) -> Expr -> Expr -> Either String Double
evalBinOp op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 `op` v2)

evalRelOp :: (Double -> Double -> Bool) -> Expr -> Expr -> Either String Double
evalRelOp op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  return (if v1 `op` v2 then 1 else 0)

evalLogicOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Either String Double
evalLogicOp op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  let bool1 = v1 /= 0
  let bool2 = v2 /= 0
  return (if bool1 `op` bool2 then 1 else 0)

exprParser :: Parser Expr
exprParser = buildExpressionParser table term
  where
    table = [ [Prefix (Neg <$ symbol "-")]
            , [Infix (Mul <$ symbol "*") AssocLeft, Infix (Div <$ symbol "/") AssocLeft]
            , [Infix (Add <$ symbol "+") AssocLeft, Infix (Sub <$ symbol "-") AssocLeft]
            , [Infix (Le <$ try (symbol "<=")) AssocNone, Infix (Ge <$ try (symbol ">=")) AssocNone,
               Infix (Lt <$ symbol "<") AssocNone, Infix (Gt <$ symbol ">") AssocNone]
            , [Infix (Eq <$ try (symbol "==")) AssocNone, Infix (Ne <$ try (symbol "/=")) AssocNone]
            , [Prefix (Not <$ symbol "!")]
            , [Infix (And <$ symbol "&&") AssocLeft]
            , [Infix (Or <$ symbol "||") AssocLeft]
            ]
    term = parens exprParser <|> (Lit <$> number)

symbol :: String -> Parser String
symbol sym = try $ string sym <* notFollowedBy (char '=') <* spaces

symbolNe :: Parser String
symbolNe = try (string "/=") <* spaces

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

number :: Parser Double
number = lexeme $ do
  n <- many1 digit
  frac <- option "" (char '.' >> many1 digit)
  return (read (n ++ if null frac then "" else '.' : frac) :: Double)

evalExpr :: String -> Either String Double
evalExpr input = case parse exprParser "" input of
  Left err -> Left ("Erro de análise: " ++ show err)
  Right expr -> eval expr

main :: IO ()
main = do
  putStrLn "Digite uma expressão:"
  input <- getLine
  case evalExpr input of
    Left err -> putStrLn err
    Right result -> putStrLn $ "Resultado: " ++ show result