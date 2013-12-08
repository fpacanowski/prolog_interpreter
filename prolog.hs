import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import System.IO

-- Data types

data Term = Var String | Funct String [Term] deriving Eq
type Rule = (Term, [Term])
type Rules = [Rule]
type Substitution = [(Term, Term)]

instance Show Term where
  show (Var s) = s
  show (Funct s []) = s
  show (Funct s args) = s ++ showWithRoundBrackets args where
    showWithRoundBrackets xs = "(" ++ showWithoutBrackets (show xs) ++ ")"
    showWithoutBrackets = reverse . tail . reverse . tail

-- Operations on terms

vars :: [Term] -> [Term]
vars [] = []
vars (term:terms) =
  case term of
    Var _ -> term : vars terms
    Funct _ args -> vars args ++ vars terms

contains :: Term -> Term -> Bool
contains term var = elem var $ vars [term]

substituteWith :: Term -> (Term, Term) -> Term
substituteWith term (term1, term2)
  | term == term1 = term2
  | otherwise =
    case term of
      Var _ -> term
      Funct f args ->
        let substitutedArgs = map (\x -> x `substituteWith` (term1, term2)) args
	in Funct f substitutedArgs

applySubstitution :: Substitution -> Term -> Term
applySubstitution subst term = foldl substituteWith term subst

refreshVars :: Term -> Term
refreshVars term =
  case term of
    Var x -> Var (x ++ "_")
    Funct x args -> Funct x (map refreshVars args)

refreshVarsInRules :: Rules -> Rules
refreshVarsInRules = map (\(head, body) -> (refreshVars head, map refreshVars body))

-- Unification algorithm

unify :: [(Term, Term)] -> Maybe Substitution
unify [] = Just []
unify (term:terms) = 
  case term of
    (x@(Var _), y@(Var _)) ->
      let unificator = unify terms
      in case unificator of
        Nothing -> Nothing
	Just u -> Just $ (x, y) : u
    (Funct f args1, Funct g args2) -> 
      if f==g && length args1 == length args2
      then unify $ (zip args1 args2) ++ terms
      else Nothing
    (t@(Funct _ _), x@(Var _)) ->
      unify $ (x, t):terms
    (x@(Var _), t@(Funct _ _)) ->
      if t `contains` x
      then Nothing
      else let unificator = unify terms'
               subst a = a `substituteWith` (x, t)
	       terms' = map (\(a,b) -> (subst a, subst b)) terms
        in case unificator of
	  Nothing -> Nothing
	  Just u -> Just $ (x, t) : u

-- Solver

solve :: Rules -> [Term] -> [Substitution]
solve rules goals = map (filter (\(v, _) -> elem v variables)) solution where
  variables = vars goals
  solution = solve' rules goals

solve' :: Rules -> [Term] -> [Substitution]
solve' _ [] = [[]]
solve' rules (goal:goals) = do
  let rules' = refreshVarsInRules rules
  ((head, body), subst) <- applicableRules rules' goal
  let newGoals = map (applySubstitution subst) $ body ++ goals
  solution <- solve' rules' newGoals
  let subst' = map (\(var, term) -> (var, applySubstitution solution term)) subst
  return $ subst' ++ solution

applicableRules :: Rules -> Term -> [(Rule, Substitution)]
applicableRules rules goal = mapMaybe isApplicable rules where
  isApplicable (rule@(head, _)) = case (unify [(head, goal)]) of
    Nothing -> Nothing
    Just subst -> Just (rule, subst)

-- Parser

data Query = Query [Term] | AddRule Rule deriving Show

parseAtom :: Parser Term
parseAtom = do first <- lower
               rest <- many letter
               return $ Funct (first:rest) []

parseVariable :: Parser Term
parseVariable = do first <- upper
                   rest <- many alphaNum
                   return $ Var $ first:rest

parseArgument :: Parser Term
parseArgument = parseAtom <|> parseVariable

argumentsSeparator :: Parser ()
argumentsSeparator = (spaces >> char ',' >> spaces)

parseFunctor :: Parser Term
parseFunctor = do
  first <- lower
  rest <- many alphaNum
  char '('
  args <- parseTerm `sepBy` argumentsSeparator
  char ')'
  return $ Funct (first:rest) args

parseTerm :: Parser Term
parseTerm =  try parseFunctor <|> parseAtom <|> try parseVariable

parseQuery :: Parser Query
parseQuery = try parseQuery'' <|> parseQuery' where
  parseQuery' = do
    term <- parseFunctor `sepBy` argumentsSeparator
    return $ Query term
  parseQuery'' = do
    head <- parseFunctor
    spaces
    string ":-"
    spaces
    body <- parseTerm `sepBy` argumentsSeparator
    return $ AddRule (head, body)

-- Interactive part

printSubstitution :: Substitution -> String
printSubstitution s = foldl1 (\x y -> x ++ "\n" ++ y) $ map (\(Var v, t) -> v ++ " = " ++ show t) s

printSolution :: [Substitution] -> String
printSolution s = foldl1 (\x y -> x ++ "\n\n" ++ y) $ map printSubstitution s

executeQuery :: Rules -> Either ParseError Query -> (Rules, String)
executeQuery rules (Left _) = (rules, "Parse error!")
executeQuery rules (Right (AddRule r)) = (rules ++ [r], "Added rule: " ++ show r)
executeQuery rules (Right (Query []))= (rules, "")
executeQuery rules (Right (Query t))= (rules, msg) where
  msg = "Executing query: " ++ show t ++ msg'
  msg' = if solution == []
        then "\nNo solutions"
	else "\nPossible solutions:\n" ++ printSolution solution
  solution = solve rules t

mainLoop :: Rules -> IO ()
mainLoop rules = do
  line <- getLine
  let query = parse parseQuery "prolog" line
  let (newRules, msg) = executeQuery rules query
  putStrLn msg
  eof <- isEOF
  if eof then  putStrLn "halt" else mainLoop newRules

main :: IO ()
main = mainLoop []
