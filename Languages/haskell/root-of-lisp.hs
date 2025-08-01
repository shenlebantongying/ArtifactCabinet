-- https://crypto.stanford.edu/~blynn/lambda/lisp.html
-- It actually works
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import System.Console.Haskeline
import Text.Parsec.Error
import Control.Monad
import Data.List
import Text.Parsec

infixr 5 :.

data Expr = Lf String | Expr :. Expr | Label String Expr | Bad

instance Show Expr where
  show Bad = "?"
  show (Label s _) = s
  show (Lf "") = "()"
  show (Lf s) = s
  show (l :. r) = "(" ++ show l ++ showR r ++ ")"
    where
      showR (Lf "") = ""
      showR (l :. r) = " " ++ show l ++ showR r
      showR x = "" ++ show x

eval env = g where
  f "quote" (x :. Lf "") = x
  f "atom"  (Lf _ :. Lf "") = Lf "t"
  f "atom"  _               = Lf ""
  f "eq"    (Lf x :. Lf y :. Lf "") | x == y    = Lf "t"
                                 | otherwise = Lf ""
  f "eq"    (_    :. _    :. Lf "") = Lf ""
  f "car"   ((l :. r) :. Lf "") = l
  f "cdr"   ((l :. r) :. Lf "") = r
  f "cons"  ( l :. r  :. Lf "") = l :. r
  f "cond"  ((l :. r :. Lf "") :. rest) | Lf "t" <- g l = g r
                                        | otherwise     = f "cond" rest
  f "label" (Lf s :. e :. Lf "") = Label s e
  f "defun" (s :. etc) = g $ Lf "label" :. s :. (Lf "lambda" :. etc) :. Lf ""
  f "list"  x = x
  f _ _ = Bad

  g (Label s e :. r) = eval ((s, e):env) $ e :. r
  g (Lf s) | Just b <- lookup s env = b
           | otherwise              = Bad

  g ((Lf "lambda" :. args :. body :. Lf "") :. t) =
     eval (zip (fromLeaf <$> fromTree args) (fromTree $ mapL g t) ++ env) body
  g (Lf h :. t)
    | Just b <- lookup h env                  = g $ b :. t
    | elem h $ words "cond quote defun label" = f h t
    | otherwise                               = f h $ mapL g t
  g _ = Bad

  fromTree (Lf "")  = []
  fromTree (h :. t) = h:fromTree t
  fromLeaf (Lf x)   = x

  mapL f (Lf "")  = Lf ""
  mapL f a@(Lf _) = f a
  mapL f (l :. r) = f l :. mapL f r

expr :: Parsec String () Expr
expr = between ws ws $ atom <|> list <|> quot where
  ws   = many $ void space <|> comm
  comm = void $ char ';' >> manyTill anyChar (void (char '\n') <|> eof)
  atom = Lf <$> many1 (alphaNum <|> char '.')
  list = foldr (:.) (Lf "") <$> between (char '(') (char ')') (many expr)
  quot = char '\'' >> (Lf "quote" :.) . (:. Lf "") <$> expr

oneExpr = expr <* eof

addEnv (Label s e) = ((s, e):)
addEnv _           = id

preload = foldl' f [] $ concat $ genCadr <$> [2..4] where
  f env s = let Right expr = parse oneExpr "" s in addEnv (eval env expr) env

genCadr n = [concat ["(defun c", s, "r (x) (c", [h], "r (c", t, "r x)))"] |
  s@(h:t) <- replicateM n "ad"]

expectParen (Expect "\"(\"") = True
expectParen (Expect "\")\"") = True
expectParen _                = False
repl pre env = do
  ms <- getInputLine "> "
  case ms of
    Nothing -> outputStrLn ""
    Just s  -> case parse oneExpr "" $ pre ++ s of
      Left err  -> case find expectParen $ errorMessages err of
        Nothing -> do
          outputStrLn $ "parse error: " ++ show err
          repl "" env
        _ -> repl (pre ++ s ++ "\n") env
      Right expr -> do
        let r = eval env expr
        outputStrLn $ show r
        repl "" $ addEnv r env

main = runInputT defaultSettings $ repl "" preload

{-
(defun null. (x)
  (eq x '()))

(defun and. (x y)
  (cond (x (cond (y 't) ('t '())))
        ('t '())))

(defun not. (x)
  (cond (x '())
        ('t 't)))

(defun append. (x y)
  (cond ((null. x) y)
        ('t (cons (car x) (append. (cdr x) y)))))

(defun pair. (x y)
  (cond ((and. (null. x) (null. y)) '())
        ((and. (not. (atom x)) (not. (atom y)))
         (cons (list (car x) (car y))
               (pair. (cdr x) (cdr y))))))

(defun assoc. (x y)
  (cond ((eq (caar y) x) (cadar y))
        ('t (assoc. x (cdr y)))))

(defun eval. (e a)
  (cond
    ((atom e) (assoc. e a))
    ((atom (car e))
     (cond
       ((eq (car e) 'quote) (cadr e))
       ((eq (car e) 'atom)  (atom   (eval. (cadr e) a)))
       ((eq (car e) 'eq)    (eq     (eval. (cadr e) a)
                                    (eval. (caddr e) a)))
       ((eq (car e) 'car)   (car    (eval. (cadr e) a)))
       ((eq (car e) 'cdr)   (cdr    (eval. (cadr e) a)))
       ((eq (car e) 'cons)  (cons   (eval. (cadr e) a)
                                    (eval. (caddr e) a)))
       ((eq (car e) 'cond)  (evcon. (cdr e) a))
       ('t (eval. (cons (assoc. (car e) a)
                        (cdr e))
                  a))))
    ((eq (caar e) 'label)
     (eval. (cons (caddar e) (cdr e))
            (cons (list (cadar e) (car e)) a)))
    ((eq (caar e) 'lambda)
     (eval. (caddar e)
            (append. (pair. (cadar e) (evlis. (cdr e) a))
                     a)))))

(defun evcon. (c a)
  (cond ((eval. (caar c) a)
         (eval. (cadar c) a))
        ('t (evcon. (cdr c) a))))

(defun evlis. (m a)
  (cond ((null. m) '())
        ('t (cons (eval. (car m) a)
                  (evlis. (cdr m) a)))))

(eval. '(
  (label subst (lambda (x y z)
    (cond ((atom z)
           (cond ((eq z y) x)
                 ('t z)))
          ('t (cons (subst x y (car z))
                    (subst x y (cdr z )))))))
  'm 'b '(a b (a b c) d)) '())
-}
