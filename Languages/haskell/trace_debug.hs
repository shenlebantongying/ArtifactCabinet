import qualified Debug.Trace

fact n = let n' = Debug.Trace.trace ("fact: " ++ (show n)) n in
         if n == 1 then 1 else  n' * fact (n' - 1)

-- >>> fact 10
-- 3628800
