import Control.Monad
import Control.Applicative
main = do
           n <- ((read) <$> (getLine)) :: IO Int
           let n' = (factorial) (n)
           (putStrLn) ((show) (n'))
           (return) (())
factorial n = let
                  factorial'_ = 1
                  factorial'_' = (((foldl) (\factorial'_ const'i -> let
                                                                        factorial'_' = (factorial'_) * (const'i)
                                                                    in factorial'_')) (factorial'_)) ([1..n])
              in factorial'_'