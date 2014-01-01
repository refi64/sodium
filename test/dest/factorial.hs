import Control.Monad
import Control.Applicative
main = do
           n <- ((read) <$> (getLine)) :: IO Int
           let a = 1
           a' <- (((foldM) (\a const'i -> do
                                              let a' = (a) * (const'i)
                                              (return) (a'))) (a)) ([1..n])
           (putStrLn) ((show) (a'))
           (return) (())