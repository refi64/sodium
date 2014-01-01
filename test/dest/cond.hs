import Control.Monad
import Control.Applicative
main = do
           a <- ((read) <$> (getLine)) :: IO Int
           () <- if ((a) == (0)) || ((a) == (1))
                 then do
                          (putStrLn) ("Nice choice!")
                          (return) (())
                 else do
                          () <- if (a) < (0)
                                then do
                                         (putStrLn) ("You went negative")
                                         (return) (())
                                else do
                                         (putStrLn) ("WOW SUCH BIG NUMBER")
                                         (return) (())
                          (return) (())
           (return) (())