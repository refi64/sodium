import Control.Monad
import Control.Applicative
main = do
           let message = "Hello, world!"
           (putStrLn) (message)
           (return) (())