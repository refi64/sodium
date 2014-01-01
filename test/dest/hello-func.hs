import Control.Monad
import Control.Applicative
main = do
           let s = message
           (putStrLn) (s)
           (return) (())
message = let
              message'_ = "Hello, world!"
          in message'_