import Control.Monad (guard)
import Control.Exception (try, evaluate, SomeException)

dumbDivide :: Int -> Int -> Int
dumbDivide x y = x `div` y

divide :: Int -> Int -> Maybe Int
divide x y = do
    guard (y /= 0)
    return (x `div` y)
    --if y==0 then Nothing
    --else Just(x `div` y)

dumbMain :: IO ()
dumbMain = do
    let p = dumbDivide 1 0
    print p

    putStrLn "And I'm still standing..."

main :: IO()
main = do
    case divide 1 0 of
        Just a  -> print a
        Nothing -> putStrLn "Division by zero"

    putStrLn "And I'm still standing..."

notdumbMain :: IO ()
notdumbMain = do
    result <- try (evaluate (dumbDivide 1 0)) :: IO (Either SomeException Int)
    case result of
        Left ex -> putStrLn $ "Caught exception: " ++ show ex
        Right val -> print val

    putStrLn "And I'm still standing..."
