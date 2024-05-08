import GHC.Base (ZeroBitRep)
import Control.Monad ((>=>))

type Name = String
type DataBase = [(Name, Name)]

fathers, mothers :: DataBase
fathers = [("Bill", "John"),
           ("Ann", "Marcus"),
           ("Daniel", "Santiago"),
           ("John", "Ben"),
           ("Jane", "Noah"),
           ("Alice", "Oliver"),
           ("Santiago", "Henry")]
mothers = [("Bill", "Jane"),
           ("John", "Alice"),
           ("Jane", "Dorothy"),
           ("Ann", "Alice"),
           ("Alice", "Mary"),
           ("Santiago", "Lupita")]

getM, getF :: Name -> Maybe Name
getM person = lookup person mothers
getF person = lookup person fathers

granmas :: Name -> Maybe (Name, Name)
granmas person = do
    gmm <- case getM person of
        Just mother -> do
            case getM mother of
                Just grandma -> return grandma
                Nothing -> return "Unknown"
        Nothing -> Just "Unknown"

    gmf <- case getF person of
        Just father -> do
            case getM father of
                Just grandma -> return grandma
                Nothing -> return "Unknown"
        Nothing -> return "Unknown"

    return (gmm, gmf)

main :: IO()
main = do
    putStrLn $ "Bill's grandmothers: " ++ show (granmas "Bill")
    putStrLn $ "Ann's grandmothers: " ++ show (granmas "Ann")
    putStrLn $ "Daniel's grandmothers: " ++ show (granmas "Daniel")



checkGranpas1 :: Name -> Maybe Bool
checkGranpas1 person = do
    f <- getF person
    gff <- getF f

    m <- getM person
    gfm <- getF m

    return True -- Just True

checkGranpas2 :: Name -> Maybe Bool
checkGranpas2 person =
    getF person >>= getF >>
    getM person >>= getF >>
    return True

    
checkGranpas3 :: Name -> Maybe Bool
checkGranpas3 person =
    (getM >=> getF) person >>
    (getF >=> getF) person >>
    return True