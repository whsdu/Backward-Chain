-- {-# LANGUAGE ExistentialQuantification #-}
module Space.Dung where 

type AF = ([AR],Attacks)

-- data AR = forall a . Eq a => AR {getAr :: a}
type AR = String 
type Attack = (AR,AR)
type Attacks = [Attack]

demoARs :: [String]
demoARs = ["A","B","C", "D", "E", "F", "L", "G", "O"]

demoAttacks :: [(String, String)]
demoAttacks =
    [ ("A", "B")
    , ("L", "A")
    , ("B", "C")
    , ("C", "F")
    , ("F", "D")
    , ("B", "E")
    , ("E", "G")
    , ("G", "O")
    , ("O", "D")
    ]

testARs :: [String]
testARs = ["A","B","C", "D", "E", "F","G"]
testAttacks :: [(String, String)]
testAttacks = 
    [ ("A", "B")
    , ("B", "A")
    , ("A", "C")
    , ("B", "C")
    , ("C", "D")
    , ("D", "E")
    , ("G", "E")
    , ("E", "F")
    ]

cicleARs :: [String]
cicleARs = ["A","B"]
cicleAttacks :: [(String, String)]
cicleAttacks = 
    [ ("A","B")
    , ("B","A")
    ]

demoAF :: ([String], [(String, String)])
demoAF = (demoARs,demoAttacks)

testAF :: ([String], [(String, String)])
testAF = (testARs, testAttacks)

cicleAF :: ([String], [(String, String)])
cicleAF = (cicleARs, cicleAttacks)


isAttack ::  AF -> AR -> AR -> Bool
isAttack (ars, attacks) a b = 
    a `elem` ars &&
    b `elem` ars &&
    (a,b) `elem` attacks

-- | argument set s attack argument a 
isSetAttack ::  AF -> [AR] -> AR -> Bool
isSetAttack af s b = or $ flip (isAttack af) b <$> s

-- | Any two elements of argument set sars do not attack each other
isConflictFree :: AF -> [AR] -> Bool
isConflictFree af sars = 
    not $ or  
            [ isAttack af a b 
            | a <- sars
            , b <- sars 
            ] 

isAcceptable :: ([AR], Attacks) -> [AR] -> AR -> Bool
isAcceptable af@(ars,_) [] a = 
    not $ isSetAttack af ars a  
isAcceptable af@(ars, attacks) s a =                    
    let 
        attackers = [ b | b <- ars, (b,a) `elem` attacks]
    in 
        if attackers == [] 
            then True                                   
            else and [ or (flip (isAttack af) a <$> s) | a <- attackers]

isAdmissible af s = 
        isConflictFree af s 
        &&
        and [isAcceptable af s a | a <- s]

faf :: AF -> [AR] -> [AR]
faf af@(ars, _) supportARs = [ a | a <- ars, isAcceptable af supportARs a]

