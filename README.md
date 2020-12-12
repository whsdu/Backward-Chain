# DT examples
1. stack ghci

1. demo1 -- test basic single path query 
env1 <- parseEnv "./Examples/test/demo1.txt"
run1 = runApp env1
lMap = parseLiteralMap env
q1 = parseQueryLiteral "p1" lMap
q2 = parseQueryLiteral "!p2" lMap
run $ querySingleConclusion q1
run $ querySingleConclusion q2

1. test Ordering issue
env1 <- parseEnv "./Examples/test/demo1.txt"
env3d <- parseEnv "./Examples/test/demo3-dem.txt"
fMap = parseLiteralMap env1
qc = parseQueryLiteral "p1" fMap
p1 <- runApp env1 $ querySingleConclusion qc
p3 <- runApp env3d $ querySingleConclusion qc

1. test Preferable in demo4-eli
import Data.HashMap.Strict

env4 <- parseEnv "./Examples/test/demo4-eli.txt"
preMap =union ( (getRdPrefMap . envRdPrefMap) env4) ( (getKnwlPrefMap . envKnwlPrefMap ) env4)
fmap4 = parseLiteralMap env4
qb = parseQueryLiteral "p9" fmap4
qc = parseQueryLiteral "!p9" fmap4
rb <- runApp env4 $ querySingleConclusion qb
rc <- runApp env4 $ querySingleConclusion qc

lastLink preMap eli (head rc) (head rb)
weakestLink preMap eli (head rc) (head rb)

qd = parseQueryLiteral "p21" fmap4
qe = parseQueryLiteral "!p21" fmap4
rd <- runApp env4 $ querySingleConclusion qd
re <- runApp env4 $ querySingleConclusion qe

lastLink preMap eli (head re) (head rd)
weakestLink preMap eli (head re) (head rd)

1. test Preferable in demo4-dem
env4 <- parseEnv "./Examples/test/demo4-dem.txt"
preMap =union ( (getRdPrefMap . envRdPrefMap) env4) ( (getKnwlPrefMap . envKnwlPrefMap ) env4)
fmap4 = parseLiteralMap env4
qb = parseQueryLiteral "p9" fmap4
qc = parseQueryLiteral "!p9" fmap4
rb <- runApp env4 $ querySingleConclusion qb
rc <- runApp env4 $ querySingleConclusion qc

lastLink preMap dem (head rc) (head rb)
weakestLink preMap dem (head rc) (head rb)

qd = parseQueryLiteral "p21" fmap4
qe = parseQueryLiteral "!p21" fmap4
rd <- runApp env4 $ querySingleConclusion qd
re <- runApp env4 $ querySingleConclusion qe

lastLink preMap dem (head re) (head rd)
weakestLink preMap dem (head re) (head rd)

1. test demo2-dem
env2 <- parseEnv "./Examples/test/demo2-dem.txt"
preMap =union ( (getRdPrefMap . envRdPrefMap) env2) ( (getKnwlPrefMap . envKnwlPrefMap ) env2)
fmap2 = parseLiteralMap env2
qa = parseQueryLiteral "p4" fmap2
qb = parseQueryLiteral "!p4" fmap2
rb <- runApp env2 $ querySingleConclusion qa
rc <- runApp env2 $ querySingleConclusion qb

weakestDem preMap  (head rc) (head rb)
weakestEli preMap (head rc) (head rb)

lastDem preMap (head rc) (head rb)
lastEli  preMap (head rc) (head rb)

1. test demo2-eli
env2 <- parseEnv "./Examples/test/demo2-eli.txt"
preMap =union ( (getRdPrefMap . envRdPrefMap) env2) ( (getKnwlPrefMap . envKnwlPrefMap ) env2)
fmap2 = parseLiteralMap env2
qa = parseQueryLiteral "p4" fmap2
qb = parseQueryLiteral "!p4" fmap2
ra <- runApp env2 $ querySingleConclusion qa
rb <- runApp env2 $ querySingleConclusion qb

weakestDem preMap  (head rb) (head ra)
weakestEli preMap (head rb) (head ra)

lastDem preMap (head rb) (head ra)
lastEli  preMap (head rb) (head ra)

1. test demo3-dem
env2 <- parseEnv "./Examples/test/demo3-dem.txt"
preMap =union ( (getRdPrefMap . envRdPrefMap) env2) ( (getKnwlPrefMap . envKnwlPrefMap ) env2)
fmap2 = parseLiteralMap env2
qa = parseQueryLiteral "p2" fmap2
qb = parseQueryLiteral "!p2" fmap2
ra <- runApp env2 $ querySingleConclusion qa
rb <- runApp env2 $ querySingleConclusion qb

weakestDem preMap  (head rb) (head ra)
weakestEli preMap (head rb) (head ra)

lastDem preMap (head rb) (head ra)
lastEli  preMap (head rb) (head ra)

1. test demo3-eli
env2 <- parseEnv "./Examples/test/demo3-eli.txt"
preMap =union ( (getRdPrefMap . envRdPrefMap) env2) ( (getKnwlPrefMap . envKnwlPrefMap ) env2)
fmap2 = parseLiteralMap env2
qa = parseQueryLiteral "p2" fmap2
qb = parseQueryLiteral "!p2" fmap2
ra <- runApp env2 $ querySingleConclusion qa
rb <- runApp env2 $ querySingleConclusion qb

weakestDem preMap  (head rb) (head ra)
weakestEli preMap (head rb) (head ra)

lastDem preMap (head rb) (head ra)
lastEli  preMap (head rb) (head ra)
---------------
1. use `:m` to clean the import space
1. import necessary modules:
    ```
    import qualified Space.Language as L
    import qualified Space.Argumentation as A
    import Space.Meta as M
    import qualified Utility.Language as LU
    import qualified Utility.Argumentation as AU
    import qualified Data.HashMap.Strict as Map
    import qualified GRI.GRI as GRI
    import qualified Algorithm.BC as BC
    import Data.Maybe
    import Parser.FileParser
    import Arena
    import Env
    ```
1. head (conC), name, imp, body 
    ```
    L.conC r4
    L.name r4
    L.imp r4
    L.imp r6
    L.imp a 
    L.body r4
    L.body r6
    L.body r7
    ```
1. applicable 
    ```
    r7
    LU.isApplicable r7 [c,t]
    LU.isApplicable r7 [c,t,r3,r7]
    LU.isApplicable r7 [c,r3,r6]
    ```
1. consistent
    ```
    demoLanguage
    LU.isConsistent demoLanguage
    let nb = neg b
    LU.isConsistent [a,b,c]
    LU.isConsistent [a,b,c,nb]
    ```
1. paper demo: langAL (AL) and langASG(ASG)
    ```
    let run = runApp $ paperEnv
    al <- run $ LU.langAL $ neg b
    al
    ```
    ```
    asg <- run $LU.langASG $ neg b
    asg
    ```
    ```
    asdef <- run $ BC.funcDefGen asg
    asdef
    ```
1. examples demo: 

    use `Examples/Teams/b3.txt` to generate new `env`
    ```
    env <- parseEnv (testPath ++ devFile)
    let run = runApp env
    ```
    get query `p6`
    ```
    let lm = parseLiteralMap env
    p6 = parseQueryLiteral "p6" lm 
    ```

    compute ASG and DEF
    ```
    asg <- run $ BC.funcASG p6
    def <- run $ BC.funcDefGen asg
    asg
    def
    ```