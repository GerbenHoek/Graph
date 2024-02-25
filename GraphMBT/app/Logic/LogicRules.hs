module Logic.LogicRules where
import FSMMaker.Rules 
import Logic.Logic



--commutativity

switchAnd :: Rule (Logic a)
switchAnd = rule "switchAnd" f
   where f l = 
            case l 
              of l1 :&&: (l2 :&&: l3) -> [l2 .&&. l1 .&&. l3]
                 l1 :&&: l2 -> [l2 :&&: l1]
                 _ -> [] 

switchOr :: Rule (Logic a)
switchOr = rule "switchOr" f
   where f l = 
            case l 
              of l1 :||: (l2 :||: l3) -> [l2 .||. l1 .||. l3]
                 l1 :||: l2 -> [l2 .||. l1]
                 _ -> [] 

switchEq :: Rule (Logic a)
switchEq = rule "switchEq" f
   where f l = 
            case l 
              of l1 :<=>: (l2 :<=>: l3) -> [l2 .<=>. l1 .<=>. l3]
                 l1 :<=>: l2 -> [l2 .<=>. l1]
                 _ -> [] 



--Rules for constants 
andTrue :: Rule (Logic a)
andTrue = rule "andTrue" f
   where f l = 
            case l 
              of l' :&&: T -> [l']
                 T :&&: l' -> [l']
                 _ -> [] 

andTrueB :: Rule (Logic a)
andTrueB = buggyRule "p andTrue -> True" f
   where f l = 
            case l 
              of l' :&&: T -> [T]
                 T :&&: l' -> [T]
                 _ -> [] 

orTrue :: Rule (Logic a)
orTrue = rule "orTrue" f
   where f l = 
            case l 
              of l' :||: T -> [T] 
                 T :||: l' -> [T]       
                 _ -> [] 

orTrueB :: Rule (Logic a)
orTrueB = buggyRule "p orTrue -> p" f
   where f l = 
            case l 
              of l' :||: T -> [l']
                 T :||: l' -> [l']         
                 _ -> [] 

notTrue :: Rule (Logic a)
notTrue = rule "notTrue" f
   where f l = 
            case l 
              of Not T -> [F]
                 _ -> [] 

andFalse :: Rule (Logic a)
andFalse = rule "andFalse" f
   where f l = 
            case l 
              of l' :&&: F -> [F]
                 F :&&: l' -> [F]
                 _ -> [] 

andFalseB :: Rule (Logic a)
andFalseB = buggyRule "p andFalse -> p" f
   where f l = 
            case l 
              of l' :&&: F -> [l']
                 F :&&: l' -> [l']
                 _ -> [] 

orFalse :: Rule (Logic a)
orFalse = rule "orFalse" f
   where f l = 
            case l 
              of l' :||: F -> [l'] 
                 F :||: l' -> [l']           
                 _ -> [] 

orFalseB :: Rule (Logic a)
orFalseB = buggyRule "orFalse -> False" f
   where f l = 
            case l 
              of l' :||: F -> [F] 
                 F :||: l' -> [F]             
                 _ -> [] 

notFalse :: Rule (Logic a)
notFalse = rule "notFalse" f
   where f l = 
            case l 
              of Not F -> [T]
                 _ -> [] 

implTrue :: Rule (Logic a)
implTrue  = rule "implTrue" f
   where f l = 
            case l 
              of a :=>: T -> [T] 
                 _ -> []

implTrueB :: Rule (Logic a)
implTrueB  = buggyRule "a implTrue -> a" f
   where f l = 
            case l 
              of a :=>: T -> [T] 
                 _ -> []

implFalse :: Rule (Logic a)
implFalse = rule "implFalse" f
   where f l = 
            case l 
              of a :=>: F -> [Not a] 
                 _ -> []

implFalseB :: Rule (Logic a)
implFalseB = buggyRule "a implFalse -> F" f
   where f l = 
            case l 
              of a :=>: F -> [F] 
 
                 _ -> []
trueImpl :: Rule (Logic a)
trueImpl = rule "trueImpl" f
   where f l = 
            case l 
              of T :=>: b -> [b] 
                 _ -> []

trueImplB :: Rule (Logic a)
trueImplB = buggyRule "trueImpl b -> True" f
   where f l = 
            case l 
              of T :=>: b -> [T] 
                 _ -> []

falseImpl :: Rule (Logic a)
falseImpl = rule "falseImpl" f
   where f l = 
            case l 
              of F :=>: b -> [T] 
                 _ -> [] 

falseImplB :: Rule (Logic a)
falseImplB = buggyRule "falseImpl b -> False" f
   where f l = 
            case l 
              of F :=>: b -> [F] 
                 _ -> [] 

trueEquiv :: Rule (Logic a)
trueEquiv = rule "trueEquiv" f
   where f l = 
            case l 
              of T :<=>: b -> [b] 
                 a :<=>: T -> [a] 
                 _ -> [] 

trueEquivB :: Rule (Logic a)
trueEquivB = buggyRule "trueEquiv b -> True" f
   where f l = 
            case l 
              of T :<=>: b -> [T] 
                 a :<=>: T -> [T] 
                 _ -> [] 

falseEquiv :: Rule (Logic a)
falseEquiv = rule "falseEquiv" f
   where f l = 
            case l 
              of F :<=>: b -> [Not b] 
                 a :<=>: F -> [Not a]
                 _ -> [] 

falseEquivB :: Rule (Logic a)
falseEquivB = buggyRule "falseEquiv b -> False" f
   where f l = 
            case l 
              of F :<=>: b -> [F] 
                 a :<=>: F -> [F] 
                 _ -> [] 

--Definitions
implDef :: Rule (Logic a)
implDef = rule "implDef" f
   where f l = 
            case l 
              of a :=>: b -> [(Not a) .||. b] 
                 _ -> []

equivDef :: Rule (Logic a)
equivDef = rule "equivDef" f
   where 
      f l = 
         case l 
           of a :<=>: b -> [(a .&&. b) .||. ((Not a) .&&. (Not b))]
              _ -> []

equivDefB :: Rule (Logic a)
equivDefB = rule "equivDef forget Not true" f
   where 
      f l = 
         case l 
           of a :<=>: b -> [(a .&&. b)]
              _ -> []

--Negations
notNot :: Rule (Logic a)
notNot = rule "notNot" f
   where 
      f l = 
         case l 
           of Not (Not a) -> [a]
              _ -> []

notNotB :: Rule (Logic a)
notNotB = buggyRule "notNot -> Not" f
   where 
      f l = 
         case l 
           of Not (Not a) -> [Not a]
              _ -> []

notB :: Rule (Logic a)
notB = buggyRule "not p -> p" f
   where 
      f l = 
         case l 
           of Not a -> [a]
              _ -> []

deMorganAnd :: Rule (Logic a)
deMorganAnd = rule "deMorganAnd" f
   where 
      f l = 
         case l 
           of Not (a:&&:b) -> [(Not a) .||. (Not b)]
              _ -> []

deMorganAndB1 :: Rule (Logic a)
deMorganAndB1 = buggyRule "deMorganAnd forget nots" f
   where 
      f l = 
         case l 
           of Not (a:&&:b) -> [a .||. b]
              _ -> []

deMorganAndB2 :: Rule (Logic a)
deMorganAndB2 = buggyRule "deMorganAnd keep and" f
   where 
      f l = 
         case l 
           of Not (a:&&:b) -> [(Not a) .&&. (Not b)]
              _ -> []

deMorganOr :: Rule (Logic a)
deMorganOr = rule "deMorganOr" f
   where 
      f l = 
         case l 
           of Not (a:||:b) -> [(Not a) .&&. (Not b)]
              _ -> []

deMorganOrB1 :: Rule (Logic a)
deMorganOrB1 = buggyRule "deMorganOr forget nots" f
   where 
      f l = 
         case l 
           of Not (a:||:b) -> [a .&&. b]
              _ -> []

deMorganOrB2 :: Rule (Logic a)
deMorganOrB2 = buggyRule "deMorganOr keep or" f
   where 
      f l = 
         case l 
           of Not (a:||:b) -> [(Not a) .||. (Not b)]
              _ -> []

--Distribution
andOverOr :: Rule (Logic a)
andOverOr = rule "andOverOr" f
   where 
      f l = 
         case l 
           of a :&&: (b :||: c) -> [(a .&&. b) .||. (a .&&. c)]
              _ -> []

andOverOrB :: Rule (Logic a)
andOverOrB = buggyRule "andOverOr interchange && and ||" f
   where 
      f l = 
         case l 
           of a :&&: (b :||: c) -> [(a .||. b) .&&. (a .||. c)]
              _ -> []





--Tautologies 
implTaut :: Eq a => Rule (Logic a)
implTaut = rule "implTaut" f
   where 
      f l = 
         case l 
           of a :=>: b | a == b  -> [T]
              _ -> []

equivTaut :: Eq a =>Rule (Logic a)
equivTaut = rule "equivTaut" f
   where 
      f l = 
         case l 
           of a :<=>: b | a == b  -> [T]
              _ -> []

orTaut :: Eq a =>Rule (Logic a)
orTaut = rule "orTaut" f
   where 
      f l = 
         case l 
           of a :||: Not b | a == b -> [T]
              _ -> []


andSame :: Eq a =>Rule (Logic a)
andSame = rule "andSame" f
   where 
      f l = 
         case l 
           of a :&&: b | a == b -> [a]
              _ -> []

orSame :: Eq a =>Rule (Logic a)
orSame = rule "andSame" f
   where 
      f l = 
         case l 
           of a :||: b | a == b -> [a]
              _ -> []

--Contradictions
andContr :: Eq a => Rule (Logic a)
andContr = rule "andContr" f
   where 
      f l = 
         case l 
           of a :&&: Not b | a == b -> [F]
              _ -> []

equivContr:: Eq a => Rule (Logic a)
equivContr = rule "equivContr" f
   where 
      f l = 
         case l 
           of a :<=>: Not b | a == b -> [F]
              _ -> []

--Make a standardized DNF
stDNF :: Int -> Rule (Logic Int)
stDNF n = rule "stDNF" f
   where 
      f l = [dnf n l]
         

