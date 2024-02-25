module Logic.LogicEx where 
import Logic.Logic

testi1 :: Logic Int
testi1 = 
   (Not (Var 6) :<=>: Not T) :&&: ((Var 1 :&&: Not (Var 2)) 
      :||: Not (Var 2 :<=>: Var 4))

testi2 :: Logic Int
testi2 = 
   (F) :&&: ((Var 1 :&&: Not (Var 2)) 
      :||: Not (Var 2 :<=>: Var 4))

testi3 :: Logic Int
testi3 = 
   (Var 6) :&&: ((Var 1 :&&: Not (Var 2)) :||: 
      Not ((Var 2 :&&: Var 4) :||: (Not (Var 2) :&&: Not (Var 4))))

testi4 :: Logic Int
testi4 = 
   (Var 6) :&&: ((Var 1 :&&: Not (Var 2)) :||: 
      (Not (Var 2 :&&: Var 4) :&&: Not (Not (Var 2) :&&: Not (Var 4))))

testi5 :: Logic Int
testi5 = 
   (Var 6) :&&: ((Var 1 :&&: Not (Var 2)) :||: 
      ((Not (Var 2) :&&: Not (Var 4)) :&&: Not (Not (Var 2) :&&: Not (Var 4))))



