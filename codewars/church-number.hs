type Lambda a = (a -> a)
type Cnum a = Lambda a -> Lambda a

churchAdd :: Cnum a -> Cnum a -> Cnum a
churchAdd c1 c2 = \x y -> c1 x (c2 x y)

churchMul :: Cnum a -> Cnum a -> Cnum a
churchMul c1 c2 = \x -> c1 (c2 x)

--Extra credit: Why is the type signature different?
churchPow :: Cnum a -> (Cnum a -> Cnum a) -> Cnum a
churchPow cb ce = ce cb
