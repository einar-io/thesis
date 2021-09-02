-- Example vals
let scalar :     i32 = 1i32
let array  :   []i32 = [1,2,3,4]
let matrix : [][]i32 = [[1,2,3,4]
                       ,[1,2,3,4]
                       ,[1,2,3,4]
                       ,[1,2,3,4]]
let tuple = (matrix, array)

type r = f32

------------LIBRARY
-- id is in futhark already
---- tuple operations
let dupe a    = (a,a)
let fst (a,_) = a
let snd (_,b) = b
let fliptup (a,b) = (b,a)
let tup f g = (f,g)

---- function operations
let comp f g x = f (g x) --sequential application of two funs
let para f g (a,b) = (f a, g b) --apply two funs to tuple of vals

let sv (s:r) v = map (*s) v --scale vector
let scalarprod s z : r = s*z


-- naming scheme: general function, underscore first-order, underscore second-order

-- sperging:
let pmap f x y = map (f x) y

let outer_0_0 : r->r->r = (*)
let outer_0_1 = pmap outer_0_0
let outer_0_2 = pmap outer_0_1

let outer_1_0 = flip outer_0_1
let outer_1_1 = pmap outer_1_0
let outer_1_2 = pmap outer_1_1

let outer_2_0 = flip outer_0_2
let outer_2_1 = pmap outer_2_0
let outer_2_2 = pmap outer_2_1

let contract = (reduce (+) 0.0f32)
let contr_1_1 x y = map2 (*) x y |> contract
let contr_2_2 xx yy = map (\x -> map (\y -> contr_1_1 x y) <| transpose yy) xx

let contr_1_2 x yy = map (\y -> contr_1_1 x y) <| transpose yy
let contr_2_1 xx y = map (\x -> contr_1_1 y x) <| xx
-- TODO: how do you do matrix-vector or vector-matrix mult? are these middle-stages between dotprod and matmult?
-- is it just 'transpose the second arg'

-- assume that 'transpose' reverses the order of indices for the generalization to work?

--let megaflatten [n][m][p] (x: [n][m][p]r) = map (flatten) x

let dotprod = contr_1_1
let matmult = contr_2_2
