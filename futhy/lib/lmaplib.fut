-- Example vals
let scalar :     i32 = 1i32
let array  :   []i32 = [1,2,3,4]
let matrix : [][]i32 = [[1,2,3,4]
                       ,[1,2,3,4]
                       ,[1,2,3,4]
                       ,[1,2,3,4]]
type r = f32

------------LIBRARY
---- tuple operations
let dupe a    = (a,a)
let fst (a,_) = a
let snd (_,b) = b
let flip f x y = f y x

---- function operations
let comp f g x = f (g x) --sequential application of two funs
let para f g (a,b) = (f a, g b) --apply two funs to tuple of vals

let sv (s:r) v = map (*s) v --scale vector
let scalarprod s z : r = s*z

let applytotup f (x,y) = f x y

let add (x: f32) (y: f32) = x + y

let lplus_0 f g v = (applytotup add (f v, g v))
let lplus_1 f g v = (applytotup (map2 add) (f v, g v))
let lplus_2 f g v = (applytotup (map2 (map2 add)) (f v, g v))

-- naming scheme: general function, underscore first-order, underscore second-order

-- sperging:
let mapr f x y = map (f x) y
let mapl f x y = map (flip f y) x

let outer_0_0 : r->r->r = (*)
let outer_0_1 = mapr outer_0_0
let outer_0_2 = mapr outer_0_1
let outer_0_3 = mapr outer_0_2

let outer_1_0 = mapl outer_0_0
let outer_2_0 = mapl outer_1_0
let outer_3_0 = mapl outer_2_0

let outer_1_1 = mapl outer_0_1
let outer_1_2 = mapl outer_0_2
let outer_1_3 = mapl outer_0_3

let outer_2_1 = mapl outer_1_1
let outer_2_2 = mapl outer_1_2
let outer_2_3 = mapl outer_1_3




let contract = (reduce (+) 0.0f32)
let contr_1_1 x y = map2 (*) x y |> contract
let contr_2_2 xx yy = map (\x -> map (\y -> contr_1_1 x y) <| transpose yy) xx

let contr_1_2 x yy = map (\y -> contr_1_1 x y) <| transpose yy
let contr_2_1 xx y = map (\x -> contr_1_1 y x) <| xx
-- TODO: how do you do matrix-vector or vector-matrix mult? are these middle-stages between dotprod and matmult?
-- is it just 'transpose the second arg'

-- assume that 'transpose' reverses the order of indices for the generalization to work?

--let megaflatten [n][m][p] (x: [n][m][p]r) = map (flatten) x

let dotprod_1_1 = contr_1_1
let matrixmult_2_2 = contr_2_2
