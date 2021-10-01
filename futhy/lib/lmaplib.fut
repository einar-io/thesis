type r = f32
let add : r->r->r = (+)
let mul : r->r->r = (*)

------------LIBRARY
---- tuple operations
let dupe a    = (a,a)
let fst (a,_) = a
let snd (_,b) = b
let flip f x y = f y x

---- lmap operations
let comp f g x = f (g x) --sequential application of two funs
let para f g (a,b) = (f a, g b) --apply two funs to tuple of vals

-- this is almost like monads
-- derive the pattern and make it a higher order function
-- the rest becomes trivial

----- near sperg perfection:
-- add
let add_0_0 (x,y) = (add) x y
let add_1_1 (x,y) = (map2 add) x y
let add_2_2 (x,y) = (map2 (map2 add)) x y
let add_3_3 (x,y) = (map2 (map2 (map2 add))) x y

-- lplus
let lplus_h h f g v = h (f v) (g v)
let lplus_0 = lplus_h (add)
let lplus_1 = lplus_h (map2 add)
let lplus_2 = lplus_h (map2 (map2 add))
let lplus_3 = lplus_h (map2 (map2 (map2 add)))

-- outer
let mapr f x y = map (f x) y
let mapl f x y = map (flip f y) x

let outer_0_0 = mul
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


-- no-transpose contraction (consider matmul as a composite operation of a transposition and a ntcont_2_2)
let aptr f x y = f x y |> reduce (+) 0.0f32


let ntcont_1_1 = aptr (map2 (*)) --dot prod

let ntcont_1_2 = mapr ntcont_1_1
let ntcont_1_3 = mapr ntcont_1_2

let ntcont_2_1 = mapl ntcont_1_1
let ntcont_2_2 = mapr ntcont_2_1
let ntcont_2_3 = mapr ntcont_2_2

let ntcont_3_1 = mapl ntcont_2_1
let ntcont_3_2 = mapl ntcont_2_2
let ntcont_3_3 = mapr ntcont_3_2

-- contract
let cont_aux f x y = f x <| transpose y

--let cont_0_0 = mul
--let cont_0_1 = ntcont_0_1
--let cont_0_2 = aptr outer_0_1

let cont_1_1 = ntcont_1_1
let cont_2_1 = ntcont_2_1
let cont_3_1 = ntcont_3_1

let cont_1_2 = cont_aux ntcont_1_2
let cont_2_2 = cont_aux ntcont_2_2
let cont_3_2 = cont_aux ntcont_3_2

let cont_1_3 = cont_aux ntcont_1_3
let cont_2_3 = cont_aux ntcont_2_3
let cont_3_3 = cont_aux ntcont_3_3

-- -- --simple versions
let dotprod_1_1 = aptr (map2 (*))
let matrixmult_2_2 xss yss = map (\xs -> map (cont_1_1 xs) <| transpose yss) xss

-- let cont_2_2 xss yss = map (\xs -> map (cont_1_1 xs) <| transpose yss) xss -- matmul

-- assumes that 'transpose' reverses the order of indices for the generalization to work?

-- -- -- neg
let neg_0 : r->r = (\x -> -x)
let neg_1 = map neg_0
let neg_2 = map neg_1
let neg_3 = map neg_2

-- -- --reduce
let reduce_1 [n][m] (rel : [m](i64, i64)) (vals : [n]r) =
	let ne = 0.0
	let (srcs, is) = unzip rel
	let k = reduce i64.max 0 is
	let output = replicate (k+1) ne
	let as = map (\i -> if i < length vals then vals[i] else ne) srcs
	in reduce_by_index output (+) ne is as
