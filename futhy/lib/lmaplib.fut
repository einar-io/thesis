type r = f32
type int = i64
let add : r->r->r = (+)
let mul : r->r->r = (*)

------------LIBRARY
---- tuple operations
let dupe a    = (a,a)
let fst (a,_) = a
let snd (_,b) = b
let flip f x = (\y -> f y x)

let comp f g x = f (g x) --sequential application of two funs
let para f g (a,b) = (f a, g b) --apply two funs to tuple of vals

let toss_dummy_const f (_,a) = f a

let pass_consts_comp f g ((x,y),z) = f (x,(g (y,z)))
let pass_consts_para f g ((x,y),(a,b)) = (f (x,a), g (y,b))

let constPassingMap f (x,y) = map (\yi -> f (x,yi)) y

let constPassingMap2 f (x,y) = map f (zip x y)

let unzipmap2 f = map2 (\x y -> f (x,y))

let mktuple x y = (x,y )-- for partial application

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

let inner_1_1 v u = reduce (+) 0.0f32 (map2 (*) v u)
let inner_1_2 v m = map (\u -> inner_1_1 v u) m
let inner_2_1 m u = map (\v -> inner_1_1 v u) <| transpose m
let inner_2_2 m1 m2 = map (\v -> map (\u -> inner_1_1 v u)  <| transpose m2) m1

let errorfunction_1_1 v u = let a =  map2 (-) v u in inner_1_1 a a

-- -- -- neg
let neg_0 : r->r = (\x -> -x)
let neg_1 = map neg_0
let neg_2 = map neg_1
let neg_3 = map neg_2

-- -- --reduce
let applyAsTuple f x y = f (x,y)

let rep1 n ne = replicate n ne
let rep2 n m ne = rep1 n (rep1 m ne)
let rep3 n m p ne = rep2 n m (rep1 p ne)

let reduce_h 't (rel : [](int, int)) vals ne (k : int) f : [k]t =
	let (srcs, is) = unzip rel
	let output = replicate k ne
	let as = map (\i -> if i < length vals then vals[i] else ne) srcs
	in reduce_by_index output (applyAsTuple f) ne is as

let reduce_1 (rel : [](int, int)) (k : int) (vals : []r) : [k]r =
	reduce_h rel vals 0.0 k add_0_0

let reduce_2 [n] (rel : [](int, int)) (k : int) (vals : [][n]r) : [k][n]r =
	reduce_h rel vals (rep1 n 0.0) k add_1_1

let reduce_3 [m][n] (rel : [](int, int)) (k : int) (vals : [][m][n]r) : [k][m][n]r =
	reduce_h rel vals (rep2 m n 0.0) k add_2_2
