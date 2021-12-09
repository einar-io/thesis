type r = f32
type int = i64
let add : r->r->r = (+)

------------LIBRARY
---- tuple operations
let dupe 't (a: t) : (t,t) = (a,a)
let fst 't1 't2 ((a,_): (t1,t2)) : t1 = a
let snd 't1 't2 ((_,b): (t1,t2)) : t2 = b

let comp 't1 't2 't3 (f: t2 -> t3) (g: t1 -> t2) (x: t1) : t3 = f (g x) --sequential application of two funs
let para 't1 't2 't3 't4 (f: t1 -> t2) (g: t3 -> t4) ((a,b): (t1, t3)) : (t2, t4) = (f a, g b) --apply two funs to tuple of vals

let zipAndMap 't1 't2 't3 [n] (f: (t1, t2) -> t3) (xs: [n]t1) (ys: [n]t2) : *[n]t3 = map f (zip xs ys)

--- const-extraction specific functions

let ignoreDummyVal 't1 't2 't3 (f: t2 -> t3)( (_,a): (t1, t2)) : t3 = f a

let constPassingComp 't1 't2 't3 't4 't5 (f: (t1, t4) -> t5) (g: (t2, t3) -> t4) (((x,y),z): ((t1, t2), t3)) : t5 = f (x,(g (y,z)))
let constPassingPara 't1 't2 't3 't4 't5 't6 (f: (t1, t3) -> t5)  (g: (t2, t4) -> t6) (((x,y),(a,b)): ((t1, t2), (t3, t4))) :  (t5, t6) = (f (x,a), g (y,b))
let constPassingMap  't1 't2 't3 [n] (f: (t1, t2) -> t3) ((x,y): (t1, [n]t2))    : *[n]t3 = map (\yi -> f (x,yi)) y
let constPassingMap2 't1 't2 't3 [n] (f: (t1, t2) -> t3) ((x,y): ([n]t1, [n]t2)) : *[n]t3 = map2 (\xi yi -> f (xi,yi)) x y

-- add
let add_0_0 ((x,y): (r,r)) : r = (add) x y
let add_1_1 [n] 	  ((x,y): ([n]r,      [n]r)) 	   : [n]r       = (map2 add) x y
let add_2_2 [n][m]    ((x,y): ([n][m]r,   [n][m]r))    : [n][m]r    = (map2 (map2 add)) x y
let add_3_3 [n][m][p] ((x,y): ([n][m][p]r,[n][m][p]r)) : [n][m][p]r = (map2 (map2 (map2 add))) x y

-- lplus
let lplus_h 't1 't2 't3 't4 (plus: t2 -> t3 -> t4) (f1: t1 -> t2) (f2: t1 -> t3) (v: t1) : t4 = plus (f1 v) (f2 v)
let lplus_0 't 			 (f1: t -> r)          (f2: t -> r)          (v: t) : r          = lplus_h (add) f1 f2 v
let lplus_1 't [n] 		 (f1: t -> [n]r)       (f2: t -> [n]r)       (v: t) : [n]r       = lplus_h (map2 add) f1 f2 v
let lplus_2 't [n][m]    (f1: t -> [n][m]r)    (f2: t -> [n][m]r)    (v: t) : [n][m]r    = lplus_h (map2 (map2 add)) f1 f2 v
let lplus_3 't [n][m][p] (f1: t -> [n][m][p]r) (f2: t -> [n][m][p]r) (v: t) : [n][m][p]r = lplus_h (map2 (map2 (map2 add))) f1 f2 v

-- outer
let mapr 't1 't2 't3 [n] (f : t1 -> t2 -> t3) (x: t1) (y: [n]t2) : *[n]t3 = map (f x) y
let mapl 't1 't2 't3 [n] (f : t1 -> t2 -> t3) (x: [n]t1) (y: t2) : *[n]t3 = map (flip f y) x

let outer_0_0 (x: r) (y: r) 				 = x * y

let outer_0_1 (x: r) (y: []r)     : *[]r     = mapr outer_0_0 x y
let outer_0_2 (x: r) (y: [][]r)   : *[][]r   = mapr outer_0_1 x y
let outer_0_3 (x: r) (y: [][][]r) : *[][][]r = mapr outer_0_2 x y

let outer_1_0 (x: []r)     (y: r) : *[]r     = mapl outer_0_0 x y
let outer_2_0 (x: [][]r)   (y: r) : *[][]r   = mapl outer_1_0 x y
let outer_3_0 (x: [][][]r) (y: r) : *[][][]r = mapl outer_2_0 x y

let outer_1_1 (x: []r) (y: []r)		: *[][]r = mapl outer_0_1 x y
let outer_1_2 (x: []r) (y: [][]r)   : *[][][]r = mapl outer_0_2 x y
let outer_1_3 (x: []r) (y: [][][]r) : *[][][][]r = mapl outer_0_3 x y

let outer_2_1 (x: [][]r) (y: []r) 	  : *[][][]r = mapl outer_1_1 x y
let outer_2_2 (x: [][]r) (y: [][]r)   : *[][][][]r = mapl outer_1_2 x y
let outer_2_3 (x: [][]r) (y: [][][]r) : *[][][][][]r = mapl outer_1_3 x y


let dotprod_1_1 (v : []r) (u : []r) : r = reduce (+) 0.0f32 (map2 (*) v u)
let vecMatProd_1_2 (v : []r) (m : [][]r) : []r = map (\u -> dotprod_1_1 v u) m
let matVecProd_2_1 (m : [][]r) (u : []r) : []r = map (\v -> dotprod_1_1 v u) <| transpose m
let matrixMult_2_2 (a : [][]r) (b : [][]r) : [][]r = map (\v -> map (\u -> dotprod_1_1 v u)  <| transpose b) a

-- used to calculate the neural network example
let lossfunction_1_1 [n] (v : [n]r) (u : [n]r) = let a = map2 (-) v u in dotprod_1_1 a a

-- -- -- neg
let neg_0 (x :r)       : r        = (-x)
let neg_1 (x :[]r)     : *[]r     = map neg_0 x
let neg_2 (x :[][]r)   : *[][]r   = map neg_1 x
let neg_3 (x :[][][]r) : *[][][]r = map neg_2 x

-- -- --reduce
let applyAsTuple 't1 't2 't3 (f: (t1, t2) -> t3) (x: t1) (y: t2) : t3 = f (x,y)

let rep1 't (n: int) 				   (ne: t) : *[n]t       = replicate n ne
let rep2 't (n: int) (m: int) 		   (ne: t) : *[n][m]t    = rep1 n (rep1 m ne)
let rep3 't (n: int) (m: int) (p: int) (ne: t) : *[n][m][p]t = rep2 n m (rep1 p ne)

let reduce_h 't (rel : [](int, int)) (vals: []t) (ne: t) (k : int) f : [k]t =
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
