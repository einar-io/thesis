-- Example vals
let scalar :     i32 = 1i32
let array  :   []i32 = [1,2,3,4]
let matrix : [][]i32 = [[1,2,3,4]
                       ,[1,2,3,4]
                       ,[1,2,3,4]
                       ,[1,2,3,4]]
let tuple = (matrix, array)

type real = f32

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

let sv (s:real) v = map (*s) v --scale vector
