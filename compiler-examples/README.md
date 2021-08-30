* lmaplib.fut defines some general functions needed for every compiled program.
* Compilation should work in this manner: recurse through FUN tree, 'write' out the inner funs first, and get their UNIQUE_ID as a reference. Left-first.
-- this way every LFUN generates a short line of code. It may be desirable that even every VAL should also define this, so that each line of code is just a value or a partial application.
* The final line of code is the actual application of the function to the argument ARG.

*** To try it out:
>'cd compiler-examples'
>'futhark repl'
>':load ex1.fut'
>'main'
<[168.0f32, 336.0f32]
