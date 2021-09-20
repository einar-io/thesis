# Source Code for Thesis Project.
Source code for master thesis by Ulrik Elmelund and Einar Rasmussen. The report
can be found here: [report](https://github.com/einar-io/thesis-report/).

## Purpose
When the code is compiled with `make`, the resulting executable takes as input on `stdin` a Linear Function
as defined under `LFun` in `Types.hs` and a value, as defined under `Val`, and applies the function to the value.

This is useful, because the derivatives produced by some auto differentiation
algortims can be expressed as linear functions.  Thus, by optimizing the
expression and producing an executable, that utilizes a GPU, the computation
can be sped up.  One way to think of the executable is as a _Virtual GPGPU
Co-Processor_ for derivative calculations when doing auto differentiation.

## relevant projects
1. [`caddie`](https://github.com/diku-dk/caddie/blob/master/README.md) produces the input programs for our code.
2. [`tail2futhark`](https://github.com/henrikurms/tail2futhark) produces Futhark code like our code be able to do.


