# Type inference (Hindley–Milner)

Algorithm W is the best known algorithm for implementing Hindley-Milner type inference.
Algorithm W is simple but has the property of intermingling two separate processes: constraint generation and solving.

Our implementation is based on Heeren B. J., Hage J., & Swierstra S. D. (2002) [Generalizing Hindley-Milner type inference algorithms](https://www.researchgate.net/profile/Jurriaan-Hage/publication/2528716_Generalizing_Hindley-Milner_Type_Inference_Algorithms/links/09e415108dfe6e7cbe000000/Generalizing-Hindley-Milner-Type-Inference-Algorithms.pdf).
In this approach constraints on types are first collected by bottom-up traversal ([constraints generation](gen)), and then solved independently ([constraints solving](solve)).

To see type inference in action please check [test.ml](test/test.ml)

## Effect types

WIP
