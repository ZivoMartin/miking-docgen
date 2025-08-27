---
sidebar_class_name: hidden
---

# CorePPL Tutorial

In this tutorial, you will learn how to:

- Run a simple CorePPL program

- Understand the fundamental language constructs of CorePPL

- Implement a simple inference algorithm within the framework

- Understand the structure to enable the implementation of advanced algorithms and transformations

Note that this tutorial assumes a basic understanding of Bayesian theory and familiarity with other probabilistic programming languages, such as WebPPL, Pyro, or Anglican.

## What is CorePPL?

CorePPL is an intermediate language intended to develop domain-specific probabilistic programming languages. Although it has a concrete syntax and it is possible to write probabilistic models as CorePPL models, the intention is not to be an end-user language. Examples of domain-specific languages that are built using CorePPL are [ProbTime](https://github.com/miking-lang/ProbTime), a probabilistic modeling language for real-time systems, and [TreePPL](https://treeppl.org/), a universal probabilistic programming language for modeling of phylogenetics.

CorePPL is built on top of the [Miking](https://miking.org/) framework, a general compiler framework for constructing compilers for domain-specific languages. That is, CorePPL is a light extension of Miking Core (MCore) with a few specific constructs handling probabilistic inference. Besides these new constructs, the syntax and semantics of CorePPL are the same as MCore.

The basic constructs are `assume`, `weight`, `observe` and `infer`. In the rest of this tutorial, we will discuss these constructs, give an intuition of their semantics, and discuss how to implement new inference algorithms using these constructs.


## Running a simple CorePPL program

```
Some code
```


## Adding a simple inference algorithm


## Exending with advanced algorithms and transformations

### Basic MCMC algorithms

### Dynamic Delayed Sampling