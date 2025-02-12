# Probabilistic Programming in Scala

This is my exploration of probabilistic programming in Scala. This implemented as
an embedded domain specific language in Scala. Several inference algorithms have
been implemented for this DSL and more will come. The implementation has several
goals:

1. Learn about probabilistic programming and what problems it can solve; and,
2. Find out what inference algorithms work best for what problems and programs; and,
3. Explore deep learning in the context of probabilistic programming and how it
   relates to building models for artificial intelligence.

## Work in Progress

- Deep learning using stochastic gradient descent. Using the reverse mode 
  automatic differentiation to find gradients.
- More examples including probabilistic networks and probabilistic neural networks.
- More tests and some test rework. Due to the probabilistic nature of execution, 
  tests will pass with certain probability instead of just pass/fail deterministically
  (this is a problem).

## Todo

- Some continuation-passing-style (Cats `ContT` based types) and an importance 
  sampling inference module.
- Use scalafix to clean up everything.

## License, Warranty, and Copyright

- This work is Copyright Thomas Pécseli 2025.
- There is no warranty or fit-for-particular-purpose or anything like that. The
  code is provided as-is and that's it.
- There is no license to use this code for anything. It is provided as inspiration.
  (This might change in the future to MIT- or GPL- license or something like that).

## References

- "An Introduction to Probabilistic Programming", Jan-Willem van de Meent et al,
  October 2021, pre-print at [arXiv](https://arxiv.org/abs/1809.10756)
