# Flowpaint
A reactive live painting software. The goal is to integrate it with some FRP framework (yampa).

For now, it is:



## Things to do
- Finalise the implementation of the watercolour diffusion algorithm
- Figure out how to use `yampa` as the primary means of interaction with the mouse-events and the eventual watercolour diffusion.
    - This may result in a change in front-end, as GLUT uses its own event-loop. I am convinced that using wrappers to openGL is better than writing a new JS front-end though (after exploring the JS front end that already exists (blank-canvas), I am not convinced I have sufficient time to learn how to code a JS wrapper in Haskell with the time left).
- Performance evaluations by using some benchmarking tool in Haskell
- Speak to [professor] about provable properties within my program for evaluation, and prove these properties.
- Write the dissertation.
