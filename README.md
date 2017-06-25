# haskellDoTfPlay
## A simple Haskell.do with Tensorflow demo

Haskell do demo project using TensorFlow.

Open the project using HaskellDo, or just use `stack build && stack exec run-test > demo.html && open demo.html`.

Based on https://github.com/tensorflow/haskell/blob/release-0.1.0.2/tensorflow-ops/tests/RegressionTest.hs

![Screenshot](https://files.gitter.im/theam/haskell-do/jR0L/image.png)


# Instructions
* Get haskell/tensorflow working (see https://github.com/tensorflow/haskell ).  (Note: you actually only need tensorflow itself, since this example uses Haskell-Do, which relies on stack, which should take care of the haskell/tensorflow side, but I found that using the dependency installers from https://github.com/tensorflow/haskell simplified things. )
* Clone this repo (i.e. `git clone https://github.com/vertexcite/haskellDoTfPlay.git` )
* Run this example stand-alone: 
```
    cd haskellDoTfPlay
    stack build && stack exec run-test > demo.html && open demo.html
```
* If that worked, you should see the haskell code and the plot in your browser.
* Get haskell.do (see http://haskell.do/ )
* Start haskell.do, open this project and run it (i.e. press Ctrl-Enter (Cmd-Enter on Mac) )
* If that worked, you should see a similar result to the screenshot above.
