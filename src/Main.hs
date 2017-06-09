{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF inlitpp #-}
```html_header
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
```

```haskell hide top
import Inliterate.Import
import Graphics.Plotly hiding (text)
import Graphics.Plotly.Lucid
```
# TF/HaskellDo

A really simple TensorFlow demo in HaskellDo.

```haskell top
import Control.Monad (replicateM, replicateM_, zipWithM)
import System.Random (randomIO)
import Data.Function ((&))

import qualified TensorFlow.Core as TF
import qualified TensorFlow.GenOps.Core as TF
import qualified TensorFlow.Gradient as TF
import qualified TensorFlow.Ops as TF

import Lens.Micro
import Data.Text (Text)
```

```haskell top
fit :: [Float] -> [Float] -> IO (Float, Float)
fit xData yData = TF.runSession $ do
    -- Create tensorflow constants for x and y.
    let x = TF.vector xData
        y = TF.vector yData
    -- Create scalar variables for slope and intercept.
    w <- TF.initializedVariable 0
    b <- TF.initializedVariable 0
    -- Define the loss function.
    let yHat = (x `TF.mul` w) `TF.add` b
        loss = TF.square (yHat `TF.sub` y)
    -- Optimize with gradient descent.
    let learningRate = 0.01
        iterations = 400
    trainStep <- gradientDescent learningRate loss [w, b]
    replicateM_ iterations (TF.run trainStep)
    -- Return the learned parameters.
    (TF.Scalar w', TF.Scalar b') <- TF.run (w, b)
    return (w', b')

gradientDescent :: Float
                -> TF.Tensor TF.Build Float
                -> [TF.Tensor TF.Ref Float]
                -> TF.Session TF.ControlNode
gradientDescent alpha loss params = do
    let applyGrad param grad =
            TF.assign param (param `TF.sub` (TF.scalar alpha `TF.mul` grad))
    TF.group =<< zipWithM applyGrad params =<< TF.gradients loss params

pointsCount = 30
```

```haskell do
-- Generate data where `y = x*4 + 8`.
xData <- replicateM pointsCount randomIO
let yData = [x*4 + 8 | x <- xData]
    minX = min xData
    maxX = max xData

-- Add some noise to y values
yNoiseData <- replicateM pointsCount randomIO
let noiseScale = 0.5
    noisyY = zipWith (+) yData (map (*noiseScale) yNoiseData)
(w, b) <- fit xData noisyY
```

```haskell eval
show (w, b)
```

```haskell eval
plotly "p2" [points (aes & x .~ fst & y .~ snd) (zip xData noisyY), line (aes & x .~ fst & y .~ snd) [(minimum xData, (minimum xData * w + b)), (maximum xData, (maximum xData * w + b))]]
```
