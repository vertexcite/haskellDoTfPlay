{-# OPTIONS_GHC -F -pgmF inlitpp #-}
```haskell hide top
import Inliterate.Import
```
```html_header
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
```

# TF/HaskellDo

A really simple TensorFlow demo in HaskellDo.

```haskell top
import Control.Monad (replicateM, replicateM_)
import System.Random (randomIO)
import Data.Function ((&))

import qualified TensorFlow.Core as TF
import qualified TensorFlow.GenOps.Core as TF
import qualified TensorFlow.Minimize as TF
import qualified TensorFlow.Ops as TF hiding (initializedVariable)
import qualified TensorFlow.Variable as TF

import Graphics.Plotly hiding (text)
import Graphics.Plotly.Lucid
import Lens.Micro
import Data.Text (Text, pack)
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
    let yHat = (x `TF.mul` TF.readValue w) `TF.add` TF.readValue b
        loss = TF.square (yHat `TF.sub` y)
    -- Optimize with gradient descent.
    let learningRate = 0.01
        iterations = 400
    trainStep <- TF.minimizeWith (TF.gradientDescent learningRate) loss [w, b]
    replicateM_ iterations (TF.run trainStep)
    -- Return the learned parameters.
    (TF.Scalar w', TF.Scalar b') <- TF.run (TF.readValue w, TF.readValue b)
    return (w', b')

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
plotly (pack "p2") [points (aes & x .~ fst & y .~ snd) (zip xData noisyY), line (aes & x .~ fst & y .~ snd) [(minimum xData, (minimum xData * w + b)), (maximum xData, (maximum xData * w + b))]]
```
