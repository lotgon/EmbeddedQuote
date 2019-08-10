# EmbeddedQuote
Encode array of rates.

Model has 4 convolutional layers and 3 dense layers.There are ~1.5M parameters for input of 3600 points. It takes at least 24 hours to train it on CPU.

Example of embedding 3600 points to 32 integers.
![Example](https://raw.githubusercontent.com/lotgon/EmbeddedQuote/master/Rplot.png)

There are a lot of issues with hyper parameters and architecture for rates encoding.
The main resolved issue:
1. The best result with loss = 'mean_absolute_error'. Squared error did not allow to converge.
2. layer_conv_2d_transpose layer should not be too "wide". At first, I added too many conv layers with stride, as a result, residuals was much larger on last part of generated array.
The unresolved issue:
Large error on the edge of arrays. I believe the problem is connected with padding="same". I would try to add dummy values at the start and the end of array.

