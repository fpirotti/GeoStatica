# GeoStatica
Web app based on shiny for getting a better understanding of methods for geostatistical analysis.


## Usage
Load one of the data sets available and add random or regular samples in the area - area can be defined by zooming on the window. 
Load different type of analyses and visualize results and explanations. 

### Semivariogram
For example the main dataset is on temperature over a region which has mountains on the northern part. Notably temperature and altitude are correlated, therefore a simple variogram without covariates will not provide a reasonable empirical semivariogram. If altitude is provided as covariate, then the semivariogram is reasonable and a model can be applied. 

Didactically it is intersting to note that providing the X+Y as covariates also results in a reasonable semivarioagram that can be modelled, but this is because there is a correlation between the altitude and the Y coordinate. 

Changing values and showing in real time the results gives trainees a better understanding of the effects of choosing parameters to fit a semivariogram, and the implications in Kriging interpolation.


