# MOVIE - Multi-Omics VIsualization of Estimated contributions
MOVIE provides a framework for evaluating variance classification methods using multi-omics data.  Using data segmentation, this framework aims to identify the consistency and the extent of overfitting of multi-omics methods.   


Installing MOVIE
================
MOVIE is available as an R package which can be installed using `devtools` as follows.
```{r}
library(devtools)
devtools::install_github("mccabes292/movie")
```

Vignettes
=========
Please consult our [main vignette](https://htmlpreview.github.io/?https://github.com/mccabes292/movie/blob/master/vignettes/movie.html) for guidance on how to generate "contribution" and "comparison" plots.  You can also view our [Cross Validation Analysis Vignette](https://htmlpreview.github.io/?https://github.com/mccabes292/movie/blob/master/inst/long_vignettes/CVAnalysisWorkflow.html) for an explanation on how to conduct an appropriate analysis before utilizing the `MOVIE` R package.
