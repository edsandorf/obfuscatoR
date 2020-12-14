<!-- badges: start -->

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-last-release/obfuscatoR)](https://cran.r-project.org/package=obfuscatoR)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/obfuscatoR)](http://www.r-pkg.org/pkg/obfuscatoR)
[![R build status](https://github.com/edsandorf/obfuscatoR/workflows/R-CMD-check/badge.svg)](https://github.com/edsandorf/obfuscatoR/actions)
<!-- badges: end -->

# obfuscatoR
Chorus et al. 2019 put forward the hypothesis that sometimes when people make choices they wish to hide their true motivation from a potential onlooker. The obfuscatoR package allows researchers to easily create experimental designs to test the obfuscation hypothesis, i.e. when properly incentivized are people able to obfuscate?

*References*: [Chorus et al., 2021, Obfuscation maximization-based decision-making: Theory, methodology and first empirical evicence, Mathematical Social Sciences, 109, 28-44](https://www.sciencedirect.com/science/article/pii/S0165489620300913)


# Installing the package

The package is available from CRAN.

```
install.packages("obfuscatoR")
```

You can install the most recent version of the package from [GitHub](https://github.com/edsandorf/obfuscatoR). 

```
devtools::install_github('edsandorf/obfuscatoR')
```

Once you have installed the package using either of the approaches above, go ahead and load it using the standard library function in R. 

```
library(obfuscatoR)
```

# Citing the package
To cite this package: 

```
citation("obfuscatoR")
```

# How to contribute to obfuscatoR?
1. Go to GitHub and create an account if you don't have one.
2. Fork the project and clone it locally on your computer. Make sure that the repository is synced remotely before you move on to the next step.
3. Create a new branch for each bug fix or new feature you want to add.
4. Do the work and write a descriptive commit message. If you have added a new feature, please contribute documentation and tests. 
5. Push the changes to your remote repository.
6. Create a new pull request for each bug fix or new feature added.
7. Respond to any code review feedback.


This list is based on a great post on [how to contribute](https://akrabat.com/the-beginners-guide-to-contributing-to-a-github-project/) to a github project. 

In order keep everything readable and maintainable, please adhere to the code style. For details, please see the [R chapter](http://r-pkgs.had.co.nz/r.html) of 'R packages' by Hadley Wickham.

# Acknowledgments
This research has received funding from the European Research Council by means of ERC-Consolidator grant 724431-BEHAVE. Erlend Dancke Sandorf acknowledges funding from the the Jan Wallander and Tom Hedelius foundation, and the Tore Browaldh foundation, Sweden (Grant No. B2015-0490:1) and the European Union's Horizon 2020 research and innovation program under the Marie Sklodowska-Curie grant agreement 793163-INSPiRE. We are grateful to Teodora Szep for testing and commenting on earlier versions of the package. Any remaining errors are the sole responsibility of the authors. 
