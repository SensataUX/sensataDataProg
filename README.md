
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sensataDataProg

<!-- badges: start -->
<!-- badges: end -->

This package has the functions needed to clean and manipulate data
created with the Sensata platform.

Este paquete tiene las funciones necesarias para limpiar y manipular los
datos de la plataforma Sensata.

## Installation

It will not be publish on CRAN, you can only install it from
[GitHub](https://github.com/) with:

No se va a publicar en CRAN, solo se puede instalar de
[GitHub](https://github.com/) con:

``` r
# install.packages("devtools")
devtools::install_github("GaborioSensata/sensataDataProg")
```

## Example

This is a basic example which shows how to prepare raw data from sensata
platform (data included as an example in the package):

Este es un ejemplo básico que muestra cómo preparar datos raw de la
plataforma sensata (datos incluidos como ejemplo en el paquete):

``` r
library(sensataDataProg)

rawData <- sensataDataProg::sensataExample

Dict <- dictGenerator(
  df = rawData,
  questionPrefix = "",
  forceOrdered = "q_AB_NI_01"
)

Dict$options <- sub("\\//.*", "", Dict$options)


intData <- cleanCols(df = rawData,
                    dictionary = Dict)

intData<- scrubRows(df = intData,
          testParamName = "test")

intData <- intData %>% makeFactors(dictionary = Dict,
                                   multChoiceText = c("Yes", "No"))
```

## TO-DO

-   Create vignettes
