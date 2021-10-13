
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sensataDataProg

<!-- badges: start -->
<!-- badges: end -->

This package is a WIP that has the functions needed to clean and
manipulate data created with the Sensata platform.

Este paquete es un WIP que tendrá las funciones necesarias para limpiar
y manipular los datos de la plataforma Sensata.

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

This is a basic example which shows you how to create a dictionary of
the microdata:

Este es un ejemplo básico que muestra cómo crear un diccionario de los
microdatos:

``` r
library(sensataDataProg)

dictGenerator(dat = pathOfMongoCsvFile, f = pathToSaveDictionaries )
```

## TO-DO

Why is there a line of tidyverse, in the namespace
