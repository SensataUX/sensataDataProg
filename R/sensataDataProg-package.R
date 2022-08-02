#' sensataDataProg: from Mongo to R
#'
#' @description
#' This package has the functions needed to clean and manipulate data created with the Sensata platform.
#'
#' Este paquete tiene las funciones necesarias para limpiar y manipular los datos de la plataforma Sensata.
#'
#' @section sensataDataProg basic functions:
#' El paquete provee funciones para los pasos más importantes en la depuración de los datos:
#' - dictGenerator:  crear un diccionario
#' - cleanCols: limpiar las columnas
#' - scrubRows: depurar los datos
#' - makeFactors: crear factores para el análisis
#' - selectCols: dejar las columnas finales para el cliente
#'
#' @section Additional functions:
#' Además incluye funciones para otros procesamientos de datos que solemos hacer:
#' - scrubPeaks: eliminar picos atípicos
#' - translateFactors: traducir datos de un idioma a otro
#' - extractMetaData: Extraer metadatos para archivo de seguimiento
#' - conjoint2Tasks: Function to create conjoint data by rounds
#' - delExtraCar: Function to eliminate extra characters for conjoint
#' - dividehun_fun and dividethou_fun: helper functions to divide by a hundred and a thousand
#' - from0to100: to rescale, usually likert, to a 0 to a 100 scale
#' - not_all_na: Selecter for columns that are ALL NA
#' - not_any_na: Selecter for columns that have ANY NA
#' - scrubPeaks: Function to scrub atypical peaks to control manipulation attempts using medium param
#' - scrubPeaksSource: Function to scrub atypical peaks to control manipulation attempts using source param
#'
#' @source \url{https://github.com/SensataUx/sensataDataProg}
#'
#'
#' @docType package
#' @name sensataDataProg
NULL
#> NULL
