% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensataDataProg-package.R
\docType{package}
\name{sensataDataProg}
\alias{sensataDataProg}
\title{sensataDataProg: from Mongo to R}
\source{
\url{https://github.com/SensataUx/sensataDataProg}
}
\description{
This package has the functions needed to clean and manipulate data created with the Sensata platform.

Este paquete tiene las funciones necesarias para limpiar y manipular los datos de la plataforma Sensata.
}
\section{sensataDataProg basic functions}{

El paquete provee funciones para los pasos más importantes en la depuración de los datos:
\itemize{
\item dictGenerator:  crear un diccionario
\item cleanCols: limpiar las columnas
\item scrubRows: depurar los datos
\item makeFactors: crear factores para el análisis
\item selectCols: dejar las columnas finales para el cliente
}
}

\section{Additional functions}{

Además incluye funciones para otros procesamientos de datos que solemos hacer:
\itemize{
\item scrubPeaks: eliminar picos atípicos
\item translateFactors: traducir datos de un idioma a otro
\item extractMetaData: Extraer metadatos para archivo de seguimiento
\item conjoint2Tasks: Function to create conjoint data by rounds
\item delExtraCar: Function to eliminate extra characters for conjoint
\item dividehun_fun and dividethou_fun: helper functions to divide by a hundred and a thousand
\item from0to100: to rescale, usually likert, to a 0 to a 100 scale
\item not_all_na: Selecter for columns that are ALL NA
\item not_any_na: Selecter for columns that have ANY NA
\item scrubPeaks: Function to scrub atypical peaks to control manipulation attempts using medium param
\item scrubPeaksSource: Function to scrub atypical peaks to control manipulation attempts using source param
}
}

