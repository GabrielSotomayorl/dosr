#' Encuesta CASEN 2022 — subconjunto de variables clave
#'
#' Subconjunto de la Encuesta de Caracterización Socioeconómica Nacional (CASEN)
#' 2022, publicada por el Ministerio de Desarrollo Social y Familia de Chile.
#' Contiene todas las observaciones de la encuesta original y las variables
#' necesarias para ilustrar el uso de las funciones del paquete `dosr`.
#'
#' @format Un `data.frame` con 202.231 filas y 18 variables:
#' \describe{
#'   \item{expr}{Factor de expansión regional.}
#'   \item{varstrat}{Estratos de varianza (variable de estratificación para el diseño muestral).}
#'   \item{varunit}{Conglomerados de varianza (unidad primaria de muestreo).}
#'   \item{region}{Región (haven_labelled, 16 categorías).}
#'   \item{area}{Área: 1 = Urbano, 2 = Rural (haven_labelled).}
#'   \item{sexo}{Sexo: 1 = Hombre, 2 = Mujer (haven_labelled).}
#'   \item{edad}{Edad en años.}
#'   \item{pobreza}{Situación de pobreza por ingresos: 1 = Pobreza extrema,
#'     2 = Pobreza no extrema, 3 = Fuera de la pobreza (haven_labelled).}
#'   \item{ytotcorh}{Ingreso total corregido del hogar (pesos chilenos).}
#'   \item{activ}{Condición de actividad: 1 = Ocupados, 2 = Desocupados,
#'     3 = Inactivos (haven_labelled).}
#'   \item{r8a}{\emph{¿Se preocupó por no tener suficientes alimentos por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8b}{\emph{¿No pudo comer alimentos saludables y nutritivos por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8c}{\emph{¿Comió poca variedad de alimentos por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8d}{\emph{¿Tuvo que dejar de desayunar, almorzar, tomar once o cenar por dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8e}{\emph{¿Comió menos de lo que pensaba que debía comer por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8f}{\emph{¿Se quedó sin alimentos por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8g}{\emph{¿Sintió hambre y no comió por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8h}{\emph{¿Dejó de comer todo un día por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#' }
#' @source Ministerio de Desarrollo Social y Familia, Chile.
#'   \url{https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen}
"casen_2022"


#' Encuesta CASEN 2024 — subconjunto de variables clave
#'
#' Subconjunto de la Encuesta de Caracterización Socioeconómica Nacional (CASEN)
#' 2024, publicada por el Ministerio de Desarrollo Social y Familia de Chile.
#' Contiene todas las observaciones de la encuesta original y las variables
#' necesarias para ilustrar el uso de las funciones del paquete `dosr`.
#'
#' @format Un `data.frame` con 218.367 filas y 18 variables:
#' \describe{
#'   \item{expr}{Factor de expansión regional.}
#'   \item{varstrat}{Estratos de varianza (variable de estratificación para el diseño muestral).}
#'   \item{varunit}{Conglomerados de varianza (unidad primaria de muestreo).}
#'   \item{region}{Región (haven_labelled, 16 categorías).}
#'   \item{area}{Área: 1 = Urbano, 2 = Rural (haven_labelled).}
#'   \item{sexo}{Sexo: 1 = Hombre, 2 = Mujer (haven_labelled).}
#'   \item{edad}{Edad en años.}
#'   \item{pobreza}{Situación de pobreza por ingresos: 1 = Pobreza extrema,
#'     2 = Pobreza no extrema, 3 = Fuera de la pobreza (haven_labelled).}
#'   \item{ytotcorh}{Ingreso total corregido del hogar (pesos chilenos).}
#'   \item{activ}{Condición de actividad: 1 = Ocupados, 2 = Desocupados,
#'     3 = Inactivos (haven_labelled).}
#'   \item{r8a}{\emph{¿Se preocupó por no tener suficientes alimentos por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8b}{\emph{¿No pudo comer alimentos saludables y nutritivos por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8c}{\emph{¿Comió poca variedad de alimentos por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8d}{\emph{¿Tuvo que dejar de desayunar, almorzar, tomar once o cenar por dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8e}{\emph{¿Comió menos de lo que pensaba que debía comer por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8f}{\emph{¿Se quedó sin alimentos por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8g}{\emph{¿Sintió hambre y no comió por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#'   \item{r8h}{\emph{¿Dejó de comer todo un día por falta de dinero?}
#'     (0 = No, 1 = Sí).}
#' }
#' @source Ministerio de Desarrollo Social y Familia, Chile.
#'   \url{https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen}
"casen_2024"
