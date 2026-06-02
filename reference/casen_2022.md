# Encuesta CASEN 2022 — subconjunto de variables clave

Subconjunto de la Encuesta de Caracterización Socioeconómica Nacional
(CASEN) 2022, publicada por el Ministerio de Desarrollo Social y Familia
de Chile. Contiene todas las observaciones de la encuesta original y las
variables necesarias para ilustrar el uso de las funciones del paquete
\`dosr\`.

## Usage

``` r
casen_2022
```

## Format

Un \`data.frame\` con 202.231 filas y 18 variables:

- expr:

  Factor de expansión regional.

- varstrat:

  Estratos de varianza (variable de estratificación para el diseño
  muestral).

- varunit:

  Conglomerados de varianza (unidad primaria de muestreo).

- region:

  Región (haven_labelled, 16 categorías).

- area:

  Área: 1 = Urbano, 2 = Rural (haven_labelled).

- sexo:

  Sexo: 1 = Hombre, 2 = Mujer (haven_labelled).

- edad:

  Edad en años.

- pobreza:

  Situación de pobreza por ingresos: 1 = Pobreza extrema, 2 = Pobreza no
  extrema, 3 = Fuera de la pobreza (haven_labelled).

- ytotcorh:

  Ingreso total corregido del hogar (pesos chilenos).

- activ:

  Condición de actividad: 1 = Ocupados, 2 = Desocupados, 3 = Inactivos
  (haven_labelled).

- r8a:

  *¿Se preocupó por no tener suficientes alimentos por falta de dinero?*
  (0 = No, 1 = Sí).

- r8b:

  *¿No pudo comer alimentos saludables y nutritivos por falta de
  dinero?* (0 = No, 1 = Sí).

- r8c:

  *¿Comió poca variedad de alimentos por falta de dinero?* (0 = No, 1 =
  Sí).

- r8d:

  *¿Tuvo que dejar de desayunar, almorzar, tomar once o cenar por
  dinero?* (0 = No, 1 = Sí).

- r8e:

  *¿Comió menos de lo que pensaba que debía comer por falta de dinero?*
  (0 = No, 1 = Sí).

- r8f:

  *¿Se quedó sin alimentos por falta de dinero?* (0 = No, 1 = Sí).

- r8g:

  *¿Sintió hambre y no comió por falta de dinero?* (0 = No, 1 = Sí).

- r8h:

  *¿Dejó de comer todo un día por falta de dinero?* (0 = No, 1 = Sí).

## Source

Ministerio de Desarrollo Social y Familia, Chile.
<https://observatorio.ministeriodesarrollosocial.gob.cl/encuesta-casen>
