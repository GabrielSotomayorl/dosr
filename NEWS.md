# dosr 0.1.1

# dosr 0.1.0

* Initial CRAN submission.

# dosr 0.1.1

## CORRECCIONES Y MEJORAS

*   Se ha solucionado un error fatal en `obs_prop` que impedía su ejecución.
*   Se ha corregido el problema que causaba filas vacías en los reportes de `obs_media` para combinaciones sin casos.
*   Se ha implementado la generación de la pestaña de reporte formateado para los resultados a nivel nacional en `obs_media`.
*   Se ha corregido el ordenamiento en la hoja de consolidado para respetar la jerarquía de las desagregaciones (nacional -> 1 variable -> 2 variables, etc.).
*   Se ha eliminado un `warning` innecesario que aparecía al solicitar un reporte nacional formateado.
