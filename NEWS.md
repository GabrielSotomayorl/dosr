# dosr 0.1.2

## MEJORAS DE FORMATO Y USABILIDAD

*   Se ha rediseñado por completo el formato de las hojas de reporte en Excel para seguir un estándar de publicación:
    *   Los títulos de cada bloque de métricas (ej. "Estimación", "Error estándar") ahora se muestran en una única celda combinada horizontalmente sobre su respectiva tabla.
    *   La estructura superior de cada hoja de formato ha sido estandarizada a: Fila vacía, "Nombre indicador" (en negrita), "Tipo de cálculo", Fila vacía.
    *   Se han corregido los estilos de borde y negrita que no se aplicaban correctamente en versiones anteriores.
    *   Se han añadido las tildes a los títulos de los bloques ("Estimación", "Población expandida", etc.).
*   Se ha añadido un nuevo argumento `verbose` a `obs_prop()` y `obs_media()`. Por defecto es `TRUE` y muestra mensajes de progreso por etapas en la consola, informando al usuario sobre el estado de los cálculos.
*   Los nombres de las hojas de reporte en Excel han sido simplificados (ej. de `Formato_nac` a `2_nac`) para ser más concisos.

## CORRECCIONES

*   Se ha corregido el orden de las columnas en el `data.frame` que devuelven las funciones a R. Ahora la columna `nivel` aparece consistentemente antes que las variables de desagregación, coincidiendo con el formato de la hoja "Consolidado" en Excel.


# dosr 0.1.1

## CORRECCIONES Y MEJORAS

*   Se ha solucionado un error fatal en `obs_prop` que impedía su ejecución.
*   Se ha corregido el problema que causaba filas vacías en los reportes de `obs_media` para combinaciones sin casos.
*   Se ha implementado la generación de la pestaña de reporte formateado para los resultados a nivel nacional en `obs_media`.
*   Se ha corregido el ordenamiento en la hoja de consolidado para respetar la jerarquía de las desagregaciones (nacional -> 1 variable -> 2 variables, etc.).
*   Se ha eliminado un `warning` innecesario que aparecía al solicitar un reporte nacional formateado.

# dosr 0.1.0

* Initial submission.
