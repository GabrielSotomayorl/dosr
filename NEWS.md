# dosr 0.1.4

## CAMBIO FUNDAMENTAL EN CRITERIOS DE FIABILIDAD

*   Se ha reescrito por completo la lógica para clasificar la fiabilidad de las estimaciones (`Fiable`, `Poco Fiable`, `No Fiable`) para alinearse con los nuevos estándares de calidad.
*   **Para proporciones:**
    *   La clasificación ahora distingue entre variables dicotómicas y no dicotómicas, infiriendo esto automáticamente del número de niveles de la variable.
    *   Se aplican umbrales de tamaño muestral diferenciados (`n_universo` para dicotómicas, `n_mues` para no dicotómicas).
    *   Se introduce un nuevo argumento `es_var_estudio` (Booleano, por defecto `FALSE`) que permite relajar los criterios de tamaño muestral para variables clave del instrumento.
*   **Para medias:**
    *   La lógica de clasificación también ha sido actualizada para seguir el nuevo flujo de decisión basado en grados de libertad, tamaño muestral, el argumento `es_var_estudio` y el coeficiente de variación (CV).

## MEJORAS INTERNAS Y CORRECCIONES

*   Se ha añadido el argumento `es_var_estudio` a las funciones `obs_prop()` y `obs_media()`.

# dosr 0.1.3

## NUEVAS FUNCIONALIDADES

*   Se ha añadido un nuevo argumento `multi_des` a `obs_prop()` y `obs_media()` para controlar el comportamiento de las desagregaciones:
    *   Si `multi_des = TRUE` (valor por defecto), el paquete calcula todas las combinaciones posibles de las variables de desagregación, como en versiones anteriores.
    *   Si `multi_des = FALSE`, el paquete solo calcula las desagregaciones simples (por cada variable en `des` de forma individual), lo que acelera significativamente los cálculos y es ideal para análisis exploratorios o cuando se usan muchas variables.
*   Se ha implementado un "fusible de seguridad" para proteger al usuario. Si se solicitan más de 3 variables de desagregación con `multi_des = TRUE`, la función se detendrá con un error informativo, previniendo ejecuciones excesivamente largas y posibles cuelgues de la sesión.

## MEJORAS INTERNAS

*   Se ha refactorizado la lógica interna para pasar el parámetro `multi_des` al motor de cálculo, permitiendo la nueva funcionalidad.

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
