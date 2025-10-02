# dosr 0.2.3

## NUEVAS FUNCIONALIDADES PRINCIPALES

*   Se incorporó la nueva función `obs_cuantil()` para calcular cuantiles (mediana por defecto) con las mismas capacidades de desagregación, criterios de fiabilidad, pruebas de significancia y reportes en Excel que `obs_media()`.
*   Se añadió la función `obs_total()` para estimar totales ponderados con idéntico flujo de trabajo (significancia, fiabilidad y reportes en Excel) que las utilidades existentes.
*   Se incorporó la función `obs_ratio()` para estimar razones ponderadas (numerador/denominador) replicando la lógica de filtrado, desagregaciones, significancia y reportes de las demás herramientas `obs_*`.

## MEJORAS Y CORRECCIONES

*   Todas las funciones `obs_*` ejecutadas en paralelo heredan automáticamente la opción global `survey.lonely.psu`, evitando errores en estratos con una sola PSU al usar `parallel = TRUE`.

# dosr 0.2.2

## NUEVA FUNCIONALIDAD: PERFILADO RÁPIDO DE VARIABLES BINARIAS

*   Se ha añadido una nueva función `multi_bin()` al paquete.
*   Esta función está diseñada para el análisis exploratorio rápido de **múltiples variables dicotómicas (0/1)** dentro de un **único diseño de encuesta**.
*   Calcula estimaciones de proporción (presentadas como porcentajes por defecto), errores estándar, N expandido (de los "1s"), N muestral (de los "1s") y criterios de fiabilidad.
*   Permite desagregaciones simples (no cruzadas) a través del argumento `des`, generando hojas de reporte separadas.
*   Incluye argumentos para personalizar el número de decimales de la estimación y del error estándar por separado (`decimales` y `decimales_se`).
*   Genera un reporte en Excel con una hoja consolidada y hojas de formato profesional, siguiendo el estilo de `obs_prop` y `obs_media`.

## MEJORAS INTERNAS

*   Se ha movido el operador helper `%||%` a un archivo de utilidades (`R/utils.R`) para mejorar la estructura del paquete y seguir las mejores prácticas, eliminando código ejecutable del nivel superior de los scripts de funciones.

# dosr 0.2.1

## MEJORAS DE USABILIDAD

*   Se ha añadido un nuevo argumento `dir` a `obs_prop()` y `obs_media()`.
*   Este argumento permite al usuario especificar el directorio de destino donde se guardará el archivo Excel.
*   El valor por defecto es `dir = "output"`, manteniendo la compatibilidad con el comportamiento de versiones anteriores. La función crea el directorio si este no existe.

# dosr 0.2.0

## FUNCIONALIDAD MAYOR: PRUEBAS DE SIGNIFICANCIA ESTADÍSTICA

*   Se ha introducido una nueva funcionalidad para calcular y reportar pruebas de significancia estadística, controlada por el nuevo argumento `sig` (Booleano, por defecto `FALSE`) en `obs_prop()` y `obs_media()`.
*   Cuando `sig = TRUE`, las hojas de formato en los reportes de Excel ahora incluyen tablas adicionales con los p-values de las siguientes comparaciones:
    1.  **Test Intra-Anual:** Una matriz que compara todas las categorías de una desagregación simple entre sí, para cada año.
    2.  **Test Contra Último Año:** Compara la estimación de cada categoría contra la del año anterior (disponible cuando se procesan múltiples diseños).
    3.  **Test Contra Estimación Nacional:** Compara la estimación de cada categoría contra el total nacional de ese mismo año.
*   Los cálculos se realizan mediante un test t de Student que considera las estimaciones, los errores estándar y los grados de libertad de cada grupo, asegurando la validez estadística para diseños muestrales complejos.
*   Se ha creado un nuevo módulo interno (`R/significance.R`) con una lógica robusta y defensiva para manejar todos los casos de cálculo y prevenir errores con datos vacíos o casos límite.

# dosr 0.1.5

## MEJORAS DE FORMATO Y USABILIDAD

*   Se ha añadido el argumento `usar_etiqueta_var` (Booleano, por defecto `TRUE`) a `obs_prop()` y `obs_media()`. Si está activo, se utiliza la etiqueta de la variable de interés (extraída con el paquete `labelled`) como título del indicador en los reportes de Excel.
*   En `obs_prop()`, el argumento `porcentaje = TRUE` ahora multiplica las estimaciones y errores estándar por 100 directamente en los datos, en lugar de depender del formato de celda de Excel. Esto mejora la portabilidad y consistencia de los resultados.

## CORRECCIONES

*   Se ha solucionado un bug que causaba que la columna `fiabilidad` apareciera en blanco para las categorías sin casos generadas por `tidyr::complete()`. Ahora se muestra correctamente como "Sin casos".
*   Se ha corregido una inconsistencia en la columna `nivel` para las categorías sin casos, asegurando que siempre muestre los nombres de las variables de desagregación.
*   El paquete ahora pasa `R CMD check` sin `NOTE`s, gracias a la correcta declaración de variables globales y la importación explícita de todas las funciones necesarias.

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
