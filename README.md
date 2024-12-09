# MineriadeTextos
Proyecto Mineria de textos:

Este proyecto se centra en la creación de una aplicación en Shiny para realizar un análisis de reseñas de productos y visualizaciones interactivas de datos. Las principales funcionalidades del servidor incluyen:

### Renderizado de R Markdown: Si el archivo Rmd no existe, el servidor lo genera como un archivo HTML. Además, ofrece la opción de descargar el archivo Rmd.

### Visualización de datos: Se proporcionan resúmenes y estructuras de los datos, incluyendo tablas interactivas con opciones para copiar, exportar a CSV, Excel, PDF y otras funciones de visualización.

### Análisis exploratorio de datos (EDA):

Gráficos de la cantidad de reseñas por departamentos y por rangos de edad, agrupados por departamentos.
Gráficos de palabras más frecuentes en las reseñas, diferenciados por tipo (positivas o negativas).
Generación de bigramas (pares de palabras) y visualización de los más frecuentes, con especial enfoque en términos positivos o negativos.

### Simulación de Sentimientos:

Análisis de sentimientos mediante la clasificación de reseñas como positivas o negativas utilizando el diccionario de sentimientos "afinn".
Generación de una nube de palabras interactiva para visualizar términos frecuentes en las reseñas.
Cálculo de la ganancia neta basada en el análisis de los sentimientos de las reseñas, sumando o restando dinero según el tipo de reseña.

### Interactividad:

Permite filtrar datos y visualizaciones según el departamento seleccionado por el usuario.
Gráficos dinámicos e interactivos mediante plotly para una visualización más atractiva y comprensible.
En resumen, el proyecto permite realizar un análisis detallado de las reseñas de productos, visualizando tendencias, sentimientos y palabras clave.
