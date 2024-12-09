library(gridExtra)
library(shiny)
library(shinythemes)
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(corrplot)
library(liver)
library(dplyr)
library(plotly)
library(ggcorrplot)
library(DT)
library(tidyr)
library(shinydashboard)
library(stringr) 
library(caret)
library(cluster)
library(factoextra)
library(reshape2)
library(shinythemes)
library(shinyBS)
library(openxlsx)
library(tseries)
library(highcharter)
library(TSstudio)
library(gridExtra)
library(forecast)
library(lubridate)
library(tibble)
library(tidyverse)
library(stats)
library(wordcloud2)
library(tidytext)

datos <- read.csv("womens_reviews.csv")

str(datos)

data_summary <- summary(datos)
data_str <- capture.output(str(datos))
data_table <- datatable(datos)

color <- c(
  "#A4D8E1",  # Cyan claro
  "#4DA6C4",  # Cyan medio
  "#2E93B8",  # Cyan oscuro
  "#1C7A92",  # Cyan muy oscuro
  "#0B4462" , # Cyan profundo,
  "#DAF7A6",  # Verde claro,
  "#FF6F61",  # Coral
  "#F7CAC9",  # Rosa claro
  "#B9D3C1",  # Verde agua
  "#92A8D1",  # Azul suave
  "#88B04B" ,# Verde manzana
  "#B9D3C1",  # Verde agua
  "#92A8D1",  # Azul suave
  "#82CCDD",  # Aqua
  "#C5E1A5",  # Verde claro (Mate)
  "#FFB142",  # Naranja suave
  "#BFACE2"   # Lavanda
  
)
#_________________________________EDA
#Asignar nombre al dpto "sin nombre"
datos$Class.Name[datos$Class.Name == ""] <- "Sin categoria"
datos <- datos %>%
  mutate(Department.Name = if_else(Department.Name == "", "Others", Department.Name))

data_filtrada <- datos %>% 
  group_by(Department.Name) %>% 
  summarise(frecuencia = n()) %>% 
  arrange(desc(frecuencia)) 

#para grupo de edades

breaks <- c(18, 34, 52, 100)
labels <- c("Grupo 18-34", "Grupo 35-52", "Grupo 53-100")

rev_age <- datos %>%
  mutate(Group_Age = cut(Age, breaks = breaks, labels = labels, right = FALSE)) %>%
  group_by(Group_Age) %>%
  summarise(frecuencia = n(), .groups = "drop") %>%
  arrange(desc(frecuencia))





g <- ggplot(data_filtrada, aes(x = reorder(Department.Name, -frecuencia), 
                               y = frecuencia, fill = Department.Name)) +
  geom_bar(stat = "identity") + 
  xlab("Department Name") +
  ylab("Frecuencia") +
  theme_minimal() +
  scale_fill_manual(values = color)  
g1 <- ggplotly(g)


g3 <- ggplot(rev_age, aes(x= Group_Age, y = frecuencia)) + 
  geom_bar(stat = "identity" , fill =  "#A4D8E1") +
  labs(x = "Rango Edades") + 
  theme_minimal()

g3<-ggplotly(g3)

###RECOMENDACIONES

# Convertir 'Recommended IND' a una variable categórica: positiva o negativa
datos$Recommendation_Status <- ifelse(datos$Recommended.IND == 1, "Positiva", "Negativa")

# Contar la cantidad de reseñas positivas y negativas
data_pie <- table(datos$Recommendation_Status)

# Crear el gráfico de torta interactivo con plotly
fig <- plot_ly(
  labels = names(data_pie),  
  values = data_pie,         
  type = 'pie',              
  textinfo = 'percent+label', 
  hoverinfo = 'label+percent', 
  marker = list(colors = c("#A4D8E1", "#FF6F61")) 
)

# Crear bigramas y filtrar palabras vacías
datos_bigrams <- datos %>%
  unnest_tokens(bigram, Review.Text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(
    !is.na(word1), 
    !is.na(word2),               # Filtra palabras NA explícitamente
    word1 != "NA", 
    word2 != "NA",               # Filtra palabras "NA" como texto
    !word1 %in% stop_words$word, 
    !word2 %in% stop_words$word
  ) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

datos_bigrams <- datos_bigrams %>%
  filter(!str_detect(bigram, "\\bNA\\b"))

# Definir palabras positivas y negativas
positive_words <- c("good", "great", "excellent", "amazing", "best")
negative_words <- c("bad", "poor", "terrible", "awful", "worst")

# Filtrar bigramas que contienen palabras positivas
datos_bigrams_pos <- datos_bigrams %>%
  filter(str_detect(bigram, paste(positive_words, collapse = "|")))

# Filtrar bigramas que contienen palabras negativas
datos_bigrams_neg <- datos_bigrams %>%
  filter(str_detect(bigram, paste(negative_words, collapse = "|")))

datos_bigrams_filtrados <- datos_bigrams %>%
  filter(bigram != "NA NA")

# Gráfico con los datos filtrados
b <- ggplot(datos_bigrams_filtrados[1:10, ], aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "#A4D8E1") +
  coord_flip() +  # Gira las barras horizontalmente
  labs(x = "Bigramas", y = "Frecuencia", title = "Top 10 Bigramas Filtrados") +
  theme_minimal()


# Gráfico de bigramas positivos (si hay datos)
if (nrow(datos_bigrams_pos) > 0) {
  b_pos <- ggplot(datos_bigrams_pos[1:min(10, nrow(datos_bigrams_pos)), ], aes(x = reorder(bigram, n), y = n)) +
    geom_bar(stat = "identity", fill = "#88B04B") +
    coord_flip() +
    labs(x = "Bigramas Positivos", y = "Frecuencia", title = "Top Bigramas Positivos") +
    theme_minimal()
  
  ggplotly(b_pos)
}

# Gráfico de bigramas negativos (si hay datos)
if (nrow(datos_bigrams_neg) > 0) {
  b_neg <- ggplot(datos_bigrams_neg[1:min(10, nrow(datos_bigrams_neg)), ], aes(x = reorder(bigram, n), y = n)) +
    geom_bar(stat = "identity", fill = "#FF6F61") +
    coord_flip() +
    labs(x = "Bigramas Negativos", y = "Frecuencia", title = "Top Bigramas Negativos") +
    theme_minimal()
  
  ggplotly(b_neg)
}




#--------------------------------------------------------------------------------------------------------


# Definir la interfaz de usuario
ui <- navbarPage(
  title = "MINERIA DE DATOS",
  theme = shinytheme("flatly"),
  
  # Incluir el CSS en el <head>
  tags$head(
    tags$style(HTML("
      /* FONDOS */
.dataset-background {
  background-image: url('www/fondo1.jpg'); /* Imagen en la carpeta www */
  background-size: cover; /* Cubre todo el fondo */
  background-position: center; /* Centra la imagen */
  min-height: 100vh; /* Ajusta la altura mínima según el contenido */
  width: 100vw; /* Ocupa toda la anchura de la pantalla */
  position: absolute; /* Asegura que el fondo cubra toda la pantalla */
  top: 0;
  left: 0;
  z-index: -1; /* Para que no interfiera con otros elementos */
}

/* Fondo transparente para el contenido */
.transparent-bg {
  background-image: url('fondo2.jpg'); /* Imagen en la carpeta www */
  background-size: cover; /* Cubre todo el fondo */
  background-position: center; /* Centra la imagen */
  width: 100vw; /* Ocupa toda la anchura de la pantalla */
  position: absolute; /* Asegura que el fondo cubra toda la pantalla */
  top: 0;
  left: 0;
  z-index: -1; /* Para que no interfiera con otros elementos */
}

/* FLIP CARDS */
.flip-card {
  position: relative;
  z-index: 1;
  width: 100%;
  height: 700px;
  perspective: 1000px;
  margin: 10px; /* Añadir margen para separación */
}

/* Asegúrate de que las flip cards estén bien contenidas */
.flip-card-inner {
  position: relative;
  width: 100%;
  height: 100%;
  transition: transform 0.8s;
  transform-style: preserve-3d;
}

/* Flip on hover */
.flip-card:hover .flip-card-inner {
  transform: rotateY(180deg);
}

.flip-card-front, .flip-card-back {
  position: absolute;
  width: 100%;
  height: 100%;
  backface-visibility: hidden;
  z-index: 2;
} 

.flip-card-front {
  background-color: #bbb;
  color: black;
  font-weight: bold;
  display: flex;
  justify-content: center;
  align-items: center;
  font-family: 'Arial', sans-serif; /* Fuente de las flip cards */
}

.flip-card-back {
  background-color: #2980b9;
  color: white;
  font-weight: bold;
  transform: rotateY(180deg);
  display: flex;
  justify-content: center;
  align-items: center;
  font-family: 'Arial', sans-serif; /* Fuente de las flip cards */
}

/* BOTONES INTERACTIVOS */
.slider-label {
  font-size: 24px; /* Cambia el tamaño de la fuente */
  font-weight: bold; /* Cambia el peso de la fuente */
  color: #333; /* Cambia el color del texto si lo deseas */
  font-family: 'Verdana', sans-serif; /* Fuente de los botones */
}

/* INTEGRANTES */
.member-link {
  display: inline-block; /* Cambiar a inline-block */
  transition: background-color 0.3s, transform 0.3s;
  font-weight: bold; /* Letra más gruesa */
  font-family: 'Tahoma', sans-serif; /* Fuente para los miembros */
}

.member-link:hover {
  background-color: #f0f0f0; /* Color de fondo al pasar el mouse */
  transform: scale(1.5); /* Aumentar ligeramente el tamaño */
}

.icon {
  display: none; /* Ocultar el ícono por defecto */
  position: absolute; /* Posicionarlo donde quieras */
  left: 100%; /* Ajusta la posición según necesites */
  top: 0; /* Ajusta la posición según necesites */
  margin-left: 10px; /* Espaciado entre el nombre y el ícono */
}

.member:hover .icon {
  display: block; /* Mostrar el ícono al pasar el mouse */
}

/* EFECTO HOVER */
.hover-effect:hover {
  transform: scale(1.15);
}

/* NAVBAR COLOR */
.navbar {
  background-color: #1C7A92 !important; /* Color del navbar */
}

/* Cambiar el estilo y color de las fuentes en el navbar */
.navbar .navbar-brand, .navbar .nav-link {
  color: white !important; /* Cambia el color del texto en el navbar */
  font-weight: bold; /* Letras más gruesas en el navbar */
  font-family: 'Helvetica', sans-serif; /* Fuente en el navbar */
}

.navbar .nav-link:hover {
  color: #f2f2f2 !important; /* Color al pasar el mouse sobre el enlace del navbar */
}
      
    "))), 
  


  
  
  
  
  navbarMenu("Teoría", icon = icon('book'),
             tabPanel("Información", 
                      icon = icon('lightbulb'),
                      fluidRow(
                        box(
                          title = tags$h3("Introducción a la minería de datos", style = "text-align: center; color: black; font-weight: bold;"),
                          width = 12,
                          status = "warning",
                          solidHeader = TRUE,
                          tags$div(
                            style = "padding: 15px; text-align: center;",
                            tags$p(
                              "La minería de datos es el uso del machine learning y el análisis estadístico para descubrir patrones y otra información valiosa a partir de grandes conjuntos de datos.",
                              style = "margin-bottom: 15px; text-align: justify; font-weight: bold;"
                            ),
                            tags$p(
                              "Estos métodos se utilizan para organizar y filtrar los datos, sacando a la luz la información más útil, desde el fraude hasta los comportamientos de los usuarios, los cuellos de botella e incluso las violaciones de seguridad. El uso de algoritmos de ML e inteligencia artificial (IA) permite la automatización del análisis, lo que puede acelerar enormemente el proceso.",
                              style = "margin-bottom: 15px; text-align: justify; font-weight: bold;"
                            )
    
                          )
                        
                      ,
                            
                            # Div contenedor para alinear las imágenes en fila
                            tags$div(
                              style = "display: flex; justify-content: space-between;",
                              # Primera imagen
                              tags$img(src = 'data.jpg', 
                                       style = "width: 48%; height: 450px; margin-bottom: 20px; display: inline-block; transition: transform 0.3s; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2);",
                                       class = "hover-effect"),
                              # Segunda imagen
                              tags$img(src = 'order.gif', 
                                       style = "width: 48%; height: 450px; margin-bottom: 20px; display: inline-block; transition: transform 0.3s; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2);",
                                       class = "hover-effect"),
                              tags$br(),
                            )
                        )
                      )) , 
             
             tabPanel("Ventajas y desventajas",icon=icon("check-circle"),
                                   tags$div(
                                     style = "padding-left: 10px; padding-right: 10px;",
                                     tags$style(HTML("
        .card {
          background-color: #f8f9fa;
          padding: 20px;
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
          margin-bottom: 20px;
          transition: transform 0.3s ease;
        }
        .card:hover {
          transform: scale(1.02);
        }
        .card-header {
          font-size: 18px;
          font-weight: bold;
          margin-bottom: 10px;
        }
        .card-content {
          margin-left: 15px;
          color: #666;
        }
        .card-icon {
          font-size: 24px;
          margin-right: 10px;
          color: #007bff;
        }
      "))
                                   ),
                                   
                                   fluidRow(
                                     # Título general centrado para toda la sección
                                     tags$h3("Comparativa de Ventajas y Desventajas de la Minería de Datos", style = "text-align: center; font-weight: bold; color: #333; margin-bottom: 20px;"),
                                     
                                     # Contenedor de ventajas y desventajas, centrado en la página
                                     div(
                                       style = "display: flex; justify-content: center; gap: 20px;",
                                       
                                       # Columna de Ventajas
                                       column(
                                         width = 6,
                                         tags$h4("Ventajas", style = "text-align: center; margin-bottom: 15px; font-weight: bold;"),
                                         
                                         # Carta 1
                                         tags$div(
                                           class = "card",
                                           style = "margin-bottom: 15px;",
                                           tags$div(class = "card-header", icon("check-circle", class = "card-icon"), "Descubra ideas y tendencias ocultas"),
                                           tags$div(class = "card-content", "Toma los datos en bruto y encuentra el orden en el caos. Esto puede dar lugar a una planificación mejor informada en todos los sectores y funciones corporativas.")
                                         ),
                                         
                                         # Carta 2
                                         tags$div(
                                           class = "card",
                                           style = "margin-bottom: 15px;",
                                           tags$div(class = "card-header", icon("check-circle", class = "card-icon"), "Ahorra presupuesto"),
                                           tags$div(class = "card-content", "Se pueden identificar los cuellos de botella en los procesos empresariales para acelerar su resolución y aumentar la eficacia.")
                                         ),
                                         
                                         # Carta 3
                                         tags$div(
                                           class = "card",
                                           style = "margin-bottom: 15px;",
                                           tags$div(class = "card-header", icon("check-circle", class = "card-icon"), "Resuelve múltiples desafíos"),
                                           tags$div(class = "card-content", "Los datos de casi cualquier fuente y cualquier aspecto de una organización se pueden analizar para descubrir patrones y mejores formas de hacer negocios.")
                                         )
                                       ),
                                       
                                       # Columna de Desventajas
                                       column(
                                         width = 6,
                                         tags$h4("Desventajas", style = "text-align: center; margin-bottom: 15px; font-weight: bold;"),
                                         
                                         # Carta 1
                                         tags$div(
                                           class = "card",
                                           style = "margin-bottom: 15px;",
                                           tags$div(class = "card-header", icon("times-circle", class = "card-icon"), "Complejidad y riesgo"),
                                           tags$div(class = "card-content", "La información útil requiere datos válidos, además de expertos con experiencia en codificación.")
                                         ),
                                         
                                         # Carta 2
                                         tags$div(
                                           class = "card",
                                           style = "margin-bottom: 15px;",
                                           tags$div(class = "card-header", icon("times-circle", class = "card-icon"), "Coste"),
                                           tags$div(class = "card-content", "Para obtener los mejores resultados, a menudo se necesita una colección amplia y profunda de conjuntos de datos.")
                                         ),
                                         
                                         # Carta 3
                                         tags$div(
                                           class = "card",
                                           style = "margin-bottom: 15px;",
                                           tags$div(class = "card-header", icon("times-circle", class = "card-icon"), "Incertidumbre"),
                                           tags$div(class = "card-content", "Los resultados pueden parecer válidos pero en realidad son aleatorios y no son de fiar. Es importante recordar que la correlación no es causal.")
                                         )
                                       )
                                     )
                                   ))
                                   
                        ,
             tabPanel("Minería de Datos, Textos y Procesos",icon=icon("cube"),
                      tags$div(
                        style = "padding: 20px; background-color: #f9f9f9; border-radius: 8px;",
                        
                        # Título principal
                        tags$h3("Exploración de Minerías: Datos, Textos y Procesos", 
                                style = "text-align: center; font-weight: bold; color: #333; margin-bottom: 20px;"),
                        
                        # Minería de Datos
                        tags$div(
                          style = "padding: 15px; background-color: #ffffff; border-radius: 8px; margin-bottom: 20px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);",
                          tags$h4(icon("database"), " Minería de Datos", style = "color: #4A90E2; font-weight: bold;"),
                          tags$p("La minería de datos es el proceso de identificar patrones y extraer información útil de grandes conjuntos de datos. Se utiliza para analizar datos estructurados y no estructurados, con aplicaciones en marketing, ventas y más. Ejemplos incluyen la predicción de comportamientos, detección de fraudes y análisis de canasta de mercado.", 
                                 style = "text-align: justify; color: #555;"),
                          tags$img(src = 'pasos.png', 
                                   style = "width: 40%; height: auto; margin-top: 15px; border-radius: 8px; 
                                   box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2); display: block; margin-left: auto; margin-right: auto;")
                        ),
                        
                        # Minería de Textos
                        tags$div(
                          style = "padding: 15px; background-color: #ffffff; border-radius: 8px; margin-bottom: 20px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);",
                          tags$h4(icon("file-alt"), " Minería de Textos", style = "color: #FF6F61; font-weight: bold;"),
                          tags$p("La minería de textos, también llamada minería de datos de texto, convierte texto no estructurado en datos estructurados para identificar patrones significativos. Se usa para analizar contenido de redes sociales, reseñas de productos, artículos y más, facilitando el descubrimiento de conocimientos a partir de fuentes no estructuradas.", 
                                 style = "text-align: justify; color: #555;"),
                          tags$img(src = 'text_mining.png', 
                                   style = "width: 40%; height: auto; margin-top: 15px; border-radius: 8px; 
                                   box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2); display: block; margin-left: auto; margin-right: auto;")
                        ),
                        
                        # Minería de Procesos
                        tags$div(
                          style = "padding: 15px; background-color: #ffffff; border-radius: 8px; box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);",
                          tags$h4(icon("cogs"), " Minería de Procesos", style = "color: #50C878; font-weight: bold;"),
                          tags$p("La minería de procesos utiliza datos de registros de eventos para identificar patrones y optimizar flujos de trabajo. Permite descubrir cuellos de botella en procesos empresariales y aplicar la ciencia de datos para mejorar la eficiencia en los sistemas.",style = "text-align: justify; color: #555;"),
                          tags$img(src = 'process.jpg', 
                                   style = "width: 40%; height: auto; margin-top: 15px; border-radius: 8px; 
                  box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.2); display: block; margin-left: auto; margin-right: auto;")
 ))),
 tabPanel(
   title = "Cómo funciona la minería de datos",icon=icon("chart-bar"),
   
   # Panel content
   fluidRow(
     box(
       title = tags$h3("Cómo funciona la minería de datos", style = "text-align: center; color: black; font-weight: bold;"),
       width = 12,
       status = "primary",
       solidHeader = TRUE,
       
       # Introducción
       tags$p("El proceso de extracción de datos implica varios pasos, desde la recopilación de datos hasta la visualización, para extraer información valiosa de grandes conjuntos de datos. Las técnicas de extracción de datos se pueden utilizar para generar descripciones y predicciones sobre un conjunto de datos objetivo.",
              style = "text-align: justify; font-size: 16px; line-height: 1.5;"),
       
       tags$p("Los científicos de datos o especialistas en inteligencia empresarial (BI) describen los datos a través de sus observaciones de patrones, asociaciones y correlaciones. También clasifican y agrupan datos mediante métodos de clasificación y regresión, e identifican valores atípicos para casos de uso, como la detección de spam.",
              style = "text-align: justify; font-size: 16px; line-height: 1.5;"),
       
       tags$p("La minería de datos suele incluir cinco pasos principales: establecimiento de objetivos, selección de datos, preparación de datos, creación de modelos de datos y minería de patrones y evaluación de resultados.",
              style = "text-align: justify; font-size: 16px; line-height: 1.5;"),
       
       tags$br(),
       
       # Pasos principales con títulos en negrita e iconos
       tags$div(
         tags$h4(tags$strong(icon("flag", class = "fa-lg"), " 1. Establecer los objetivos empresariales"), style = "color: #2e86c1;"),
         tags$p("Esta puede ser la parte más difícil del proceso de minería de datos, y muchas organizaciones dedican muy poco tiempo a este importante paso. Incluso antes de identificar, extraer o limpiar los datos, los científicos de datos y las partes interesadas del negocio pueden trabajar juntas para definir el problema empresarial preciso, lo que ayuda a informar las preguntas y parámetros de los datos de un proyecto.",
                style = "text-align: justify; font-size: 16px; line-height: 1.5;")
       ),
       
       tags$div(
         tags$h4(tags$strong(icon("database", class = "fa-lg"), " 2. Selección de datos"), style = "color: #2e86c1;"),
         tags$p("Cuando se define el alcance del problema, es más fácil para los científicos de datos identificar qué conjunto de datos ayudará a responder las preguntas pertinentes para el negocio. Ellos y el equipo de TI también pueden determinar dónde se deben almacenar y proteger los datos.",
                style = "text-align: justify; font-size: 16px; line-height: 1.5;")
       ),
       
       tags$div(
         tags$h4(tags$strong(icon("cogs", class = "fa-lg"), " 3. Preparación de datos"), style = "color: #2e86c1;"),
         tags$p("Los datos relevantes se recopilan y limpian para eliminar cualquier ruido, como duplicados, valores faltantes y valores atípicos. En función del conjunto de datos, se puede tomar un paso adicional de gestión de datos para reducir el número de dimensiones, ya que demasiadas entidades pueden ralentizar cualquier cálculo posterior.",
                style = "text-align: justify; font-size: 16px; line-height: 1.5;")
       ),
       
       tags$div(
         tags$h4(tags$strong(icon("chart-line", class = "fa-lg"), " 4. Creación de modelos y minería de patrones"), style = "color: #2e86c1;"),
         tags$p("Según el tipo de análisis, los científicos de datos pueden investigar cualquier tendencia o relación de datos interesante, como patrones secuenciales, reglas de asociación o correlaciones. Aunque los patrones de alta frecuencia tienen aplicaciones más amplias, a veces las desviaciones en los datos pueden ser más interesantes y resaltar áreas de posible fraude.",
                style = "text-align: justify; font-size: 16px; line-height: 1.5;")
       ),
       
       tags$div(
         tags$h4(tags$strong(icon("check-circle", class = "fa-lg"), " 5. Evaluación de los resultados e implementación del conocimiento"), style = "color: #2e86c1;"),
         tags$p("Cuando los datos se agregan, se pueden preparar para su presentación, a menudo mediante técnicas de visualización de datos, de modo que los resultados se puedan evaluar e interpretar. Lo ideal es que los resultados finales sean válidos, novedosos, útiles y comprensibles.",
                style = "text-align: justify; font-size: 16px; line-height: 1.5;")
       ),
       
       tags$br()
     )
   )
 )

                          
                          
                          
                        )
  
  ,
  
  
  # Pestaña Datos con subpestañas
 navbarMenu("Datos", icon = icon('database'),
            tabPanel("Dataset", icon = icon('table'),
                     fluidRow(
                       column(12,
                              box(
                                title = 'Descripción de los Datos',
                                width = 12,
                                status = 'warning',
                                solidHeader = TRUE,
                                DTOutput("columnDescription")
                              )
                       ) ),
                          
                          
                          fluidRow(
                            box(title = 'Tabla de Datos', width = 12, status = 'warning', DTOutput('data_table'), class="box_custom")
                          ),
                          tags$br(),
                          fluidRow(
                            box(title = 'Forma de los Datos', width = 6, status = 'warning', verbatimTextOutput('data_str'), class="box_custom"),
                            box(title = 'Estadísticas Descriptivas', width = 6, status = 'warning', verbatimTextOutput('data_summary'), class="box_custom")
                          )
                          
                        )
                      ),
  

  # Panel de Análisis Exploratorio de Datos
  tabPanel("EDA", 
           icon = icon('search'),
           fluidRow(
             
             # Caja para Gráfico de Barras
             column(
               width = 12,
               style = "margin-bottom: 30px;",  # Espacio entre las cajas
               box(
                 title = "Frecuencia Reviews por Departamento: ", 
                 width = 12, 
                 status = 'warning', 
                 height = "500px", 
                 solidHeader = TRUE,
                 plotlyOutput("g1")
               )
             ),
             
             
             # Caja para Gráfico de Barras
             column(
               width = 12,
               style = "margin-bottom: 30px;",  # Espacio entre las cajas
               box(
                 title = "Cantidad de Reviews por Departamentos segun Grupo de Edad :",
                 width = 12, 
                 status = 'warning', 
                 height = "500px", 
                 solidHeader = TRUE,
                 
                 selectInput("age_group",
                             label = "Selecciona Grupo de Edad:",
                             choices = c("Grupo 18-34","Grupo 35-52", "Grupo 53-100"),
                             selected = "Grupo 35-52"),
              
                 
                 # Espacio para mostrar el gráfico de barras
                 plotlyOutput("edades")
               )
             ),
             # Espacio de separación con una columna vacía
             column(
               width = 12,
               style = "height: 50px;"  # Columna vacía para separar
             )),
             fluidRow(
             
             # Caja para Gráfico  3
             column(
               width = 6,
               style = "margin-bottom: 30px;",  # Espacio entre las cajas
               box(
                 title = "Frecuencia de Reviews totales segun grupo de edad",
                 width = 12, 
                 status = 'info', 
                 height = "500px", 
                 solidHeader = TRUE,
                 plotlyOutput("g3")
               )),
             

             # Caja para Gráfico 4
             column(
               width = 6,
               style = "margin-bottom: 30px;",  # Espacio entre las cajas
               box(
                 title = "Distribución de Reviews positivas y negativas",
                 width = 12, 
                 status = 'info', 
                 height = "500px", 
                 solidHeader = TRUE,

                 
                 # Espacio para mostrar el gráfico de barras por Agrupación
                 plotlyOutput("fig")
               )
             ),
             column(
               width = 12,
               style = "margin-bottom: 30px;",  # Espacio entre las cajas
               box(
                 title = "Visualización del Top 10 de las palabras más usadas por departamento.",
                 width = 12, 
                 status = 'info', 
                 height = "500px", 
                 solidHeader = TRUE,
                 
      selectInput("tipo_grafico", 
                  label = "Selecciona el tipo de palabras:", 
                  choices = c("Positivas", "Negativas"))
    ,
    
          plotOutput("grafico_palabras"))
  
             ),
    # Nuevo apartado para Bigramas
    column(
      width = 12,
      style = "margin-bottom: 30px;",  # Espacio entre las cajas
      box(
        title = "Análisis de Bigramas",
        width = 12, 
        status = 'info', 
        height = "500px", 
        solidHeader = TRUE,
        
        tabsetPanel(
          tabPanel("Generales",
                   plotlyOutput("plot_bigrams")),
          tabPanel("Positivos",
                   plotlyOutput("plot_positive_bigrams")),
          tabPanel("Negativos",
                   plotlyOutput("plot_negative_bigrams"))
        )
      )
    )
             )
  )  ,

          
 tabPanel(
   "Simulación", 
   icon = icon('project-diagram'),
   sidebarLayout(
     sidebarPanel(
       # Selector de departamento (Class.Name)
       selectInput("class_name", 
                   "Selecciona el Departamento:",
                   choices = NULL,  # Inicialmente vacío
                   selected = NULL)
     ),
     
     mainPanel(
       # Mostrar la nube de palabras
       wordcloud2Output("wordcloud"),
       
       # Mostrar tabla de sentimiento por reseña
       DTOutput("sentiment_table"),
       # Box con la ganancia neta
       box(
         title = "Ganancia Neta segun las reviews: ", 
         width = 12, 
         status = 'info', 
         solidHeader = TRUE,
         height = "150px",
         uiOutput("ganancia_neta_general")) 
     )
   )
 )
 
 
 
 
 
 
 
)


## Define server logic
server <- function(input, output ,session) {
  
  observe({
    rmd_file <- "mineria.Rmd"
    html_file <- "www/report.html"
    
    # Verifica si el archivo RMD existe
    if (!file.exists(rmd_file)) {
      print("El archivo RMD no se encuentra.")
    }
    
    # Renderiza el archivo HTML si no existe
    if (!file.exists(html_file)) {
      tryCatch({
        rmarkdown::render(rmd_file, output_format = "html_document", output_file = html_file)
        print(paste("HTML generado exitosamente:", html_file))
      }, error = function(e) {
        print(paste("Error al renderizar el archivo RMD:", e$message))
      })
    }
  })
  output$download_rmd <- downloadHandler(
    filename = function() {
      paste("mineria", ".Rmd", sep = "")
    },
    content = function(file) {
      # Verifica si el archivo existe antes de copiar
      if (file.exists("Analisis.Rmd")) {
        file.copy("mineria.Rmd", file)
        print("Archivo RMD descargado correctamente.")
      } else {
        print("El archivo RMD no se encuentra.")
      }
    }
  )
  
  output$data_str <- renderPrint({str(datos)})
  output$data_summary <- renderPrint({summary(datos)})
  output$data_table <- renderDT({
    datatable(datos, options = list(
      scrollX = TRUE, pageLength = 5, searchHighlight = TRUE,
      autoWidth = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ), rownames = FALSE, extensions = 'Buttons')  })
  
  output$final_datatable<-renderDT({
    datatable(final_dataset, options = list(
      scrollX = TRUE, pageLength =5, searchHighlight = TRUE,
      autoWidth = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ), rownames = FALSE, extensions = 'Buttons')
  })
  
  output$columnDescription <- renderDT({
    data.frame(
      "Columna" = c("Clothing ID", "Age", "Title", "Review.Text", "Rating", "Recommended.IND"),
      "Descripción" = c("Un identificador numérico único para cada prenda.",
                        "La edad del cliente.",
                        "Título de la reseña.",
                        "Texto completo de la reseña.",
                        "Calificación numérica.",
                        "Indicador binario si recomienda el producto."),
      stringsAsFactors = FALSE
    )
  })

#__________________________EDA
  #Grafico : Cantidad Reviews por departamentos
  
  output$g1 <- renderPlotly({
    g1
  })
  #---------------------------------------------------------------------------------------------------------
  # Grafico : Cantidad Reviews por rango de edades - agrupado por departamentos
  output$edades <- renderPlotly({ # Cambiado a renderPlotly para salida interactiva
    breaks <- c(18, 34, 52, 100) 
    labels <- c("Grupo 18-34", "Grupo 35-52", "Grupo 53-100") 
    
    datos1 <- datos %>% 
      mutate(Group_Age = cut(Age, breaks = breaks, labels = labels, right = FALSE)) # Agrupa por rango de edad 
    
    data_filtrada <- datos1 %>% 
      filter(Group_Age == input$age_group) %>% 
      group_by(Department.Name) %>% 
      summarise(frecuencia = n(), .groups = "drop") 
    
    # Crear gráfico de barras con ggplotly
    g2 <- ggplot(data_filtrada, aes(x = reorder(Department.Name, -frecuencia), y = frecuencia, fill = Department.Name)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      labs(x = "Department Name", y = "Frecuencia") + 
      theme_minimal() + 
      scale_fill_manual(values = c("#A4D8E1", "#BFACE2", "#FFB142", "#FF6F61", "#DAF7A6","#F7CAC9","#B9D3C1")) +
      coord_flip()
    
    ggplotly(g2)})
  
#Grafico 3
  output$g3 <- renderPlotly({
    g3
  })
  
#Grafico 4
  output$fig <- renderPlotly({
    fig
  })
  
  # Función reactiva para filtrar los datos y generar el gráfico
  output$grafico_palabras <- renderPlot({
    
    # Filtrar los datos según la selección de tipo de palabras (Positivas o Negativas)
    tipo_palabras <- ifelse(input$tipo_grafico == "Positivas", "positive", "negative")
    
    # Filtrar y procesar las palabras
    top_palabras <- datos %>%
      unnest_tokens(word, Review.Text) %>%  # Tokenizar las palabras en la columna 'Review.Text'
      anti_join(stop_words) %>%  # Eliminar las palabras vacías
      filter(Department.Name != "Others") %>%  # Excluir el departamento "Others"
      inner_join(get_sentiments("bing") %>% filter(sentiment == tipo_palabras), by = "word") %>%  # Filtrar palabras positivas o negativas
      count(Department.Name, word, sort = TRUE) %>%  # Contar las palabras por departamento
      group_by(Department.Name) %>%
      top_n(10, n) %>%  # Seleccionar las 10 palabras más frecuentes por departamento
      ungroup()
    
    # Gráfico de palabras
    ggplot(top_palabras, aes(x = reorder(word, n), y = n, fill = Department.Name)) +
      geom_col() +
      coord_flip() +  # Cambiar a barras horizontales
      facet_wrap(~ Department.Name, scales = "free_y") +  # Crear una faceta por cada departamento
      labs(
        title = paste("Top 10 Palabras", input$tipo_grafico, "más Usadas por Departamento"),
        x = "Palabras",
        y = "Frecuencia",
        fill = "Departamento"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)
      )
  })

  # Generar bigramas y filtrar palabras vacías
  datos_bigrams <- reactive({
    datos %>%
      unnest_tokens(bigram, Review.Text, token = "ngrams", n = 2) %>%
      separate(bigram, into = c("word1", "word2"), sep = " ") %>%
      filter(
        !word1 %in% stop_words$word,
        !word2 %in% stop_words$word
      ) %>%
      unite(bigram, word1, word2, sep = " ") %>%
      count(bigram, sort = TRUE)
  })
  
  # Filtrar bigramas positivos y negativos
  bigrams_positive <- reactive({
    datos_bigrams() %>%
      filter(str_detect(bigram, paste(c("good", "great", "excellent", "amazing", "best"), collapse = "|")))
  })
  
  bigrams_negative <- reactive({
    datos_bigrams() %>%
      filter(str_detect(bigram, paste(c("bad", "poor", "terrible", "awful", "worst"), collapse = "|")))
  })
  
  # Gráfico: Top 10 bigramas generales
  output$plot_bigrams <- renderPlotly({
    bigrams_data <- datos_bigrams()
    gg <- ggplot(bigrams_data[1:10, ], aes(x = reorder(bigram, n), y = n)) +
      geom_bar(stat = "identity", fill = "#A4D8E1") +
      coord_flip() +
      labs(x = "Bigramas", y = "Frecuencia", title = "Top 10 Bigramas Filtrados") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # Gráfico: Bigramas positivos
  output$plot_positive_bigrams <- renderPlotly({
    bigrams_data <- bigrams_positive()
    if (nrow(bigrams_data) > 0) {
      gg <- ggplot(bigrams_data[1:min(10, nrow(bigrams_data)), ], aes(x = reorder(bigram, n), y = n)) +
        geom_bar(stat = "identity", fill = "#88B04B") +
        coord_flip() +
        labs(x = "Bigramas Positivos", y = "Frecuencia", title = "Top Bigramas Positivos") +
        theme_minimal()
      ggplotly(gg)
    } else {
      NULL
    }
  })
  
  # Gráfico: Bigramas negativos
  output$plot_negative_bigrams <- renderPlotly({
    bigrams_data <- bigrams_negative()
    if (nrow(bigrams_data) > 0) {
      gg <- ggplot(bigrams_data[1:min(10, nrow(bigrams_data)), ], aes(x = reorder(bigram, n), y = n)) +
        geom_bar(stat = "identity", fill = "#FF6F61") +
        coord_flip() +
        labs(x = "Bigramas Negativos", y = "Frecuencia", title = "Top Bigramas Negativos") +
        theme_minimal()
      ggplotly(gg)
    } else {
      NULL
    }
  })


  #-------------SIMULACION
  
  # Filtrar los departamentos con suficientes reseñas (p. ej., más de 10 reseñas)
  observeEvent(datos, {
    dept_count <- datos %>%
      group_by(Class.Name) %>%
      tally() %>%
      filter(n > 10)  # Cambia 10 por el número mínimo de reseñas que deseas
    
    # Actualizar las opciones del selectInput con los departamentos filtrados
    updateSelectInput(session, "class_name", 
                      choices = dept_count$Class.Name, 
                      selected = dept_count$Class.Name[1])
  })
  
  # Filtrar las reseñas según el departamento seleccionado
  filtered_reviews <- reactive({
    req(input$class_name)  # Asegura que el input esté disponible
    datos %>%
      filter(Class.Name == input$class_name) %>%
      mutate(ReviewText = Review.Text) %>%
      unnest_tokens(input = "Review.Text", output = "PalabraToken") %>%
      inner_join(get_sentiments("afinn"), by = c("PalabraToken" = "word")) %>%
      mutate(Tipo = ifelse(value > 0, "Positiva", "Negativa"))
  })
  
  # Calcular el sentimiento neto por reseña
  sentiment_scores <- reactive({
    filtered_reviews() %>%
      group_by(Class.Name, ReviewText) %>%
      summarize(sentiment_score = mean(value, na.rm = TRUE), .groups = 'drop')
  })
  
  # Mostrar la tabla de sentimiento por reseña
  output$sentiment_table <- renderDT({
    sentiment_scores()
  })
  
  # Generar la nube de palabras con animación y tamaño ajustado
  output$wordcloud <- renderWordcloud2({
    word_freq <- filtered_reviews() %>%
      count(PalabraToken, sort = TRUE) %>%
      filter(n > 20)  # Solo mostrar palabras que aparezcan más de 10 veces
    
    # Crear la nube de palabras interactiva con animación y tamaño ajustado
    wordcloud2(word_freq, size = 1.5, color='random-light', shape = 'circle', rotateRatio = 0.5)
  })
  
  # Calcular la ganancia neta general basada en todas las reseñas
  sentiment_scores_general <- reactive({
    req(filtered_reviews())  # Asegura que filtered_reviews() tenga datos
    
    # Calcular la ganancia neta por reseña (basado en el sentimiento de cada palabra)
    ganancias_por_reseña <- filtered_reviews() %>%
      mutate(ganancia_neta = case_when(
        value > 0 ~ 2,   # Reseña positiva suma 2 dólares
        value < 0 ~ -4,  # Reseña negativa resta 4 dólares
        TRUE ~ 1         # Reseña neutral suma 1 dólar
      )) %>%
      group_by(ReviewText) %>%
      summarize(ganancia_total = sum(ganancia_neta, na.rm = TRUE))  # Sumar la ganancia neta por reseña
    
    # Ahora sumamos todas las ganancias netas para obtener la ganancia neta general
    total_ganancia <- sum(ganancias_por_reseña$ganancia_total, na.rm = TRUE)
    
    total_ganancia  # Devolvemos la ganancia neta general
  })
  
  # Mostrar la ganancia neta general
  output$ganancia_neta_general <- renderUI({
    req(sentiment_scores_general())  # Asegura que sentiment_scores_general() tenga datos
    
    ganancia <- sentiment_scores_general()  # Total de la ganancia neta general
    
    # Si no hay datos, mostramos un mensaje por defecto
    if (is.null(ganancia) || length(ganancia) == 0) {
      return(
        h3("No hay datos suficientes para calcular la ganancia neta.", style = "color: gray;")
      )
    }
    
    # Cambiar el color y el icono según el valor de la ganancia neta
    color <- ifelse(ganancia > 0, "green", ifelse(ganancia < 0, "red", "gray"))
    icon <- ifelse(ganancia > 0, "check-circle", ifelse(ganancia < 0, "times-circle", "circle"))
    
    # Retornar el box con la ganancia neta
    tagList(
      fluidRow(
        column(1, icon(icon, style = paste("font-size: 40px; color:", color))),
        column(11, 
               h3(paste("Ganancia Neta General: $", round(ganancia, 2)), 
                  style = paste("font-size: 24px; color:", color)),
               p("La ganancia neta general se calcula sumando 2 dólares para reseñas positivas, restando 4 dólares para reseñas negativas y sumando 1 dólar para reseñas neutras. Este valor refleja el sentimiento total de las reseñas.", 
                 style = "color: gray; font-size: 14px;")
        )
      )
    )
  })


 
}

    
    
    
    

    


    


# Run the application
shinyApp(ui = ui, server = server)