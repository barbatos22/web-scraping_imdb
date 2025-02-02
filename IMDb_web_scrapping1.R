#===============================================================================
# LIBRARIES
#===============================================================================

# Install packages if they are not installed
if (!require("rvest")) install.packages("rvest")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")
if (!require("purrr")) install.packages("purrr")
if (!require("httr")) install.packages("httr")
if (!require("forcats")) install.packages("forcats")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("stringr")) install.packages("stringr")
if (!require("gridExtra")) install.packages("gridExtra")

# Librerías a utilizar
library(rvest)
library(jsonlite)
library(dplyr)
library(purrr)
library(httr)
library(forcats)
library(ggplot2)
library(tidyr)
library(stringr)
library(gridExtra)
library(scales)
#===============================================================================
# DATA CAPTURE
#===============================================================================

#1. Web Scrapping IMDb TOP 250

# URL IMDb Top 250
url <- "https://www.imdb.com/chart/top/"

# Read the webpage
webpage <- read_html(url)

# Extract the script content with id "_NEXT_DATA_"
script_content <- webpage %>%
  html_node("script#__NEXT_DATA__") %>%
  html_text()

# Convert the script content (JSON) into an R list
data <- fromJSON(script_content, flatten = TRUE)

# Extract the data frame of movies
movies <- data$props$pageProps$pageData$chartTitles$edges

# View the first rows to confirm it contains the expected data
head(movies)


# Select only the specified columns
movies_filtered <- movies %>% select(
  currentRank,
  `node.id`,
  `node.titleText.text`,
  `node.originalTitleText.text`,
  `node.releaseYear.year`,
  `node.ratingsSummary.aggregateRating`,
  `node.ratingsSummary.voteCount`,
  `node.runtime.seconds`,
  `node.certificate.rating`,
  `node.plot.plotText.plainText`,
  `node.titleGenres.genres`
)

# View the new data frame
head(movies_filtered)

# Rename the columns in the database
movies_filtered <- movies_filtered %>%
  rename(
    `ID IMDb` = `node.id`,
    `Posición actual` = `currentRank`,
    `Titulo en castellano` = `node.titleText.text`,
    `Titulo en ingles` = `node.originalTitleText.text`,
    `Año de lanzamiento` = `node.releaseYear.year`,
    `Calificación promedio` = `node.ratingsSummary.aggregateRating`,
    `Número de votos` = `node.ratingsSummary.voteCount`,
    `Duración en segundos` = `node.runtime.seconds`,
    `Certificado` = `node.certificate.rating`,
    `Sinopsis` = `node.plot.plotText.plainText`,
    `Generos` = `node.titleGenres.genres`
  )

# Extract the genres of each movie and store them in a new column 'generos_transformados'
movies_filtered$generos_transformados <- map(movies_filtered$Generos, function(x) {
  if (length(x) > 0) {
    genres <- x$genre.text  # Obtener todos los géneros
    return(paste(genres, collapse = ", "))  # Unir los géneros con coma
  } else {
    return(NA)  # Si no hay géneros, retornar NA
  }
})

# Verify the results
head(movies_filtered$generos_transformados)

#====================================================================================================

#2. Data capture using TMDb API

# Set up API Key and Access Token
api_key <- "f3f1914a7caa09c42029b6c5dd958d6b"
access_token <- "eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiJmM2YxOTE0YTdjYWEwOWM0MjAyOWI2YzVkZDk1OGQ2YiIsIm5iZiI6MTczNDQ1MDUxMi43NzQwMDAyLCJzdWIiOiI2NzYxOWQ1MDE1NDhkODdhNmNjYmY2NmIiLCJzY29wZXMiOlsiYXBpX3JlYWQiXSwidmVyc2lvbiI6MX0.1IQRXD7s6NGbsjAi8gnZ2xJK0mVI_l9areINWCK-YiQ"

# Create a new column for the budget
movies_filtered <- movies_filtered %>%
  mutate(Budget = NA)  # Inicializar con NA

# Function to get the budget
get_budget <- function(imdb_id) {
  # Search for TMDb ID based on IMDb ID
  search_url <- paste0("https://api.themoviedb.org/3/find/", imdb_id,
                       "?api_key=", api_key, "&external_source=imdb_id")
  
  response <- GET(search_url, add_headers(Authorization = paste("Bearer", access_token)))
  
  if (http_status(response)$category != "Success") {
    warning(paste("Error al buscar IMDb ID:", imdb_id))
    return(NA)
  }
  
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  if (length(data$movie_results) > 0) {
    tmdb_id <- data$movie_results$id[1]
  } else {
    return(NA)
  }
  
  # Obtain movie details using the TMDb ID
  details_url <- paste0("https://api.themoviedb.org/3/movie/", tmdb_id,
                        "?api_key=", api_key)
  response_details <- GET(details_url, add_headers(Authorization = paste("Bearer", access_token)))
  
  if (http_status(response_details)$category != "Success") {
    warning(paste("Error al obtener detalles para TMDb ID:", tmdb_id))
    return(NA)
  }
  
  details <- fromJSON(content(response_details, as = "text", encoding = "UTF-8"))
  return(details$budget)
}

# Iterate over each movie and retrieve the budget
for (i in 1:nrow(movies_filtered)) {
  imdb_id <- movies_filtered$`ID IMDb`[i]  # Extraer el IMDb ID actual
  budget <- get_budget(imdb_id)  # Obtener el presupuesto
  movies_filtered$Budget[i] <- budget  # Asignar el presupuesto a la columna
  cat("Procesando película:", i, "de", nrow(movies_filtered), "\n")
}

#===============================================================================
#DATA ANALYSIS
#===============================================================================

# Calculate statistics
summary_stats <- movies_filtered %>%
  summarise(
    Min_Calificacion = min(`Calificación promedio`, na.rm = TRUE),
    Media_Calificacion = mean(`Calificación promedio`, na.rm = TRUE),
    Max_Calificacion = max(`Calificación promedio`, na.rm = TRUE),
    Min_Duracion = min(`Duración en segundos`, na.rm = TRUE),
    Media_Duracion = mean(`Duración en segundos`, na.rm = TRUE),
    Max_Duracion = max(`Duración en segundos`, na.rm = TRUE),
    Min_Presupuesto = min(Budget[Budget > 0], na.rm = TRUE),  # Excluir 0 en presupuesto
    Media_Presupuesto = mean(Budget[Budget > 0], na.rm = TRUE), # Excluir 0 en presupuesto
    Max_Presupuesto = max(Budget[Budget > 0], na.rm = TRUE)     # Excluir 0 en presupuesto
  )

# Table for Ratings
calificacion_table <- data.frame(
  Estimación = c("Mínima", "Media", "Máxima"),
  Valor = c(summary_stats$Min_Calificacion, summary_stats$Media_Calificacion, summary_stats$Max_Calificacion)
)

# Table for Duration
duracion_table <- data.frame(
  Estimación = c("Mínima", "Media", "Máxima"),
  Valor = c(summary_stats$Min_Duracion, summary_stats$Media_Duracion, summary_stats$Max_Duracion)
)

# Table for Budget
presupuesto_table <- data.frame(
  Estimación = c("Mínima", "Media", "Máxima"),
  Valor = c(summary_stats$Min_Presupuesto, summary_stats$Media_Presupuesto, summary_stats$Max_Presupuesto)
)

#====================================================================================================
# GENRE

# Expand the 'generos_transformados' column into multiple rows
genres_expanded <- movies_filtered %>%
  separate_rows(generos_transformados, sep = ", ") %>%
  filter(!is.na(generos_transformados))

# Create a histogram of genres sorted by increasing frequency
p1 <- ggplot(genres_expanded, aes(x = fct_infreq(generos_transformados, ordered = TRUE))) +
  geom_bar(fill = "dodgerblue", color = "black", alpha = 0.7) +
  labs(
    title = "Frecuencia de Géneros de Películas",
    x = "Géneros",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_discrete(limits = rev(levels(fct_infreq(genres_expanded$generos_transformados))))  # Orden inverso

#====================================================================================================
# RELEASE YEAR

# Histogram of release year
p2 <- ggplot(movies_filtered, aes(x = `Año de lanzamiento`)) +
  geom_histogram(bins = 20, fill = "forestgreen", color = "black", alpha = 0.7) +
  labs(title = "Distribución del Año de Lanzamiento",
       x = "Año de Lanzamiento",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Relationship between release year and ranking position
p3 <- ggplot(movies_filtered, aes(x = `Año de lanzamiento`, y = `Posición actual`, color = `Año de lanzamiento`)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "green") +
  labs(
    title = "Relación entre Año de Estreno y Posición en el Ranking",
    x = "Año de Lanzamiento",
    y = "Posición en el Ranking",
    color = "Año de Estreno"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#====================================================================================================
# AVERAGE RATING

# Histogram of average rating
p4 <- ggplot(movies_filtered, aes(x = `Calificación promedio`)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Calificación Promedio",
       x = "Calificación Promedio",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Boxplot for average rating
p5 <- ggplot(movies_filtered, aes(x = "", y = `Calificación promedio`)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(title = "Distribución de Calificación Promedio",
       x = "",
       y = "Calificación Promedio") +
  theme_minimal()

#====================================================================================================
# DURATION

# Histogram of duration
movies_filtered <- movies_filtered %>% mutate(`Duración en minutos` = `Duración en segundos` / 60)

p6 <- ggplot(movies_filtered, aes(x = `Duración en minutos`)) +
  geom_histogram(bins = 20, fill = "darkorange", color = "black", alpha = 0.7) +
  labs(title = "Distribución de la Duración en Minutos",
       x = "Duración en Minutos",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Boxplot 
p7 <- ggplot(movies_filtered, aes(x = "", y = `Duración en minutos`)) +
  geom_boxplot(fill = "darkorange", color = "black") +
  labs(title = "Distribución de Duración de Películas (en minutos)",
       x = "",
       y = "Duración en minutos") +
  theme_minimal()

# Relationship between Duration and Ranking Position
p8 <- ggplot(movies_filtered, aes(x = `Duración en minutos`, y = `Posición actual`, color = `Duración en minutos`)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_gradient(low = "green", high = "red") +
  labs(
    title = "Relación entre Duración y Posición en el Ranking",
    x = "Duración en Minutos",
    y = "Posición en el Ranking",
    color = "Duración (min)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#====================================================================================================
# BUDGET

# Filter movies with a budget greater than 0
movies_filtered_nonzero_budget <- movies_filtered %>% filter(Budget > 0)

# Histogram of budget (excluding 0 values)
summary(movies_filtered_nonzero_budget$Budget)
p9 <- ggplot(movies_filtered_nonzero_budget, aes(x = Budget)) +
  geom_histogram(bins = 20, fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(title = "Distribución del Presupuesto",
       x = "Presupuesto (USD)",
       y = "Frecuencia") +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Boxplot 
p10 <- ggplot(movies_filtered_nonzero_budget, aes(x = "", y = Budget)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(title = "Distribución de Presupuesto de Producción",
       x = "",
       y = "Presupuesto (USD)") +
  theme_minimal()

#====================================================================================================

# Load CPI (Consumer Price Index) data into a data frame
cpi_data <- data.frame(
  Year = 1913:2024,
  CPI = c(9.9, 10.0, 10.1, 10.9, 12.8, 15.0, 17.3, 20.0, 17.9, 16.8, 17.1, 17.1, 
          17.5, 17.7, 17.4, 17.2, 17.2, 16.7, 15.2, 13.6, 12.9, 13.4, 13.7, 13.9, 
          14.4, 14.1, 13.9, 14.0, 14.7, 16.3, 17.3, 17.6, 18.0, 19.5, 22.3, 24.0, 
          23.8, 24.1, 26.0, 26.6, 26.8, 26.9, 26.8, 27.2, 28.1, 28.9, 29.2, 29.6, 
          29.9, 30.3, 30.6, 31.0, 31.5, 32.5, 33.4, 34.8, 36.7, 38.8, 40.5, 41.8, 
          44.4, 49.3, 53.8, 56.9, 60.6, 65.2, 72.6, 82.4, 90.9, 96.5, 99.6, 103.9, 
          107.6, 109.6, 113.6, 118.3, 124.0, 130.7, 136.2, 140.3, 144.5, 148.2, 
          152.4, 156.9, 160.5, 163.0, 166.6, 172.2, 177.1, 179.9, 184.0, 188.9, 
          195.3, 201.6, 207.3, 215.3, 214.5, 218.1, 224.9, 229.6, 233.0, 236.7, 
          237.0, 240.0, 245.1, 251.1, 255.7, 258.8, 271.0, 292.7, 304.7, 314.4)
)

# Get the CPI of the current year (e.g., 2024)
current_cpi <- cpi_data$CPI[cpi_data$Year == 2024]

# Add the corresponding CPI for the release year of each movie
movies_filtered$CPI_Release <- sapply(movies_filtered$`Año de lanzamiento`, function(year) {
  cpi_data$CPI[cpi_data$Year == year]
})

# Adjust the budget based on CPI
movies_filtered$Adjusted_Budget <- movies_filtered$Budget * (current_cpi / movies_filtered$CPI_Release)

# Show the result
head(movies_filtered)


# Filter movies with a budget greater than 0
movies_filtered_nonzero_budget <- movies_filtered %>% filter(Adjusted_Budget > 0)

# Histogram of adjusted budget (excluding 0 values)
summary(movies_filtered_nonzero_budget$Adjusted_Budget)
p9 <- ggplot(movies_filtered_nonzero_budget, aes(x = Adjusted_Budget)) +
  geom_histogram(bins = 20, fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(title = "Distribución del Presupuesto",
       x = "Presupuesto (USD)",
       y = "Frecuencia") +
  scale_x_continuous(labels = comma) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Boxplot for adjusted budget (excluding 0 values)
p10 <- ggplot(movies_filtered_nonzero_budget, aes(x = "", y = Adjusted_Budget)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(title = "Distribución de Presupuesto de Producción",
       x = "",
       y = "Presupuesto (USD)") +
  theme_minimal()

# Correlation analysis
cor(movies_filtered$Adjusted_Budget, movies_filtered$`Calificación promedio`)
cor.test(movies_filtered$`Duración en minutos`, movies_filtered$`Año de lanzamiento`)

