nota <-6.5
if (nota >=7){
  "Aprobado"
} else {
  "Suspendido"
}
notas <- c(5, 7, 9, 6)

estado <- ifelse(notas >= 7, "Aprobado", "Reprobado")

metodo <- "suma"

res <- switch(
  metodo,
  media = mean(1:10),
  mediana = median(1:10), 
  suma = sum(1:10), 
  stop("Método no reconocido")
)
res

nota <- 9
if (nota > 0) { 
  "Positivo"
} else if (nota == 0){
        "Cero"
}  else{
  "Negativo"
}

notas <- c(2, 3, 6, 7, 8.5, 9)
> estado <- ifelse(notas >= 7, "Aprobado", "Suspendido")

valor <- "max"

x <- c(1,5,2,10)

switch (
  valor,
  sum = sum(x),
  mean = mean(x),
  max = max(x),
  stop("Método no reconocido")
)

cuadrados <- numeric(5) # preasignación (buena práctica)
for (i in 1:5) {
  cuadrados[i] <- i^2
  print(i)
  }
cuadrados

butacas <- numeric(100)

for (i in 1:100) {
  if (i== 1 & i<=50) {
  butacas <- "Vendidas"
  print (butacas)
  } else {
    butacas <- "vacantes"
  }
  print (butacas)
}
  
# while: repite mientras condición sea TRUE

i<- 1
s <-0
while(i <= 5) {
  s <- s + i
  i <- i + 1
  print(s)
  print(i)
  }
s

 
i <- -5
repeat { 
  if (i > 5) break 
  print(i) 
  i <- i + 1
}

for (i in 1:10) {
  if (i %% 2 == 0) next
  print(i) # imprime impares
}
# break corta el bucle
for (i in 1:10) {
  if (i %% 2 != 0) next
  if (i == 6) break 
  print(i)
}


res <- 1
for (i in 1:8) {
  res <- res * i
  print(res)
}
res

n <- 0
s <- 0
while(sum(1:n) < 200) {
  n <- n + sum(1:n)
  print (n)
  
}
n



n <- 0
s <- 0
while (s < 200) { 
  n <- n + 1 
  s <- s + n
  }
n
s
 
m <- matrix(1:9, nrow = 3)

res1 <- apply(m, 1, sum)
res2 <- apply(m, 2, mean)
res2

l <- list(a = 1:3, b = 4:6)
         
lapply(l, sum)
sapply(l, sum)

nota <- 0

validar_nota <- function(nota) {
  if (is.na(nota)) warning("Nota es NA")
  if (nota < 0 || nota > 10) stop("Nota fuera de rango 0-10")
  nota
}

seguro_log <- function(x) {
  tryCatch(
    log(x),
    warning = function(w) { message("Warning: ", w$message); NA_real_ },
    error = function(e) { message("Error: ", e$message); NA_real_ }
  )
  }
validar_nota(8)

#Ejercicio 7
set.seed(30)
m <- matrix(sample(1:50, 12),nrow=4, ncol=3)
fila <- apply(m, 1, sum)

#Ejercicio 8
for (i in 1:20) {
  if (i %% 3 != 0) next
  if (i == 20) break 
  print(i)
  
#Ejercicio 9
validar_vec <- function(vec) {
    if (any(vec < 0)) stop("El numero es negatívo")
  vec
}

#Ejercicio 10
leer_archivo <- function(ruta) {
  tryCatch(
    read.csv(ruta),
    error=function(e) data.frame( )
  )
}

library(dplyr)
ventas <- tibble::tibble(
  id = 1:8,
  tienda = c("A","A","A","B","B","C","C","C"),
  fecha = as.Date(c("2026-01-05","2026-01-06","2026-01-10","2026-01-05","2026-01-11","2026-01-02","2026-01-09","2026-01-10")),
  producto = c("pan","leche","pan","pan","cafe","leche","cafe","pan"),
  unidades = c(10, 5, 7, 4, 3, 8, 2, 6),
  precio = c(1.2, 0.9, 1.2, 1.2, 3.5, 0.9, 3.5, 1.2)
  ) %>%
  mutate(importe = unidades * precio)
ventas

catalogo <- tibble::tibble(
  producto = c("pan","leche","cafe"),
  categoria = c("alimentos","lacteos","bebidas")
  )
ventas %>%
  left_join(catalogo, by = "producto") %>%
  select(producto, categoria, importe) %>%
  head()


# con %>%
ventas %>%
  filter(tienda == "C") %>%
  summarise(total = sum(importe))

ventas

# con |> (equivalente)
ventas |>
  filter(tienda == "C") |>
  summarise(total = sum(importe))



set.seed(123)
pedidos <- tibble(
  pedido_id = 1001:1040,
  cliente = sample(c("Ana","Luis","Marta","Pablo","Sofía","Diego","Lucía"),
                   40, replace = TRUE),
  region = sample(c("Norte","Sur","Este","Oeste"), 40, replace = TRUE),
  canal = sample(c("Web","Tienda","Telefono"), 40, replace = TRUE, prob =
                   c(0.55, 0.35, 0.10)),
  categoria = sample(c("Hogar","Electrónica","Alimentos"), 40, replace =
                       TRUE, prob = c(0.35, 0.30, 0.35)),
  fecha = as.Date("2026-01-01") + sample(0:30, 40, replace = TRUE),
  unidades = sample(1:8, 40, replace = TRUE),
  precio = round(runif(40, 5, 120), 2)
) %>%
  mutate(
    importe = unidades * precio,
    # Introducimos algunos NA intencionados
    precio = if_else(row_number() %in% c(5, 17), NA_real_, precio),
    canal = if_else(row_number() %in% c(9), NA_character_, canal)
  )
pedidos





set.seed(123)
pedidos <- tibble(
  pedido_id = 1001:1040,
  cliente = sample(c("Ana","Luis","Marta","Pablo","Sofía","Diego","Lucía"),
                   40, replace = TRUE),
  region = sample(c("Norte","Sur","Este","Oeste"), 40, replace = TRUE),
  canal = sample(c("Web","Tienda","Telefono"), 40, replace = TRUE, prob =
                   c(0.55, 0.35, 0.10)),
  categoria = sample(c("Hogar","Electrónica","Alimentos"), 40, replace =
                       TRUE, prob = c(0.35, 0.30, 0.35)),
  fecha = as.Date("2026-01-01") + sample(0:30, 40, replace = TRUE),
  unidades = sample(1:8, 40, replace = TRUE),
  precio = round(runif(40, 5, 120), 2)
) %>%
  mutate(
    importe = unidades * precio,
    # Introducimos algunos NA intencionados
    precio = if_else(row_number() %in% c(5, 17), NA_real_, precio),
    canal = if_else(row_number() %in% c(9), NA_character_, canal)

  )
pedidos


help("select")

#Ejercicio 1
pedidos %>% select(pedido_id, cliente, fecha, importe)

#Ejercicio 2
pedidos %>% filter(canal == "Web", region == "Norte")

#Ejercicio 3
pedidos %>% filter(is.na(precio) | is.na(canal))

#Ejercicio 4
pedidos %>% arrange(desc(importe))  %>%  slice_head(n=5)

#Ejercicio 5
pedidos %>%
mutate(ticket = case_when(
  importe >= 300 ~ "alto",
  importe >= 150 & importe <300 ~ "medio",
  importe <150 ~ "bajo"
))

#Ejercicio 6
pedidos %>%
  mutate(mes = format(fecha, '%Y-%m'))

#Ejercicio 7
pedidos %>%
  group_by(region) %>%
  summarise(total = sum(importe, na.rm = TRUE), .groups = "drop")

#Ejercicio 8
pedidos %>%
  group_by(canal) %>%
  summarise (
    num_pedidos = n(),
  ticket_medio = mean(importe, na.rm = TRUE), .groups = "drop")
             
#Ejercicio 9
pedidos %>%
  group_by(region, categoria) %>%
  summarise(total = sum(importe, na.rm = TRUE), .groups = "drop") %>% arrange(desc(total))

#Ejercicio 10
pedidos %>%
  select(region, importe)  %>%
    group_by(region)  %>%
    slice_max(importe, n=1, with_ties=FALSE)  %>%
ungroup()

#Ejercicio 11
pedidos %>%
  group_by(cliente) %>%
  summarise(gasto_total = sum(importe, na.rm = TRUE),
 num_pedidos = n()
  )

#Ejercicio 12
ped_clientes <- pedidos %>%
  group_by(cliente) %>%
  summarise(num_pedidos = n(), .groups = "drop")
  ped_clientes  %>% filter(num_pedidos >= 6)
  
  #Ejercicio 13
  pedidos %>% add_count(cliente, name = "n_pedidos")
  
  
  
  
  
  
  
  # Ejemplo: crear un CSV temporal
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(
    "id,fecha,importe",
    "1,2026-01-03,120.5",
    "2,2026-01-04,90.0",
    "3,2026-01-05,NA"
  ), tmp)
  df <- read_csv(tmp, show_col_types = FALSE)
  df
  spec(df)
  
  
  tb <- tibble(
    id = 1:3,
    nombre = c("Ana","Luis","Marta"),
    notas = list(c(7,8), c(9,10), c(6,7))
  )
  
  tb
  spec(tb)
  class(tb)
  

  ventas_meses <- tibble(
    tienda = c("A","B"),
    `2026_01` = c(1200, 900),
    `2026_02` = c(1500, 1100),
    `2026_03` = c(1300, 1050)
  )
  
  
  # De ancho a largo
  ventas_largo <- ventas_meses %>%
    pivot_longer(
      cols = starts_with("2026_"),
      names_to = "mes",
      values_to = "ventas"
    )
  ventas_meses
  ventas_largo
  
  
  # De largo a ancho
  ventas_ancho <- ventas_largo %>%
    pivot_wider(names_from = mes, values_from = ventas)
  
  ventas_meses
  ventas_largo
  ventas_ancho
  
  #8.1 Ejercicio
  
  tmp <- tempfile(fileext = ".csv")
  writeLines(c(
    "id,fecha,importe",
    "1,2026-01-03,120.5",
    "2,2026-01-04,90.0",
    "3,2026-01-05,NA"
  ), tmp)
  
  df <- read_csv(tmp,
      col_types = cols(fecha = col_date())
  )
  
#8.2 Ejercicio
  library(dplyr)
  library(tidyr)
  ventas_meses <- tibble(
    tienda = c("A","B"),
    `2026_01` = c(1200, 900),
    `2026_02` = c(1500, 1100),
    `2026_03` = c(1300, 1050)
  )
  
  
  ventas_largo <- ventas_meses %>%
    pivot_longer(
      cols = starts_with("2026_"),
      names_to = "mes",
      values_to = "ventas") %>%
      group_by (mes) %>%
      summarise(total = sum(ventas), .groups = "drop")
      
  dfpaises <- tibble(id = 1:3, ciudad_pais = c("Madrid-ES","Lima-PE","Bogota-CO"))
  
  dfpaises %>%
    separate(ciudad_pais, into = c("ciudad","pais"), sep = "-", remove = FALSE
             
dfpaises %>% separate(ciudad_pais, into = c("ciudad","pais"), sep = "-") %>%
  unite("lugar", ciudad, pais, sep = ", ")



library(stringr)

texto <- c(" R es genial ", "Curso R - Neoland", "Email: alumno@ejemplo.com")

str_trim(texto)
str_detect(texto, "Neoland")
str_replace(texto, "Curso", "Taller")
texto
str_extract(texto, "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]")


library(lubridate)

fechas <- ymd(c("2026-01-05","2026-02-10"))

month(fechas)
wday(fechas, label = TRUE)

ahora <- now()
floor_date(ahora, unit = "day")


f <- factor(c("bajo","alto","medio","alto","bajo"))

fct_count(f)

# Reordenar por frecuencia
f2 <- fct_infreq(f)
levels(f2)

# Recode / colapsar
f3 <- fct_collapse(f, extremo = c("alto"), resto = c("medio","bajo"))
levels(f3)



df <- tibble(categoria = c("A","B","C","A","B"), ventas = c(10, 5, 7, 3, 9))
df


df %>%
  mutate(categoria = fct_reorder(categoria, ventas, .fun = sum)) %>%
  group_by(categoria) %>%
  summarise(total = sum(ventas), .groups = "drop")



#8.4 Ejercicio

library(stringr)
texto <- c("Email: alumno@ejemplo.com", "sin email", "otro:a.b+1@dominio.es")
str_extract(texto, "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+.[A-Za-z]{2,}")


#8.5 Ejercicio

library(lubridate)

a <- ymd("2026-01-05")
b <- ymd("2026-01-20")
as.numeric(difftime(b, a, units = "days"))

#8.6 Ejercicio

list <- list(a = 1:3, b = 4:8, c = c(10, 20))
map_dbl(list, median)

#8.7 Ejercicio

fun_segura <-possibly(function(x) 1/x, otherwise=NA_real_)

fun_segura(-90)

#8.8 Ejercicio
fct_reorder(ventas, .fun = median)



#8.9 Ejercicio
nombres = c("Ana","Luis","Marta")
iniciales <- map_chr(str_split(nombres, " and ", n =1, simplify = TRUE))
str_split(nombres, " and ", simplify = TRUE)

iniciales <- map_chr(str_split(str_squish(nombres), "\\s+")

library(ggplot2)
library(plotly)
library(dplyr)
library(palmerpenguins)

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()




# mapping (variable a estética)
ggplot(penguins, aes(flipper_length_mm, body_mass_g, color = species)) +
  geom_point()

# setting (valor fijo)
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) +
  geom_point(color = "steelblue")



# Histograma
ggplot(penguins, aes(body_mass_g, color = species)) +
  geom_histogram(bins = 30)

# Boxplot
ggplot(penguins, aes(species, body_mass_g)) +
  geom_boxplot()

# Línea (series)
ggplot(economics, aes(date, unemploy)) +
  geom_line()

#Estadísticos: geom_smooth y summarise visual

ggplot(penguins, aes(flipper_length_mm, body_mass_g, color = species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE)


#Facets
ggplot(penguins, aes(flipper_length_mm, body_mass_g)) +
  geom_point() +
  facet_wrap(~ species)


#Escalas y etiquetas
ggplot(penguins, aes(flipper_length_mm, body_mass_g, color = species)) +
  geom_point() +
  labs(
    title = "Masa vs longitud de aleta",
    x = "Longitud de aleta (mm)",
    y = "Masa corporal (g)",
    color = "Especie"
  )


#Themes y ajustes de estilo
p <- ggplot(penguins, aes(flipper_length_mm, body_mass_g, color = species)) +
  geom_point() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Masa vs longitud de aleta",
    x = "Longitud de aleta (mm)",
    y = "Masa corporal (g)",
    color = "Especie"
  )
p

#Guardar gráficos
p <- ggplot(penguins, aes(flipper_length_mm, body_mass_g)) + geom_point()
ggsave("grafico.png", p, width = 7, height = 5, dpi = 150)
ggsave("grafico.pdf", p, width = 7, height = 5)



install.packages(c("shiny", "DBI", "RSQLite", "pool", "dplyr", "DT"))
library(shiny)
library(DBI)
library(RSQLite)
library(pool)
library(dplyr)
library(DT)


ui <- fluidPage(
  h2("Hola Shiny"),
  textInput("nombre", "Tu nombre:", value = "Ana"),
  textOutput("saludo")
)
server <- function(input, output, session) {
  output$saludo <- renderText({
    paste0("Hola ", input$nombre, "!")
  })
}
shinyApp(ui, server)



ui <- fluidPage(
  
  sliderInput("n", "N:", min = 10, max = 200, value = 50),
  actionButton("go", "Generar"),
  plotOutput("hist")
)
server <- function(input, output, session) {
  datos <- eventReactive(input$go, {
    rnorm(input$n)
  })
  output$hist <- renderPlot({
    hist(datos(), main = "Histograma", xlab = "valor")
  })
}
shinyApp(ui, server)



server <- function(input, output, session) {
  contador <- reactiveVal(0)
  observeEvent(input$go, {
    contador(contador() + 1)
  })
  output$salida <- renderText({
    paste("Clicks:", contador())
  })
}




library(shiny)
library(DT)
ui <- fluidPage(
  titlePanel("Explorador mtcars"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x", "Eje X:", choices = names(mtcars), selected = "wt"),
      selectInput("y", "Eje Y:", choices = names(mtcars), selected = "mpg")
    ),
    mainPanel(
      plotOutput("scatter"),
      DTOutput("tabla")
    )
  )
)
server <- function(input, output, session) {
  output$scatter <- renderPlot({
    plot(mtcars[[input$x]], mtcars[[input$y]],
         xlab = input$x, ylab = input$y, pch = 19)
  })
  output$tabla <- renderDT({
    datatable(mtcars, options = list(pageLength = 10))
  })
}
shinyApp(ui, server)


#7.1 y 2 Ejercicio

ui <- fluidPage(
  
  sliderInput("n", "N:", min = 10, max = 200, value = 50),
  actionButton("go", "Generar"),
  plotOutput("p")
)
server <- function(input, output, session) {
  
  datos <- eventReactive(input$go, {
    rnorm(input$n)
  })  
  output$p<- renderPlot({
    hist(datos(), main = "rnorm", xlab = "valor")
  })
}
shinyApp(ui, server)


#7.3 Ejercicio

ui <- fluidPage(
  sliderInput("mpg", "Rango mpg:", min(mtcars$mpg), max(mtcars$mpg),
              value = range(mtcars$mpg)),
  DTOutput("tabla")
)
server <- function(input, output, session) {
  output$tabla <- renderDT({
    datatable(mtcars[mtcars$mpg >= input$mpg[1] & mtcars$mpg <= input$mpg[2],
    ],
    options = list(pageLength = 10))
  })
}
shinyApp(ui, server)

exit