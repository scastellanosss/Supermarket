# Cargar las librerías necesarias
library(shiny)     # Carga la librería Shiny para crear aplicaciones web interactivas en R.
library(deSolve)   # Carga la librería deSolve para resolver ecuaciones diferenciales.

# Definir la interfaz de usuario
ui <- fluidPage(
  titlePanel("Modelo SIR con Natalidad y Mortalidad"),  # Título de la aplicación.
  
  sidebarLayout(
    sidebarPanel(
      # Entradas de usuario para ajustar los parámetros y valores iniciales
      sliderInput("beta", "Tasa de Infección (beta):", min = 0.1, max = 1, value = 0.3, step = 0.05),
      # Control deslizante para la tasa de infección (beta), con valores entre 0.1 y 1.
      
      sliderInput("gamma", "Tasa de Recuperación (gamma):", min = 0.05, max = 1, value = 0.1, step = 0.05),
      # Control deslizante para la tasa de recuperación (gamma), con valores entre 0.05 y 1.
      
      sliderInput("mu", "Tasa de Natalidad/Mortalidad (mu):", min = 0.01, max = 0.2, value = 0.02, step = 0.01),
      # Control deslizante para la tasa de natalidad/mortalidad (mu), con valores entre 0.01 y 0.1.
      
      sliderInput("initialS", "Susceptibles Iniciales:", min = 100, max = 10000000, value = 2368467),
      # Control deslizante para la población inicial de susceptibles, con valores entre 100 y 10 millones.
      
      sliderInput("initialI", "Infectados Iniciales:", min = 1, max = 200, value = 0),
      # Control deslizante para la población inicial de infectados, con valores entre 1 y 200.
      
      sliderInput("initialR", "Recuperados Iniciales:", min = 0, max = 10000000, value = 0),
      # Control deslizante para la población inicial de recuperados, con valores entre 0 y 10 millones.
      
      sliderInput("days", "Días de Simulación:", min = 10, max = 720, value = 200),
      # Control deslizante para el número de días a simular, entre 10 y 720 días.
      
      # Mostrar el valor de R0
      h4("Número Reproductivo Básico (R0):"),   # Título para el valor de R0.
      textOutput("r0_value")                    # Muestra el valor calculado de R0.
    ),
    
    mainPanel(
      plotOutput("sirPlot")  # Salida gráfica para mostrar los resultados del modelo SIR.
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  
  # Calcular y mostrar el valor de R0
  output$r0_value <- renderText({
    R0 <- input$beta / input$gamma  # Calcular R0 como el cociente de beta y gamma.
    paste("R0 =", round(R0, 2))     # Mostrar el valor de R0 redondeado a dos decimales.
  })
  
  # Simulación del modelo SIR
  output$sirPlot <- renderPlot({
    
    # Definir los parámetros del modelo de acuerdo con la entrada del usuario
    params <- c(
      mu = input$mu,
      beta = input$beta,
      gamma = input$gamma
    )
    # El vector params contiene los valores de los parámetros mu, beta y gamma.
    
    # Definir las condiciones iniciales basadas en la entrada del usuario
    initial_state <- c(
      S = input$initialS,
      I = input$initialI,
      R = input$initialR
    )
    # El vector initial_state contiene el estado inicial de la población en cada grupo (S, I y R).
    
    # Definir el tiempo de simulación (en días)
    time <- seq(0, input$days, by = 1)
    # El vector time define el intervalo de tiempo para la simulación, en pasos diarios desde 0 hasta el número de días seleccionado.
    
    # Definir la función para el modelo SIR
    sir_model <- function(time, state, parameters) {
      with(as.list(c(state, parameters)), {
        N <- S + I + R  # Tamaño de la población total en cada momento.
        
        dS <- mu * N - (beta * I * S / N) - mu * S
        # Cambio en los susceptibles: nacimientos (mu * N), infecciones ((beta * I * S) / N), y muertes (mu * S).
        
        dI <- (beta * I * S / N) - gamma * I - mu * I
        # Cambio en los infectados: nuevas infecciones ((beta * I * S) / N), recuperaciones (gamma * I), y muertes (mu * I).
        
        dR <- gamma * I - mu * R
        # Cambio en los recuperados: recuperaciones (gamma * I) y muertes (mu * R).
        
        list(c(dS, dI, dR))  # Retorna las tasas de cambio de S, I, y R.
      })
    }
    
    # Resolver el sistema de ecuaciones diferenciales
    output <- ode(y = initial_state, times = time, func = sir_model, parms = params)
    # Llama a la función ode de la librería deSolve para resolver el modelo SIR utilizando los parámetros y valores iniciales.
    
    # Convertir la salida a un data frame para facilitar la graficación
    output_df <- as.data.frame(output)
    # Convierte la salida en un data frame para que se pueda graficar fácilmente.
    
    # Graficar los resultados
    plot(output_df$time, output_df$S, type = "l", col = "blue", ylim = c(0, max(input$initialS + input$initialI + input$initialR)), 
         ylab = "Población", xlab = "Tiempo (días)", main = "Modelo SIR con Natalidad y Mortalidad")
    # Gráfica la población de susceptibles (S) en el tiempo, en color azul.
    
    lines(output_df$time, output_df$I, col = "red")
    # Añade una línea para la población de infectados (I), en color rojo.
    
    lines(output_df$time, output_df$R, col = "green")
    # Añade una línea para la población de recuperados (R), en color verde.
    
    # Añadir leyenda para identificar las curvas
    legend("right", legend = c("Susceptibles", "Infectados", "Recuperados"), 
           col = c("blue", "red", "green"), lty = 1)
    # Añade una leyenda para identificar cada línea con su grupo correspondiente.
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
# Ejecuta la aplicación Shiny con la interfaz de usuario definida (ui) y el servidor (server).