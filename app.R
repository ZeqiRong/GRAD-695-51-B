library(shiny)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
        text-align: center;
      }

      .controls {
        position: fixed;
        bottom: 20px;
        width: 100%;
      }

      /* Move timer/result to the top-right */
      #timer_text {
        font-size: 60px;
        font-weight: bold;
        color: #222;
        position: absolute;
        top: 80px;
        right: 40px;
      }

      #click_btn {
        background-color: #ff4d4d;
        color: white;
        font-size: 22px;
        padding: 15px 35px;
        border-radius: 50px;
        border: none;
      }

      #click_btn:active {
        transform: scale(0.9);
      }
    "))
  ),
  
  titlePanel("🎈 Balloon Pump Game"),
  
  h3(textOutput("instruction")),
  
  plotOutput("balloon_plot", height = "500px"),
  
  textOutput("timer_text"),
  
  div(class = "controls",
      actionButton("click_btn", "🪛 Pump Balloon!"),
      actionButton("p1_start", "Player 1 Start"),
      actionButton("p2_start", "Player 2 Start"),
      uiOutput("restart_ui")
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    state = "ready",
    score1 = 0,
    score2 = 0,
    current = 0,
    time = 10,
    anim = 0
  )
  
  # Reactive timer that ticks every 100 ms
  tick <- reactiveTimer(100)
  
  observe({
    tick()  # force reactivity
    
    isolate({
      if (rv$state %in% c("p1","p2")) {
        
        # timer decreases every 1 second
        if (rv$time > 0 && (as.integer(Sys.time()) %% 1 == 0)) {
          rv$time <- rv$time - 0.1
        }
        
        # Animation decay
        rv$anim <- rv$anim * 0.85
        
        # Timer reached zero
        if (rv$time <= 0) {
          if (rv$state == "p1") {
            rv$score1 <- rv$current
            rv$state <- "p1_done"
          } else {
            rv$score2 <- rv$current
            rv$state <- "done"
          }
        }
      }
    })
  })
  
  observeEvent(input$p1_start, {
    if (rv$state == "ready") {
      rv$state <- "p1"
      rv$current <- 0
      rv$time <- 10
    }
  })
  
  observeEvent(input$p2_start, {
    if (rv$state == "p1_done") {
      rv$state <- "p2"
      rv$current <- 0
      rv$time <- 10
    }
  })
  
  observeEvent(input$click_btn, {
    if (rv$state %in% c("p1","p2")) {
      rv$current <- rv$current + 1
      rv$anim <- 1
    }
  })
  
  output$balloon_plot <- renderPlot({
    
    size <- 1 + rv$current * 0.03
    bounce <- rv$anim * 0.3
    
    plot(0, 0, type = "n",
         xlim = c(-6,6), ylim = c(-8,10),
         xlab = "", ylab = "", axes = FALSE)
    
    theta <- seq(0, 2*pi, length.out = 200)
    x <- size * 1.3 * cos(theta)
    y <- size * 1.7 * sin(theta) + 3 + bounce
    
    polygon(x, y, col="red", border="darkred", lwd=3)
    
    hx <- 0.4 * size * cos(theta) - 0.8
    hy <- 0.7 * size * sin(theta) + 4 + bounce
    polygon(hx, hy, col=rgb(1,1,1,0.3), border=NA)
    
    knot_y <- 3 - size*1.7
    polygon(c(-0.4,0.4,0), c(knot_y,knot_y,knot_y-0.6), col="darkred")
    segments(0, knot_y-0.6, 0, -3, lwd=2)
  })
  
  output$instruction <- renderText({
    switch(rv$state,
           "ready" = "👉 Player 1: Click 'Player 1 Start'",
           "p1" = "🔥 Player 1: Pump as fast as you can!",
           "p1_done" = "👉 Player 2: Click 'Player 2 Start'",
           "p2" = "🔥 Player 2: Pump as fast as you can!",
           "done" = "🏁 Game Over!"
    )
  })
  
  # --- UPDATED: TIMER OR RESULT IN TOP-RIGHT ---
  output$timer_text <- renderText({
    
    # During gameplay: show timer (whole seconds)
    if (rv$state %in% c("p1","p2")) {
      return(paste("⏱️", round(rv$time)))
    }
    
    # After game: show result in SAME spot
    if (rv$state == "done") {
      if (rv$score1 > rv$score2)
        return(paste("🎉 Player 1 Wins!", rv$score1, "vs", rv$score2))
      if (rv$score2 > rv$score1)
        return(paste("🎉 Player 2 Wins!", rv$score2, "vs", rv$score1))
      return(paste("🤝 Tie!", rv$score1, "vs", rv$score2))
    }
    
    return("")
  })
  
  
  output$restart_ui <- renderUI({
    if (rv$state == "done") {
      actionButton("restart", "Restart Game")
    }
  })
  
  observeEvent(input$restart, {
    rv$state <- "ready"
    rv$score1 <- 0
    rv$score2 <- 0
    rv$current <- 0
    rv$time <- 10
    rv$anim <- 0
  })
}

shinyApp(ui, server)