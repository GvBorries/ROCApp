
library(shinydashboard)
library(shiny)
# library(plotly)

sliderInput2 <- function(inputId, label, min, max, value, step=NULL, from_min, from_max){
  x <- sliderInput(inputId, label, min, max, value, step)
  x$children[[2]]$attribs <- c(x$children[[2]]$attribs, 
                               "data-from-min" = from_min, 
                               "data-from-max" = from_max, 
                               "data-from-shadow" = TRUE)
  x
}


ui <- dashboardPage(
  skin="black", 
  dashboardHeader(title = div(img(src = "unb3.png", height = 20, width = 40), 
                              "ROC App", style = "font-weight: bold"),titleWidth = 300),
  dashboardSidebar(width = 300,
    sidebarMenu(
      
      # css to control slider colors
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #2ca02c}")),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #1f77b4}")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ff7f0e}")),
      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #9467bd}")),
      
      # css to control sliders' font size
      tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }"),
      tags$style(type='text/css', ".control-label { font-size: 15pt; }"),
      
      # css to control the color of the limits on sliders 1 and 2
      tags$style("js-irs-0 .irs-single, .js-irs-0 .irs-shadow {background: #2ca02c; width: 150%}"),
      tags$style("js-irs-1 .irs-single, .js-irs-1 .irs-shadow {background: #1f77b4; width: 150%}"),
      
      sliderInput2("m1", "Mean of Successes (1)",
                   min = 25, max = 75, value = 55, step = .1, from_min = 50, from_max = 75),
      
      sliderInput2("m2", "Mean of Failures (0)",
                   min = 25, max = 75, value = 45, step = .1, from_min = 25, from_max = 50),
      
      sliderInput("cut", "Discriminating Point",
                  min = 0, max = 100, value = 50, step = .01),
      
      sliderInput("cutoff", "Threshold",
                   min = 0, max = 1, value = 0.5, step = 0.0001)
    )
    
  ),
  dashboardBody(
    tags$style(".nav-tabs-custom .nav-tabs li.active {border-top-color: green;}"),
    
    fluidPage(
      tabBox(width = "100%", 
             tabPanel(h4(strong("App")),
                      style = "margin-left:-25px;",
                      style = "margin-right:-25px;",
                      tags$head(tags$style(HTML('
                                    .box-header h3 {
                                      font-weight: bold;
                                    }'))),
                      box(width = 6, 
                          title = "Distributions Panel", color = "orange", 
                          ribbon = TRUE, 
                          collapsible = TRUE,
                          column(width = 12,
                                 plotOutput('distPlot')
                          )
                      ),
                      box(width = 6, 
                          title = "ROC Curve Panel", color = "orange", 
                          ribbon = TRUE, 
                          collapsible = TRUE,
                          column(width = 12,
                                 plotOutput('procPlot')
                          )
                      ),
                      box(width = 12, 
                          # title = "Confusion Matrix Panel - 200 Elements in Each Group", color = "orange", 
                          title = "Confusion Matrix Panel", color = "orange", 
                          ribbon = TRUE, 
                          collapsible = TRUE,
                          column(width = 4),
                          column(width = 4,
                                 plotOutput('confPlot')
                          ),
                          column(width = 4)
                      )
             ),
             tabPanel(h4(strong("About"), style="color:black"),
                      h4(strong("Understanding ROC Curves")),
                      # br(), 
                      h4(strong("Program by George von Borries and Allan V. C. Quadros")),
                      # br(),
                      h4(strong("Universidade de Brasilia, Brazil, 2021.")),
                      br(),
                      h4(strong("Details about ROC APP:")),   # p(...) if not bold font
                      h4(strong("von Borries; G. and Quadros; A.V.C.")),
                      h4(strong("ROC App: An Application to Understand ROC Curves")), 
                      h4(strong("Revista Brasileira de Biometria, Lavras, v.xx, n.x, pxx-xx, 202x.")),
                      br(),br(),
                      img(src = "unb3.png")
              )
       )
     )
   ))

server <- function(input, output) {
  library(ggplot2)
  #library(plotly)
  plotly_palette <- c('#1F77B4', '#FF7F0E', '#2CA02C', '#D62728', '#9467bd', '#8c564b')
  #1f77b4 or rgb(31, 119, 180)  // muted blue
  #ff7f0e or rgb(255, 127, 14)  // safety orange
  #2ca02c or rgb(44, 160, 44)   // cooked asparagus green
  #d62728 or rgb(214, 39, 40)   // brick red
  #9467bd or rgb(148, 103, 189) // muted purple
  #8c564b or rgb(140, 86, 75)   // chestnut brown
  
  output$distPlot <- renderPlot({
    
    p1 <- ggplot(data = data.frame(x = c(0, 100)), aes(x)) +
      stat_function(fun = dnorm, n = 301, args = list(mean = input$m1, sd = 7),
                    size = 1.7, aes(colour = "Successes (1)"))+
      stat_function(fun = dnorm, n = 301, args = list(mean = input$m2, sd = 7),
                    size = 1.7, aes(colour = "Failures (0)"))+
      geom_vline(xintercept = input$cut, colour = plotly_palette[2], 
                 linetype = "dotted", alpha = 0.5, size = 1.5)+
      scale_color_manual(name = "", values = c(plotly_palette[1], plotly_palette[3])) + # ggplot2 ordena por ordem alfabetica
      labs(x="Values", y="Probability Density")+
      ggtitle("Distributions")+
      theme_bw()+
      theme(panel.border = element_blank())+ # para ficar igual o plotly
      guides(color=guide_legend(title=NULL, reverse = TRUE))+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12),
            legend.text = element_text(face = "bold", size = 14))+
      theme(legend.position = c(0.875,0.87))
    
    p1
    
    # ggplotly(p1) %>%
    #   config(p = ., staticPlot = TRUE, displayModeBar = TRUE) 
    
  })
  
  output$procPlot <- renderPlot({
    
    x <- seq(0, 100, by=0.1) # uma sobra nas pontas, para o grafico nao ficar "comido" nos extremos

    # curva 1 = acerto
    # curva 2 = erro
    
    pc1 = pnorm(q = x, mean = input$m1, sd = 7) # ainda nao eh sensibilidade
    
    pc2 = pnorm(q = x, mean = input$m2, sd = 7) # ainda nao eh FPR
    
    # ponto do cutoff na ROC
    pcut1 = pnorm(q = input$cut, mean = input$m1, sd = 7)
    pcut2 = pnorm(q = input$cut, mean = input$m2, sd = 7)
    
    c1 = 1-input$cutoff
    c2 = pnorm(qnorm(c1)*7+input$m1, mean = input$m2, sd = 7) 
    
    p2 <- ggplot()+
      geom_line(aes(x=1-pc2, y=1-pc1), colour = plotly_palette[6], size=1.7)+ # ROC curve
      geom_line(aes(x=c(0,1), y=c(0,1)), colour = "black", size=0.5,
                alpha = 0.5)+ # passar sem criar data frame, senao ele reclama
                              #... de nao terem o mesmo tamanho
      geom_hline(yintercept = c(1-pcut1, 1-c1), colour = c(plotly_palette[2], plotly_palette[5]), 
                 linetype = c("dotted","twodash"), alpha = 0.5, size = 1.5)+
      geom_vline(xintercept = c(1-pcut2, 1-c2), colour = c(plotly_palette[2], plotly_palette[5]), 
                 linetype = c("dotted","twodash"), alpha = 0.5, size = 1.5)+
      labs(x="1 - Specificity", y="Sensitivity")+
      ggtitle("ROC Curve")+
      theme_bw()+
      theme(panel.border = element_blank())+ # para ficar igual o plotly
      guides(color=guide_legend(title=NULL))+
      theme(plot.title = element_text(hjust=0.5, face = "bold", size = 20),
            axis.title = element_text(face = "bold", size = 14),
            axis.text = element_text(size = 12))
    
    p2
    # ggplotly(p2) %>%
    #   config(p = ., staticPlot = TRUE, displayModeBar = TRUE)
    
    })
  
  output$confPlot <- renderPlot({
    
    set.seed(1966)
    
    x <- seq(0, 100, by=0.1)
    
    # input <- list()
    # input$m1 <- 65
    # 
    # input$m2 <- 35
    # 
    # input$cut <- 65
    
    class_suc <- rnorm(200, mean = input$m1, sd = 7)
    class_fail <- rnorm(200, mean = input$m2, sd = 7)
    
    discr_point <- input$cut
    
    # right_class_suc <- length(class_suc[class_suc > discr_point])
    # wrong_class_suc <- length(class_suc[class_suc <= discr_point])
    # 
    # right_class_fail <- length(class_fail[class_fail < discr_point])
    # wrong_class_fail <- length(class_fail[class_fail >= discr_point])
    
    true_pos <- length(class_suc[class_suc > discr_point])
    false_pos <- length(class_suc[class_suc <= discr_point])
    
    true_neg <- length(class_fail[class_fail < discr_point])
    false_neg <- length(class_fail[class_fail >= discr_point])
    
    # p1 <- true_pos  / 100
    # p2 <- false_pos  / 100
    # p3 <- true_neg  / 100
    # p4 <- false_neg / 100
    # 
    # n1 <- round(p1 * 100,0) # True positive
    # n2 <- round(p2 * 100,0)
    # n3 <- round(p3 * 100,0)
    # n4 <- round(p4 * 100,0)
    
    n1 <- true_pos
    n2 <- false_pos
    n3 <- true_neg
    n4 <- false_neg
    
    samps4 <- t(replicate(n4, runif(2,0.1,0.9))) # draw samples
    samps2 <- t(replicate(n2, runif(2,1.1,1.9)))
    samps1 <- cbind(runif(n1,0.1,0.9),runif(n1,1.1,1.9))
    samps3 <- cbind(runif(n3,1.1,1.9),runif(n3,0.1,0.9))
    
    if(length(samps1) == 0) samps1 <- t(replicate(1,c(-2,-2)))  #
    if(length(samps2) == 0) samps2 <- t(replicate(1,c(-2,-2)))
    if(length(samps3) == 0) samps3 <- t(replicate(1,c(-2,-2)))  #
    if(length(samps4) == 0) samps4 <- t(replicate(1,c(-2,-2))) # will be a point out of range when length is zero
    
    samps <- rbind(samps1,samps2,samps3,samps4)
    # samps <- subset(samps, samps[,1] != -1)
    
    # sampsl <- c(rep("r",n1),rep("w",n2),rep("r",n3),rep("w",n4))
    
    # to avoid problems of class inversion
    
    n1_flag <- n2_flag <- n3_flag <- n4_flag <- FALSE # the flags will serve fo subtracting 1 when presenting the number on the graph
    # ... this is necessay due to the mock 1 that we add in case the length of some n* is zero
    if(n1 == 0) {n1 <- 1; n1_flag <- TRUE}
    if(n2 == 0) {n2 <- 1; n2_flag <- TRUE}
    if(n3 == 0) {n3 <- 1; n3_flag <- TRUE}
    if(n4 == 0) {n4 <- 1; n4_flag <- TRUE}
    
    sampsl <- c(rep("tp", n1),rep("fp", n2),rep("tn", n3),rep("fn", n4))
    # sampsl
    
    samps <- data.frame(samps,sampsl,stringsAsFactors = FALSE)
    # head(samps)
    
    colnames(samps) <- c("xa","ya","class")
    # head(samps)
    
    library(ggplot2)
    p3 <- ggplot(samps,aes(x=xa,y=ya,color=class)) +
      geom_point(size=2.2, alpha =0.7) +
      coord_cartesian(xlim = c(0,2), ylim=c(0,2)) +
      scale_color_manual(values=c(plotly_palette[4], plotly_palette[4], 
                                  plotly_palette[1], plotly_palette[3])) +
      geom_hline(yintercept = 1, size=1.3, colour = "#3a3636", 
                 linetype = c("dotted"), alpha = 0.7) +
      geom_vline(xintercept = 1, size=1.3, colour = "#3a3636",
                 linetype = c("dotted"), alpha = 0.7) +
      # annotate("text",0.5,2,label=paste0("True Positive (n = ", if(n1_flag) n1-1 else n1, ")"), col=plotly_palette[3], size=5) +
      # annotate("text",1.5,2,label=paste0("False Negative (n = ", if(n2_flag) n2-1 else n2, ")"), col=plotly_palette[4], size=5) +
      # annotate("text",0.5,0,label=paste0("False Positive (n = ", if(n4_flag) n4-1 else n4, ")"), col=plotly_palette[4], size=5) +
      # annotate("text",1.5,0,label=paste0("True Negative (n = ", if(n3_flag) n3-1 else n3, ")"), col=plotly_palette[1], size=5) +
      annotate("text",0.5,2,label=paste0("True Positive (n = ", if(n1_flag) n1-1 else n1, ")"), col='#3a3636', size=6.2) +
      annotate("text",1.55,2,label=paste0("False Negative (n = ", if(n2_flag) n2-1 else n2, ")"), col='#3a3636', size=6.2) +
      annotate("text",0.5,0,label=paste0("False Positive (n = ", if(n4_flag) n4-1 else n4, ")"), col='#3a3636', size=6.2) +
      annotate("text",1.55,0,label=paste0("True Negative (n = ", if(n3_flag) n3-1 else n3, ")"), col='#3a3636', size=6.2) +
      # annotate("text",0.5,2,label="True Positive", col=plotly_palette[1], size=5) +
      # annotate("text",1.5,2,label="False Negative", col=plotly_palette[4], size=5) +
      # annotate("text",0.5,0,label="False Positive", col=plotly_palette[4], size=5) +
      # annotate("text",1.5,0,label="True Negative", col=plotly_palette[2], size=5) +
      labs(x = "+                                  -\nDiagnostic",
           y = "Absent                Present") +
      theme(axis.title = element_text(face = 'bold', size = 17),
            strip.text = element_text(face = 'bold', size = 17),
            axis.text =  element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none",
            panel.border = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      # theme(panel.border = element_blank(), legend.position = "none")+
      # theme_bw()# para ficar igual o plotly
    
    p3
    # ggplotly(p3) %>%
    #   config(p = ., staticPlot = TRUE, displayModeBar = TRUE) 

  })
  
}

shinyApp(ui, server)
