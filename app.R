

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)


I <- 4
J <- 4
B <- rep(1:J, each = I*100) 
A <- rep( rep(1:I, each = 100), J)
repet <- rep(c(1:100), I*J)
tab <- data.frame(B = B,  A = A, repet = repet)
tab$B <- as.factor(tab$B)
tab$A <- as.factor(tab$A)
X <- model.matrix(~ A + B , data = tab)

set.seed(42)
# varepsilon <- rnorm(I*J*100)
varepsilon <- sample(c(-0.8, 0.8), I*J*100, replace = TRUE)


mu <- 3              # moyenne generale
b <- c(0, -2, 3, 4)  # coefficients associes aux modalites du B





# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Header ------------------------------------------------------------------
  dashboardHeader(
    title = "Power of a test"
  ),
  
  
  # Sidebar -----------------------------------------------------------------
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Presentation", tabName = "presentation", icon = icon("Presentation")),
      menuItem("Results", tabName = "results", icon = icon("Results"))
    ),
    
    
    sliderInput("r", "Nbr of rep within each level of A",
                min = 2, max = 100, value = 5
    ),
    
    sliderInput("var", "Residual variance",
                min = 0.3, max = 16, value = 4
    ),
    # numericInput("var", label = h3("variance résiduelle"), value = 1),
    
    
    radioButtons("factor_effect", label = "effect of factor A",
                 choices = list("no effect" = 0, "(0, -0.1, 0.1, 0.2)" = 1, "(0, -0.3, 0.3, 0.6)" = 2, "(0, -1, 1, 2)" = 3, "(0, -2, 2, 4)" = 4), selected = 1),
    
    sliderInput("alpha", "alpha",
                min = 0.01, max = 0.20, value = 0.05
    ),
    
    checkboxInput("area_alpha", label = "plot alpha", value = TRUE),
    checkboxInput("area_beta", label = "plot beta", value = TRUE),
    checkboxInput("area_power", label = "plot power", value = TRUE) ,
    
    
    br(),
    a(h4("Benjamin Heuclin") ,href = "https://github.com/Heuclin"),
    a("SupAgro Montpellier" ,href = "https://www.montpellier-supagro.fr"),
    p("2020")
  ),
  
  
  
  # Body --------------------------------------------------------------------
  dashboardBody(
    
    
    tabItems(
      # # First tab content
      # Third tab content
      tabItem(tabName = "presentation",
              withMathJax(),
              tags$script("MathJax.Hub.Config({
                          tex2jax: {
                          inlineMath: [['$','$']],
                          processEscapes: true
                          }
                          });" ),
              
              h2("Presentation"),
              p("This application aims at disentagle the risk and power of statistical test"),
              p("To illustate it, we simulate a linear model with two factors A (4 levels) and B (4 levels) (anova model without interaction):"),
              withMathJax("$$Y_{ijk} = \\mu + \\alpha_i + \\gamma_j  + \\varepsilon_{ijk}$$"),
              
              p('where $ Y_{ijk}$ is the value of the obersation $ijk$, '),
              p('$\\mu$ is the intercept'),
              p('$\\alpha_i$ is the effect of the level $i$ of factor A'),
              p('$\\gamma_j$ is the effect of the level $j$ of factor B'),
              p("$\\varepsilon_{ijk}$ is the residual of individual $ijk$. Residuals are assumed independent and identicay distributed $\\varepsilon_{ijk} \\sim N(0, \\sigma^2)$  "),
              br(),
              br(),
              p('We fixed $\\mu$ to 3 and effects of factor B to c(0, -2, 3, 4).'),
              br(),
              p('We supposed that the factor B is significant and we would like to test if factor A is also significant through an Analysis of variance (ANOVA).'),
              p('In this situation, the Type II error rate, also called the false negative rate ($\\beta$) and so the power ($1 - \\beta$) are function of sample size (the rate is larger for smaller samples), the  effect size of variable which is subject to selection (a smaller effect size is more prone to Type II error) , the residual variance, and the Type I error rate ($\\alpha$).'),
              p('User can play with these different parameters in the sidebar'),
              br(),
              h4("Fisher test:"),
              p(' H0 : A does not involved on Y ($\\alpha_1 = \\alpha_2 = \\alpha_3 = \\alpha_4$ and the model can be reformulated $M_0: ~ Y_{ijk} = \\mu + \\gamma_j  + \\varepsilon_{ijk}$'),
              p('vs'),
              p("H1 : A involve on Y ($\\exists i \\neq j \\text{ such as } \\alpha_i \\neq \\alpha_{j}$ and the model can be reformulated $M_1: ~ Y_{ijk} = \\mu + \\alpha_i + \\gamma_j  + \\varepsilon_{ijk}$"),
              br(),
              p('Statistic: $F_{obs} = \\frac{}{}'),
              
              
              br(),
              p('Results panel provides the ANOVA table in R, an interactive graph of densities of $F_{obs}$ under H0 and H1 and provides the Type II error rate, the power, the values of $F_{obs}$ and the threshold $q_{1 - \\alpha}(\\mathcal{X}^2_{(p-p_0, n-p)})$')
              
              
      ),
      
      
      # second tab content
      tabItem(tabName = "results",
              
              fluidRow(
                
                column(width = 8,
                       box( title = "ANOVA table", status = "info", solidHeader = TRUE, width = NULL,
                            p(""),
                            verbatimTextOutput("anova")
                       )
                       
                ),
                column(width = 4,
                       box( title = "Parameters", status = "info", solidHeader = TRUE, width = NULL,
                            column(width = 6,
                                   p("beta = ", verbatimTextOutput("beta")),
                                   p("Statistic F_obs = ", verbatimTextOutput("F_obs"))
                            ),
                            column(width = 6,
                                   p("Power = ", verbatimTextOutput("power")),
                                   p("Threshold = ", verbatimTextOutput("seuil"))
                            )
                       )
                ),
                column(width = 12,
                       box( title = "Fisher densities under H0 and H1", status = "info", solidHeader = TRUE, width = NULL,
                            plotlyOutput("plot")
                       )
                )
                
              )
      )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  a <- reactive({
    if(input$factor_effect == 0) a=c(0,0,0,0)
    if(input$factor_effect == 1) a=c(0, -0.1, 0.1, 0.2)
    if(input$factor_effect == 2) a=c(0, -0.1, 0.1, 0.2)*3
    if(input$factor_effect == 3) a=c(0, -1, 1, 2)
    if(input$factor_effect == 4) a=c(0, -2, 2, 4)
    a
  })
  
  y_tild <- reactive({ X %*% c(mu, a()[-1], b[-1])  })
  
  data <- reactive({
    data.frame(B = tab$B,  A = tab$A, repet = tab$repet, y = y_tild() + sqrt(input$var)*varepsilon)[which(tab$repet < input$r), ]
  })
  
  mod0  <- reactive ({ lm(y ~ B, data=data())  })
  
  p0 <- reactive ({  ncol(model.matrix(mod0())) })
  
  mod1 <- reactive ({ lm(y ~ A + B, data=data())  })
  p <- reactive ({ ncol(model.matrix(mod1()))  })
  n <- reactive ({ nrow(data())  })
  
  S <- reactive ({ qf(1-input$alpha, p()-p0(), n()-p(), 0)  })
  
  F <- reactive ({ anova(mod0(), mod1())$F[2]  })
  
  delta <- reactive ({ F()*(p()-p0())  })
  beta <- reactive ({ pf(S(), p() - p0(), n() -p(), delta())  })
  puiss <- reactive ({ 1-beta()  })
  
  x <- reactive({ seq(0, max(F()*1.5, 10), 0.01) })
  dat <- reactive ({
    data.frame(x = x(),
               density_H0 = df(x(), p() -p0(), n() -p(), 0),
               density_H1 = df(x(), p() -p0(), n() -p(), delta()),
               y=df(x(), p() -p0() , n() -p(), 0))
  })
  
  # tab_error <- reactive({
  #   data.frame("H0 vrai" = c(1-input$alpha, input$alpha), "HO fausse" = c(beta(), 1-beta()))
  #   # colnames(tmp) <- c("H0 vrai", "HO fausse")
  #   # rownames(tmp) <- c("H0 non rejetée", "H0 rejetée")
  #   
  # })
  
  output$plot <- renderPlotly({
    pp <- ggplot(dat(), aes(x=x, y=density_H0)) + geom_line(aes(colour = "Density under H0"), size = 1)+
      geom_line(aes(x=x, y=density_H1, colour = "Density under H1"), size = 1) +
      scale_y_continuous(name = "Density") +
      geom_vline(aes(xintercept = S()), color = "black", size=1) +  geom_text(x=S()+0.3, y=-0.025, label=paste0("T = ", round(S(), 2))) +
      geom_vline(aes(xintercept = F()), linetype="dotted",color = "black", size=1)  + geom_text(x=F()+0.5, y=0.025, colour = "black",label=paste0("F_obs = ", round(F(), 2)))
    
    
    if(input$area_alpha==1) p_alpha <-  geom_ribbon(data=subset(dat() ,x>S() & x<Inf),aes(ymax=density_H0), ymin=0, fill="red", colour=NA, alpha=0.5) else p_alpha <- NULL
    if(input$area_power==1) p_power <- geom_ribbon(data=subset(dat() ,x>S() & x<Inf),aes(ymax=density_H1), ymin=0, fill="blue", colour=NA, alpha=0.2) else p_power <- NULL
    if(input$area_beta==1) p_beta <- geom_ribbon(data=subset(dat() ,x>-Inf & x<S()),aes(ymax=density_H1), ymin=0, fill="green", colour=NA, alpha=0.2) else p_beta <- NULL
    
    pp + p_alpha + p_power + p_beta
    
  })
  
  output$anova <-  renderPrint({
    anova(mod0(), mod1())
  })
  
  output$table <- renderTable({
    tmp <- data.frame("H0 true" = c(1-input$alpha, input$alpha), "HO false" = c(beta(), 1-beta()))
    rownames(tmp) <- c("H0 not rejected", "H0 rejected")
    tmp
  })
  
  output$beta <- renderPrint({ beta() })
  output$power <- renderPrint({ 1-beta() })
  output$F_obs <- renderPrint({ F() })
  output$seuil <- renderPrint({ S() })
  
  output$table <- renderTable({
    tmp <- data.frame("H0 vrai" = c(1-input$alpha, input$alpha), "HO fausse" = c(beta(), 1-beta()))
    rownames(tmp) <- c("H0 non rejetée", "H0 rejetée")
    tmp
  })
  
  output$summary_mod0 <-  renderPrint({
    summary(mod0())
  })
  
  output$summary_mod1 <-  renderPrint({
    summary(mod1())
  })
  output$n <- renderPrint({ n() })
  output$var <- renderPrint({ var(data()["y"]) })
}

# Run the application 
shinyApp(ui = ui, server = server)

