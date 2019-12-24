library(shiny)
library(ggplot2)
library("readxl")
library(plyr)
library(reshape2)
library(latex2exp)
library(gridExtra)
library(ggiraph)
library(ggiraphExtra)
library(tidyr)
library(tidyverse)  
library(modelr)    
library(broom)     

##
##  Data loading
##
dataset1 <- read_excel("/Users/mertyenilmez/Desktop/Project1/LungCapData.xls")
dataset <- subset(dataset1, Age>14)

##
##  UI Part of our Shiny Project. This part includes Title Panel, Side Panel, Main Panel
##  Conditional Panels, Drop-down Boxes, Radio Buttons and Tabset Panels 
##
ui <- fluidPage(
  
  titlePanel("Statistics CA-1"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      ##
      ##  Drop-down Box
      ##
      selectInput("select","Please select", choices = c("Descriptive Analytics",
                                                        "Probabilistic Models",
                                                        "Linear Regression",
                                                        "Hypothesis Testing")),
      ##
      ##  Descriptive Analytics has 2 Radio Buttons. One of them is for Box Plot and
      ##  the second one is for Density Curve
      ##
      conditionalPanel(
        condition = "input.select == 'Descriptive Analytics'",
        radioButtons("radio1", label = h3("Radio buttons"),
                     choices = list("Box Plot" = 1, "Density Curve" = 2), 
                     selected = 1)
      ),
      
      ##
      ##  Probabilistic Models has 2 Radio Buttons. One of them is for Normal Distribution and
      ##  the second one is for Binomial Distribution
      ##
      conditionalPanel(
        condition = "input.select == 'Probabilistic Models'",
        radioButtons("radio2", label = h3("Radio buttons"),
                     choices = list("Normal Distribution" = 1, "Binomial Distribution" = 2), 
                     selected = 1)
      )
    ),
    
    ##
    ##  Main Panel includes Plot Tab, Summary Tab and Table Tab. It also has Fluid Rows
    ##  below the Plot to show our data's main insights
    ##
    mainPanel(
      
      tabsetPanel(type = "tabs", tabPanel("Plot", 
                                          
                                          br(), 
                                          
                                          ##
                                          ##  If Drop-Down Box is Descriptive Analytics
                                          ##  and Radio Button is "Box Plot"
                                          ##
                                          conditionalPanel(
                                            condition = "input.radio1 == '1' & input.select == 'Descriptive Analytics'",
                                            plotOutput("plot"), 
                                            
                                            br(),
                                            
                                            ##
                                            ##  Fluid Rows for showing Percentiles, Medians and 
                                            ##  Max and Min Lung Capacities of both males and females
                                            ##
                                            fluidRow(
                                              column(width = 4,
                                                     verbatimTextOutput("one")
                                              ),
                                              column(width = 4,
                                                     verbatimTextOutput("two")
                                              ),
                                              column(width = 4,
                                                     verbatimTextOutput("three")
                                              )
                                            ),
                                            
                                            br(),
                                            
                                            fluidRow(
                                              column(width = 4,
                                                     verbatimTextOutput("one2")
                                              ),
                                              column(width = 4,
                                                     verbatimTextOutput("two2")
                                              ),
                                              column(width = 4,
                                                     verbatimTextOutput("three2")
                                              )
                                            ),
                                            
                                            br(),
                                            
                                            fluidRow(
                                              column(width = 3,
                                                     verbatimTextOutput("one3")
                                              ),
                                              column(width = 3,
                                                     verbatimTextOutput("two3")
                                              ),
                                              column(width = 3,
                                                     verbatimTextOutput("three3")
                                              ),
                                              column(width = 3,
                                                     verbatimTextOutput("four3")
                                              )
                                            )
                                          ),
                                          
                                          ##
                                          ##  If Drop-Down Box is Descriptive Analytics
                                          ##  and Radio Button is "Density Curve"
                                          ##
                                          conditionalPanel(
                                            condition = "input.radio1 == '2' & input.select == 'Descriptive Analytics'",
                                            plotOutput("plot2"), 
                                            
                                            br(),
                                            
                                            ##
                                            ##  Fluid Row for showing mean of males and females 
                                            ##  lung capacities
                                            ##
                                            fluidRow(
                                              column(width = 5,
                                                     verbatimTextOutput("one4")
                                              ),
                                              column(width = 5,
                                                     verbatimTextOutput("two4")
                                              )
                                            )
                                          ),
                                          
                                          ##
                                          ##  If Drop-Down Box is Probabilistic Models
                                          ##  and Radio Button is "Normal Distribution"
                                          ##
                                          conditionalPanel(
                                            condition = "input.radio2 == '1' & input.select == 'Probabilistic Models'",
                                            plotOutput("plot3"), 
                                            
                                            br(),
                                            
                                            ##
                                            ##  Fluid Row for showing mean and standart
                                            ##  deviation of males and females heights
                                            ##
                                            fluidRow(
                                              column(width = 6,
                                                     verbatimTextOutput("one5")
                                              ),
                                              column(width = 6,
                                                     verbatimTextOutput("two5")
                                              )
                                            ),
                                            
                                            br(),
                                            
                                            fluidRow(
                                              column(width = 6,
                                                     verbatimTextOutput("one6")
                                              ),
                                              column(width = 6,
                                                     verbatimTextOutput("two6")
                                              )
                                            )
                                          ),
                                          
                                          ##
                                          ##  If Drop-Down Box is Probabilistic Models
                                          ##  and Radio Button is "Binomial Distribution"
                                          ##
                                          conditionalPanel(
                                            condition = "input.radio2 == '2' & input.select == 'Probabilistic Models'",
                                            
                                            mainPanel("Probability of choosing female smokers by random 44 attempts"),
                                            
                                            br(),
                                            br(),
                                            
                                            plotOutput("plot4"), 
                                            
                                            br(),
                                            
                                            ##
                                            ##  Fluid Row for showing Binomial Probabilities and
                                            ##  Binomial Distribution Presentation
                                            ##
                                            fluidRow(
                                              column(width = 6,
                                                     verbatimTextOutput("one7")
                                              ),
                                              column(width = 6,
                                                     verbatimTextOutput("two7")
                                              )
                                            ),
                                            
                                            br(),
                                            
                                            
                                            fluidRow(
                                              column(width = 6,
                                                     verbatimTextOutput("one8")
                                              ),
                                              column(width = 6,
                                                     verbatimTextOutput("two8")
                                              )
                                            ),
                                            
                                            br(),
                                            
                                            fluidRow(
                                              column(width = 12,
                                                     verbatimTextOutput("one9")
                                              )
                                            )
                                            
                                            
                                          ),
                                          
                                          ##
                                          ##  If Drop-Down Box is selected as Linear Regression
                                          ##
                                          conditionalPanel(
                                            condition = "input.select == 'Linear Regression'",
                                            
                                            ggiraphOutput("plot5"), 
                                            
                                            br(),
                                            
                                            ##
                                            ##  Fluid Row for showing estimates, std.errors, statistics,
                                            ##  p.values and Intercepts of Smoking and and Non-smoking
                                            ##  people's Lung Capacities and Heights
                                            ##
                                            fluidRow(
                                              column(width = 6,
                                                     verbatimTextOutput("one10")
                                              ),
                                              column(width = 6,
                                                     verbatimTextOutput("two10")
                                              )
                                            )
                                          ),
                                          
                                          ##
                                          ##  If Drop-Down Box is selected as Hypothesis Testing
                                          ##
                                          conditionalPanel(
                                            condition = "input.select == 'Hypothesis Testing'",
                                            
                                            plotOutput("plot6"), 
                                            
                                            br(),
                                            
                                            ##
                                            ##  Fluid Row for showing our Hypothesis Test results
                                            ##
                                            fluidRow(
                                              column(width = 6,
                                                     verbatimTextOutput("one11")
                                              ),
                                              column(width = 6,
                                                     verbatimTextOutput("two11")
                                              )
                                            )
                                          )
      ),
      
      ##
      ##  Summary Tabs for all of our plots
      ##
      tabPanel("Summary", 
               
               br(), 
               
               conditionalPanel(
                 condition = "input.radio2 == '1' & input.select == 'Probabilistic Models'",
               verbatimTextOutput("summaryAll")),
               
               conditionalPanel(
                 condition = "input.radio2 == '2' & input.select == 'Probabilistic Models'",
                 verbatimTextOutput("summary2")),
      
               conditionalPanel(
                 condition = "input.radio2 == '1' & input.select == 'Descriptive Analytics'",
                 verbatimTextOutput("summary")),
               
               conditionalPanel(
                 condition = "input.select == 'Linear Regression'",
                 verbatimTextOutput("summaryAll2")),
               
               conditionalPanel(
                 condition = "input.select == 'Hypothesis Testing'",
                 verbatimTextOutput("summaryAll3")),
               ),
      
      ##
      ##  Table Tabs for all of our plots
      ##
      tabPanel("Table", 
               
               br(), 
               
               conditionalPanel(
                 condition = "input.radio2 == '1' & input.select == 'Probabilistic Models'",
               tableOutput("tableAll")),

               conditionalPanel(
                 condition = "input.select == 'Descriptive Analytics'",
                 tableOutput("table")),
               
               conditionalPanel(
                 condition = "input.radio2 == '2' & input.select == 'Probabilistic Models'",
                 tableOutput("table2")),
               
               conditionalPanel(
                 condition = "input.select == 'Linear Regression'",
                 tableOutput("tableAll2")),
               
               conditionalPanel(
                 condition = "input.select == 'Hypothesis Testing'",
                 tableOutput("tableAll3"))
               
               )
      ),
    )
  )
)

##
##  Server side of our project includes plotting codes, summary codes, 
##  table codes and fluidRow Text codes
##    
server <- function(input, output) {
  
  ##
  ##  Divides our dataset into parts.
  ##   
  smokers <- subset(dataset,Smoke=='yes')
  smokersLungCap <- smokers$LungCap
  nonSmokers <- subset(dataset,Smoke=='no')
  nonSmokersLungCap <- nonSmokers$LungCap
  males <- subset(dataset,Gender=='male')
  malesLungCap <- males$LungCap
  females <- subset(dataset,Gender=='female')
  femalesLungCap <- females$LungCap
  allmales <- subset(dataset1,Gender=='male')
  allfemales <- subset(dataset1,Gender=='female')
  femaleSmokers <- subset(smokers,Gender=='female')

  ##
  ##  Plots Box Plot
  ##   
  output$plot <- renderPlot({
    
    ggplot(dataset, aes(x=Smoke, y=LungCap)) +
      geom_point(aes(fill=Gender), size=3, shape=21, colour="grey15",
                 position=position_jitter(width=0.3, height=0.1)) +
      geom_boxplot(outlier.colour=NA, fill=NA, colour="grey15") +
      labs(title="Over 14 Years Smokers & Non-Smokers Lung Capacities (litre)") + 
      theme(legend.position="bottom")
    
  })

  mu <- ddply(dataset, "Gender", summarise, grp.mean=mean(LungCap))
  head(mu)
  
  ##
  ##  Plots Density Curve
  ##   
  output$plot2 <- renderPlot({
    
    p <- qplot(x = LungCap, data = dataset, geom = "blank", fill=Gender) +geom_density(alpha=.2)+
      geom_histogram(aes(y=..density..),position = "identity", alpha=0.5)
    p <- p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
    p <- p + theme(legend.position="bottom")
    p <- p + labs(title="Over 14 Years Males & Females Lung Capacities (litre)",y='Density')
    p
    
  })
  
  ##
  ##  Plots Normal Distribution
  ##  
  output$plot3 <- renderPlot({
    plot <- ggplot(dataset1, aes(x=Height))  + labs(title="Males & Females Heights(cm) Normal Distribution",y='') +
      stat_function(aes(color = "male"), fun = dnorm, args = list(mean = mean(allmales$Height), sd = sd(allmales$Height))) + 
      geom_vline(aes(xintercept = mean(allmales$Height, na.rm = T)), colour = "blue", linetype ="longdash", size = .8) + 
      stat_function(aes(color = "female"),fun = dnorm, args = list(mean = mean(allfemales$Height), sd = sd(allfemales$Height))) + 
      geom_vline(aes(xintercept = mean(allfemales$Height, na.rm = T)), colour = "red", linetype ="longdash", size = .8)
    plot <- plot + scale_colour_manual("Gender", values = c("red", "blue"))
    plot <- plot + theme(legend.position="bottom")
    plot
    
  })
  
  ##
  ##  Plots Binomial Distribution
  ##  
  output$plot4 <- renderPlot({
    
    ##
    ##  First image
    ##  
    h3(textOutput("caption"))
 
    smokers <- subset(dataset,Smoke=='yes')
    smokers
    femaleSmokers <- subset(smokers,Gender=='female')
    femaleSmokers
    
    p <- nrow(femaleSmokers) / nrow(smokers)
    p
    df <- data.frame(x=0:nrow(smokers), z=dbinom(0:nrow(smokers), size=nrow(smokers), prob = p));
    g <- ggplot(df)  + 
      geom_line(aes(x=x, y=z), color="lightgray") + 
      geom_point(aes(x=x, y=z), size=2, color="Steelblue") +
      scale_x_continuous(breaks=seq(0, nrow(smokers), 4), name=TeX("n_{ smokers}")) +
      scale_y_continuous(name=TeX("P_{getting x female smokers}")) + labs(title = "Probability Density Function")+
      
      annotate("text", size=3, x=24, y=dbinom(24, size=nrow(smokers), prob = p)+0.005, label="dbinom(24,44,0.54)=0.120") 
    
    ##
    ##  Second image
    ##  
    df2 <- data.frame(q=0:nrow(smokers), z=pbinom(0:nrow(smokers), size=nrow(smokers), prob = p));
    g2 <- ggplot(df2)  + 
      geom_line(aes(x=q, y=z), color="lightgray") + 
      geom_point(aes(x=q, y=z), size=2, color="Steelblue") + labs(title = "Cumulative Density Function")+
      scale_x_continuous(breaks=seq(0, nrow(smokers), 4), name=TeX("n_{ smokers}")) +
      scale_y_continuous(name=TeX("Probability")) +
      annotate("text", size=3, x=30.5, y=pbinom(24, size=nrow(smokers), prob = p), label="pbinom(24,44,0.54)=0.56") 
    
    ##
    ##  Combination of multiplots
    ##  
    source("http://peterhaschke.com/Code/multiplot.R")
    multiplot(g, g2, cols=2)
  })
  
  ##
  ##  Plots Linear Regression Model
  ##  
  output$plot5 <- renderggiraph({
    
    fit1=lm(LungCap~Height+Smoke,data=dataset1)
    summary(fit1)
    
    ##
    ##  2 equation for 2 linear lines
    ##  
    equation1=function(x){coef(fit1)[2]*x+coef(fit1)[1]}
    equation2=function(x){coef(fit1)[2]*x+coef(fit1)[1]}
    
    a<-ggplot(dataset1,aes(y=LungCap,x=Height,color=Smoke))+geom_point()+
      stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
      stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])
    a
    
    ##
    ##  Code for adding zoom and choice features
    ## 
    ggPredict(fit1,se=FALSE,interactive=TRUE)
    
  })   
  
  ##
  ##  Plots Hypothesis testing plot
  ## 
  output$plot6 <- renderPlot({
    
    data_vector <- dataset1$LungCap
    dfs <- length(data_vector)-1
    x_ <- seq(-8,8,0.1)
    y <- dt(x_,dfs)
    t.val <- qt(1-0.05,df=dfs)
    plot(x_,y,type='l',lwd=3,col='blue',xlab='x',ylab='Density',main='Lung Cap Distribution')
    abline(v=0)
    
    ##
    ##  Adds red lines onto 2 side of normally distributed data graph
    ##
    abline(v=t.val,lwd=2,col='red')
    abline(v=-t.val,lwd=2,col='red')
    points(t.val,dt(t.val,dfs),lwd=3,col='red')
    points(-t.val,dt(-t.val,dfs),lwd=3,col='red')
    
  })
    
  ##
  ##  Codes for Texts below Plots
  ## 
  output$one <- renderPrint({
    cat("25th Percentile(Q1) of Smokers'\n Lung Capacity:" , quantile(smokersLungCap, .25))
  })
  output$two <- renderPrint({
    cat("Median of Smokers' Lung Capacity:\n" , median(smokersLungCap))
  })
  output$three <- renderPrint({
    cat("75th Percentile(Q3) of Smokers'\n Lung Capacity:" , quantile(smokersLungCap, .75))
  })
  
  output$one2 <- renderPrint({
    cat("25th Percentile(Q1) of non-Smokers'\n Lung Capacity:" , quantile(nonSmokersLungCap, .25))
  })
  output$two2 <- renderPrint({
    cat("Median of non-Smokers' Lung \nCapacity:" , median(nonSmokersLungCap))
  })
  output$three2 <- renderPrint({
    cat("75th Percentile(Q3) of non-Smokers'\n Lung Capacity:" , quantile(nonSmokersLungCap, .75))
  })
  
  output$one3 <- renderPrint({
    cat("Minimum Lung Capacity \nof Smokers' :" , min(smokersLungCap))
  })
  output$two3 <- renderPrint({
    cat("Maximum Lung Capacity \nof Smokers' :" , max(smokersLungCap))
  })
  output$three3 <- renderPrint({
    cat("Minimum Lung Capacity \nof non-Smokers' :" , min(nonSmokersLungCap))
  })
  output$four3 <- renderPrint({
    cat("Maximum Lung Capacity \nof non-Smokers' :" , max(nonSmokersLungCap))
  })
  
  output$one4 <- renderPrint({
    cat("Mean of Males' Lung Capacity :" , mean(malesLungCap))
  })
  output$two4 <- renderPrint({
    cat("Mean of Females' Lung Capacity :" , mean(femalesLungCap))
  })
  
  output$one5 <- renderPrint({
    cat("Mean of Males' Heights :" , mean(allmales$Height))
  })
  output$two5 <- renderPrint({
    cat("Mean of Females' Heights :" , mean(allfemales$Height))
  })
  output$one6 <- renderPrint({
    cat("Standard Deviation of Males' Heights :" , sd(allmales$Height))
  })
  output$two6 <- renderPrint({
    cat("Standard Deviation of Females' Heights :" , sd(allfemales$Height))
  })
  output$one7 <- renderPrint({
    cat("Number of female smokers :" , nrow(femaleSmokers))
  })
  output$two7 <- renderPrint({
    cat("Number of total smokers :" , nrow(smokers))
  })
  output$one8 <- renderPrint({
    cat("Probability of having exactly 24 female smokers \nby random 44 trials :" , dbinom(24, size=nrow(smokers), prob = p))
  })
  output$two8 <- renderPrint({
    cat("Probability of choosing 24 or less female smokers \nby random 44 trials :" , pbinom(24, size=nrow(smokers), prob = p))
  })
  
  distTable = dbinom(0:44, size=nrow(smokers), prob = p)
  
  output$one9 <- renderPrint({
    cat("Binomial Distribution Presentation in the Form of a Table :\n\n")
    paste(distTable)
  })
  output$one10 <- renderPrint({
    tidy(fit1)
  })
  output$two10 <- renderPrint({ 
    confint(fit1)
  })
  output$one11 <- renderPrint({ 

    ##
    ##  Codes for decision making of Hypothesis Testing part.
    ##
    t.test.twoTails <- function(data, mu0, alpha)
    {
      t.stat <- abs((mean(data) - mu0)) / (sqrt(var(data) / length(data)))
      dof <- length(data) - 1
      t.critical <- qt(1-alpha/2, df= dof) 
      p.value <- 2*(1-pt(t.stat, df= dof))
      if(t.stat >= t.critical)
      {
        print("Reject H0")
      }
      else
      {
        print("Accept H0")
      }
      print('T statistic')
      print(t.stat)
      print('T critical values')
      print(c(-t.critical,t.critical))
      print('P value')
      print(p.value)
      print("#####################")
      return(t.stat)
    }
    cat("H0: mu = 8\n")
    cat("H1: mu != 8\n")
    t.test.twoTails(data_vector, 8, 0.05)
  })
  
  output$two11 <- renderPrint({ #hypo plot page
    cat("H0: mu = 18\n")
    cat("H1: mu != 18\n")
    t.test.twoTails(data_vector, 18, 0.05)
  })
  
  ##
  ##  Codes for summary tabs
  ##
  output$summary <- renderPrint({
    summary(dataset)
  })
  
  output$summary2 <- renderPrint({
    summary(dataset)
  })
  
  output$summaryAll <- renderPrint({
    summary(dataset1)
  })
  
  output$summaryAll2 <- renderPrint({
    summary(dataset1)
  })
  
  output$summaryAll3 <- renderPrint({
    summary(dataset1)
  })
  
  ##
  ##  Codes for Table tabs
  ##
  output$table <- renderTable({
    dataset
  })    
  
  output$table2 <- renderTable({
    dataset
  })     
  
  output$tableAll <- renderTable({
    dataset1
  }) 
  
  output$tableAll2 <- renderTable({
    dataset1
  }) 
  
  output$tableAll3 <- renderTable({
    dataset1
  }) 
  
}

shinyApp(ui, server)