library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
#install.packages("matrixStats")
#install.packages("ggpubr")
library(ggstatsplot)
library(matrixStats)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(MASS)
library(vcd)
library(caret)
# Read the data
data <- heart_disease

# Drop the null values
missing_ca_indeces <- which(data$ca %in% 4)
missing_thal_indeces <-which(data$thal %in% 0)
missing_values_indeces <- c(missing_ca_indeces, missing_thal_indeces)
data <- data[-missing_values_indeces, ]

# Transform categorical variable to R factors
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$thal <- as.factor(data$thal)
data$target <- as.factor(data$target)

# Give a better name to the factor values for the graphs
levels(data$sex) <- c("F", "M")
levels(data$cp) <- c("Asymptomatic", "Atypical angina", "No angina", "Typical angina")
levels(data$fbs) <- c("No", "Yes")
levels(data$restecg) <- c("Hypertrophy", "Normal", "Abnormalities")
levels(data$exang) <- c("No", "Yes")
levels(data$slope) <- c("Descending", "Flat", "Ascending")
levels(data$thal) <- c("Fixed defect", "Normal flow", "Reversible defect")
levels(data$target) <- c("Yes", "No")


sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("Des_stats" ,tabName = "Des_stats", icon = icon("dashboard")),
    menuItem("Chi-Square",tabName = "Chi-Square", icon = icon("calendar")),
    menuItem("MLM", tabName = "MLM", icon = icon("chart-line"))
    
  ))

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'Des_stats',
      fluidPage(
        titlePanel("Descriptive analysis"),
          fluidRow(
            column(
              width = 12,
              tabPanel(
                "Select Variables",
                selectInput("var",label=h3("select by variable"),
                            choices = c("age"=1,
                                        "sex"=2,
                                        "cp"=3,
                                        "trestbps"=4,
                                        "chol"=5,
                                        "fbs"=6,
                                        "restecg"=7,
                                        "thalach"=8,
                                        "exang"=9,
                                        "oldpeak"=10,
                                        "slope"=11,
                                        "ca"=12,
                                        "thal"=13
                            ),selected = 1
                ),
                fluidRow(width = 8,height = 200,
                             verbatimTextOutput("text")
                ),
                  
                fluidRow(
                  box(width=6,
                      plotly::plotlyOutput(outputId = "plot_var")
                  ),
                  box(width=12,
                    DT::DTOutput('var_2')
                  )
              
                )
                
              )
              
            )
          )
    )
    
  ),
      tabItem(
        tabName = 'Chi-Square',
        fluidPage(
          titlePanel("Chi-Square Testing"),
            fluidRow(
              column(
                width=12,
                tabPanel("Chi-Square variables"),
                selectInput("var_c",label=h3("select by variable"),
                            choices = c("sex"=2,
                                        "cp"=3, 
                                        "fbs"=6,
                                        "restecg"=7, 
                                        "exang"=9,
                                        "slope"=11,
                                        "ca"=12,
                                        "thal"=13
                            ),selected = 2
                ),
            fluidRow(
              box(width = 6,
                verbatimTextOutput("cf")
              ),
              box(width = 6,
                  verbatimTextOutput("cf_3")
                  ),
              box(width = 12,
                  plotOutput("cf_2")
              )
            )
                
              )
            )
          
        )
        
      ),
  tabItem(tabName = "MLM",
          fluidPage(
            titlePanel("Machine Learning Model"),
            fluidRow(
              column(
                width = 12,
                height = 100,
                tabsetPanel(
                  tabPanel("Naive bayes",

                           box(
                             width = 12,
                             h4('Model Summary:'),
                             verbatimTextOutput("lb1"),
                           ),
                           box(
                             width = 12,
                            
                             plotOutput("lb1_2"),
                           )
                  ),
                  
                  
                  tabPanel("Logistic regression",

                           box(
                             width = 12,
                             h4('Model Summary:'),
                             verbatimTextOutput("lb2"),
                           ),
                           box(
                             width=12,
                             plotOutput("lb2_2")
                           )
                  
                           
                  ),
                  tabPanel("Decision Tree",
                           
                           box(
                             width = 12,
                             h4('Model Summary:'),
                             verbatimTextOutput("lb3"),
                           ),
                           box(
                             width=12,
                             plotOutput("lb3_2"),
                           )
                           
                           
                  )
                )
              )
            )
          )
  )

  
  
)
)

ui <- dashboardPage(
  dashboardHeader(title = "Heart Disease Exploration"),
  sidebar,
  body
)

server <- function(input, output) {
  output$text <- renderPrint({
  fe<-input$var
  if(fe=='1'){print("Patient age in years. In the data we can see, as expected, that age is a risk factor. In other words, the higher the age, the more likely that the patient has a heart disease.")}
  else if (fe=='2'){print("We can also see that sex is a risk factor, like some of the referencesn indicate, men are more likely to have a heart disease than women.")}
  else if (fe=='3'){print("Chest pain type. It seems that patients with Asymptomatic are more likely have a heart disease than others")}
  else if (fe=='4'){print("Resting blood pressure in millimeters of mercury (mm Hg) when the patient was admitted to the hospital. Very high pressures can indicate that there is a heart disease")}
  else if (fe=='5'){print("Cholesterol level in mg/dl.It also looks like up to a certain level, the presence of a heart disease is slightly higher on higher cholesterol levels")}
  else if (fe=='6'){print("Whether the level of sugar in the blood is higher than 120 mg/dl or not. It's hard to judge situation of patients by this variables.")}
  else if (fe=='7'){print("Results of the electrocardiogram on rest. Little difference between patients with heart disease and general person.")}
  else if (fe=='8'){print("Maxium heart rate during the stress test. Higher rates tend to be from younger people without heart disease")}
  else if (fe=='9'){print("Whether the patient had angina during exercise.We can see that this feature is a good indicator for the presence of heart disease")}
  else if (fe=='10'){print("Decrease of the ST segment during exercise according to the same one on rest.")}
  else if (fe=='11'){print("Slope of the ST segment during the most demanding part of the exercise.we can see that the slope by itself can help determine whether there is a heart disease or not if it is flat or ascending")}
  else if (fe=='12'){print("Results of the blood flow observed via the radioactive dye.A very good indication of the presence of a heart disease or not.")}
  else if (fe=='13'){print("Number of main blood vessels coloured by the radioactive dye. The higher the value of this feature, the more likely it is to have a heart disease")}
    
  })
output$plot_var <-plotly::renderPlotly(
    {
fe<-input$var
if(fe=='1'){x<-data$age}
else if (fe=='2'){x<-data$sex}
else if (fe=='3'){x<-data$cp}
else if (fe=='4'){x<-data$trestbps}
else if (fe=='5'){x<-data$chol}
else if (fe=='6'){x<-data$fbs}
else if (fe=='7'){x<-data$restecg}
else if (fe=='8'){x<-data$thalach}
else if (fe=='9'){x<-data$exang}
else if (fe=='10'){x<-data$oldpeak}
else if (fe=='11'){x<-data$slope}
else if (fe=='12'){x<-data$ca}
else if (fe=='13'){x<-data$thal}
      
      if(fe=='2'|fe=='3'|fe=='6'|fe=='7'|fe=='9'|fe=='11'|fe=='12'|fe=='13'){
        ggplot(data, aes(x, fill=target)) +
        geom_bar() + 
        labs(fill="Disease", y="Number of patients")}
      else if(fe=='1'|fe=='4'|fe=='5'|fe=='8'|fe=='10'){
        ggplot(data, aes(x, fill=target)) +
        geom_histogram(binwidth=3) +
        labs(fill="Disease", y="Number of patients")}
  
    }
  )
  
  output$var_2 <- DT::renderDT({
    data %>% 
      filter()
    
  })

  output$cf <- renderPrint({
    fe<-input$var_c
    
    if (fe=='2'){x<-data$sex}
    else if (fe=='3'){x<-data$cp}
    
    else if (fe=='6'){x<-data$fbs}
    else if (fe=='7'){x<-data$restecg}
    else if (fe=='9'){x<-data$exang}
    else if (fe=='11'){x<-data$slope}
    else if (fe=='12'){x<-data$ca}
    else if (fe=='13'){x<-data$thal}
    # 关联性卡方检验
    hd<-xtabs(~target+x,data=data)
    Result <- chisq.test(hd,correct=FALSE)
    print(Result)
    print(Result$residuals)

  })
  
  output$cf_2 <-renderPlot(
    {
      fe<-input$var_c
      
      if (fe=='2'){x<-data$sex}
      else if (fe=='3'){x<-data$cp}
      
      else if (fe=='6'){x<-data$fbs}
      else if (fe=='7'){x<-data$restecg}
      else if (fe=='9'){x<-data$exang}
      else if (fe=='11'){x<-data$slope}
      else if (fe=='12'){x<-data$ca}
      else if (fe=='13'){x<-data$thal}
      hd<-xtabs(~target+x,data=data)
       mosaic(hd, shade = TRUE, legend = TRUE)
      
    }
  )
  output$cf_3 <- renderPrint({
    fe<-input$var_c
    
    if (fe=='2'){x<-data$sex}
    else if (fe=='3'){x<-data$cp}
    
    else if (fe=='6'){x<-data$fbs}
    else if (fe=='7'){x<-data$restecg}
    else if (fe=='9'){x<-data$exang}
    else if (fe=='11'){x<-data$slope}
    else if (fe=='12'){x<-data$ca}
    else if (fe=='13'){x<-data$thal}
    # Chi-Square testing
    hd<-xtabs(~target+x,data=data)
    Result <- chisq.test(hd,correct=FALSE)
    if(Result$p.value<0.05){cat('-------------------    Chi-Square Testing     --------------------\n')
                            
                            cat("Conclusion:")
                            
                            cat("\n")
                            cat("Since ",Result$p.value," is < 5%, then we reject the H0. So we have enough evidence to conclude  ")
                            cat("\n")
                            cat("\n")
                            cat("that there is a association between this variable and heart disease.")
                            cat("\n")
                            cat("\n")
                            cat("Recommendation: Doctors should pay more attention to this indicator In the process of diagnosing  ")
                            cat("\n")
                            cat("\n")
                            cat("and treating heart disease.")}
    else{cat('-------------------    Chi-Square Testing     --------------------\n')
      
      cat("Conclusion:")
      
      cat("\n")
      cat("Since ",Result$p.value," is > 5%, then we can not reject the H0. So we don't have enough evidence to conclude  ")
      cat("\n")
      cat("\n")
      cat("that there is a association between this variable and heart disease.")
      cat("\n")
      cat("\n")
      cat("Recommendation: Doctors should pay less attention to this indicator In the process of diagnosing  ")
      cat("\n")
      cat("\n")
      cat("and treating heart disease.")}
   
  })

  output$lb1 <- renderPrint({
    
    cat('-------------------    Naive Bayes Modeling     --------------------\n')
    library(tidyverse)
    data <-heart_disease
    set.seed(2021)
    select <- sample(1:nrow(data),nrow(data)*0.8)
    
    train <- data[select,]
    test <- data[-select,]
    
    library(e1071)
    nb_default <- naiveBayes(target ~ ., data=train)
    
    
    
    test.y_hat <- predict(nb_default, test, type="class")
    
    test.y_hat_prob <- round(predict(nb_default, test, type="raw"),3)
    cbind(Prediction=as.character(test.y_hat), test.y_hat_prob)
    
    accuracy.nb_default <- sum(test.y_hat==test$target) / length(test$target)
    cat("The accuracy of final model is ",accuracy.nb_default)
    cat("\n")
    cat("\n")
    agreement_KNN <- test.y_hat==test$target
    cat("\n")
    cat("\n")
    print(table(test.y_hat, test$target, dnn=c("Prediction","Actual")))
    cat("\n")
    cat("\n")
    print(cbind(Prediction=as.character(test.y_hat), test.y_hat_prob))
    
    
    
    

    
    
  })
  output$lb1_2<-renderPlot({
    heart2 <- data
    ind <- createDataPartition(heart2$target,times = 1,p=0.75,list = F)
    heart_train <- heart2[ind,]
    heart_test <- heart2[-ind,]
    fitControl <- trainControl(method="cv", number=10)
    set.seed(8)
    model.nb <- train(target ~ ., 
                      data = heart_train,
                      method = "naive_bayes",
                      trControl = fitControl)
    plot(model.nb)
  })
  
  output$lb2<-renderPrint({
    cat('-------------------    Logistic Regression Modeling     --------------------\n')
    data <-distinct(heart_disease)
    
    data$cp= as.factor(data$cp)
    data$restecg= as.factor(data$restecg)
    data$thal= as.factor(data$thal)
    data$target= as.factor(data$target)
    data$ca=as.factor(data$ca)
    data$slope=as.factor(data$slope)
    
    sapply(data,function(x) sum(is.na(x)))
    apply(data,2,anyNA)
    
    train <- data[60:302,]
    test <- data[1:60,]
    
    model1=glm(formula=target~.,  family="binomial"(link='logit'),data=train)
    
    anova(model1, test="Chisq")
    model2=glm(formula=target~.-chol-restecg-fbs-slope,  family="binomial"(link='logit'),data=train)
    print(model2)
    
    fitted.values <- predict(model2,newdata=test,type='response')
    fitted.values <- ifelse(fitted.values > 0.5,1,0)
    ClassificError <- mean(fitted.values != test$target)
    print(paste('Accuracy',1-ClassificError))
  })
  
  output$lb2_2<-renderPlot({
    #logistic regression
    data <-distinct(heart_disease)
    
    data$cp= as.factor(data$cp)
    data$restecg= as.factor(data$restecg)
    data$thal= as.factor(data$thal)
    data$target= as.factor(data$target)
    data$ca=as.factor(data$ca)
    data$slope=as.factor(data$slope)
    
    sapply(data,function(x) sum(is.na(x)))
    apply(data,2,anyNA)
    
    train <- data[60:302,]
    test <- data[1:60,]
    
    model1=glm(formula=target~.,  family="binomial"(link='logit'),data=train)
    
    anova(model1, test="Chisq")
    model2=glm(formula=target~.-chol-restecg-fbs-slope,  family="binomial"(link='logit'),data=train)
    summary(model2)
    
    fitted.values <- predict(model2,newdata=test,type='response')
    fitted.values <- ifelse(fitted.values > 0.5,1,0)
    ClassificError <- mean(fitted.values != test$target)
    print(paste('Accuracy',1-ClassificError))
    
    library(ROCR)
    prediction=predict(model2,data,type="response")
    pred = prediction(prediction, data$target)
    perf = performance(pred,"tpr", "fpr")
    plot(perf,colorize = TRUE,main="Curva ROC")
    abline(0, 1, lty = 2)
    (performance(pred,"auc")@y.values)[[1]] 
  })
  
  
  

  
  output$lb3 <- renderPrint({
    heart <- data
    ind <- createDataPartition(heart$target,p=0.75,list = F)
    heart_train <- heart[ind,]
    heart_test <- heart[-ind,] 
    fitControl <- trainControl(method="cv", number=10)
    set.seed(8)
    model.tree <- train(target ~ ., 
                        data = heart_train,
                        method = "rpart",
                        trControl = fitControl)
    cat('-------------------    Desicion Tree Modeling     --------------------\n')
    return(model.tree)

  })
  output$lb3_2 <- renderPlot({
    heart <- data
    ind <- createDataPartition(heart$target,times = 1,p=0.75,list = F)
    heart_train <- heart[ind,]
    heart_test <- heart[-ind,] 
    fitControl <- trainControl(method="cv", number=10)
    set.seed(8)
    model.tree <- train(target ~ ., 
                        data = heart_train,
                        method = "rpart",
                        trControl = fitControl)
    plot(model.tree)
    
  })

  
  
}
shinyApp(ui, server)



















