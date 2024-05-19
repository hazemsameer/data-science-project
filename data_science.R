install.packages("shiny")
library(shiny)
install.packages("ggplot2")
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("important of electreccar"),
  
  
  
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
    
    )
    
  )
shinyServer(function(input, output, session){
  barplot(new_ds$Price.DE.,name = new_ds$Efficiency,xlab = "Efficiency", ylab = "Price", col="blue")
  # We notice that most of the time, while the efficiency increases, the price also increases
  # the maximum price > 20000
  # minimum price =4000
  
  hist(new_ds$Efficiency,col = "red", border = "black", main = "Efficiency", xlab = "Efficiency", ylab = "Frequency")
  # We can see that Battery Health of Most Cars is from 150 to 200 
  
  plot(x=new_ds$Top_speed,y=new_ds$Efficiency, main = "Top_Speed vs Efficiency",xlab = "Top Speed", ylab = "Efficiency", col="blue")
  
  barplot(height = new_ds$Efficiency,names = new_ds$acceleration..0.100. , col = "skyblue", main = "what Acceleration Make on Efficiency", xlab = "Acceleration", ylab = "Efficiency")
  # we see that if acceleration increases also speed increases
  # Speed is start from 150km/h
  
  boxplot(x=new_ds$Fast_charge,main = "Distributions of Fast charging", xlab = "Charging")
  # The minimum charge is 190 and The Maximum charge is 1200
  # Median = 530 , q1 = 375 ,  q3 = 680
  # There is no Outliers
  
  #determine and visualize optimal number of clusters
  
  
  })











ds<-data.frame(read.csv("E:/edu/term 3/data science/EV_cars.csv"))

sum(duplicated(ds)) # 0

duplicated(ds) # False

                 # there isn't any duplicated values


is.numeric(ds$Battery) # True

is.character(ds$Car_name) # True

is.integer(ds$Fast_charge) #True

is.integer(ds$Price.DE.) # True

is.integer(ds$Range) # True

is.integer(ds$Top_speed) # True

is.numeric(ds$acceleration..0.100.) # True

sum(is.na(ds)) # 53

is.na(ds) # There is NA values we want to remove all of those rows

new_ds<-na.omit(ds)

print(new_ds) # We removed all Na Values

par(mfrow=c(3,2))
summary(new_ds)

barplot(new_ds$Price.DE.,name = new_ds$Efficiency,xlab = "Efficiency", ylab = "Price", col="blue")
# We notice that most of the time, while the efficiency increases, the price also increases
# the maximum price > 20000
# minimum price =4000

hist(new_ds$Efficiency,col = "red", border = "black", main = "Efficiency", xlab = "Efficiency", ylab = "Frequency")
# We can see that Battery Health of Most Cars is from 150 to 200 

plot(x=new_ds$Top_speed,y=new_ds$Efficiency, main = "Top_Speed vs Efficiency",xlab = "Top Speed", ylab = "Efficiency", col="blue")

barplot(height = new_ds$Efficiency,names = new_ds$acceleration..0.100. , col = "skyblue", main = "what Acceleration Make on Efficiency", xlab = "Acceleration", ylab = "Efficiency")
# we see that if acceleration increases also speed increases
# Speed is start from 150km/h

boxplot(x=new_ds$Fast_charge,main = "Distributions of Fast charging", xlab = "Charging")
# The minimum charge is 190 and The Maximum charge is 1200
# Median = 530 , q1 = 375 ,  q3 = 680
# There is no Outliers

#determine and visualize optimal number of clusters



 Rtree <- rpart(Efficiency~  Price.DE.+Fast_charge+Battery+Range +Top_speed+acceleration..0.100.,data=new_ds )
 rpart.plot(Rtree)
 Rtree

 
 
