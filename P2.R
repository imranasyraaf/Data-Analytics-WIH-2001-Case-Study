library(ggplot2)
library(data.table)
library(dplyr)

#load data
data <- fread('https://vincentarelbundock.github.io/Rdatasets/csv/Stat2Data/Hawks.csv')
head(data)

#check data na value
sum(is.na(data))
cbind(lapply(lapply(data, is.na), sum))

#delete too many na
myData = select(data, -16:-20)

#check data na value
sum(is.na(myData))
cbind(lapply(lapply(myData, is.na), sum))

#delete na value
# Remove some rows with missing weight or wing data
myData2 <- myData[!is.na(Weight) & !is.na(Wing) & !is.na(Culmen)& !is.na(Hallux)]

#check data na value
sum(is.na(myData2))
cbind(lapply(lapply(myData2, is.na), sum))

str(myData3)

#change string to factor 
myData3 <- as.data.frame(unclass(myData2), stringsAsFactors = TRUE)

dim(myData3)
str(myData3)
summary(myData3)

# multiple scatterplots
pairs(myData3[, c("Wing", "Weight", "Tail","Culmen","Hallux")])

# correlation for all variables
round(cor(myData3[, 11:15]),
      digits = 2 # rounded to 2 decimals
)

# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch=19, col=c("red", "green3", "blue")[myData3$Species])
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(myData3[,11:15], lower.panel = NULL, 
      upper.panel = upper.panel)

library(psych)
pairs.panels(myData3[,11:15], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             lm = TRUE
)
