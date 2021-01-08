## ----setup, include=FALSE, warning=FALSE-----------------------------
knitr::opts_chunk$set(echo = FALSE, fig.align = 'center')


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE-----
# Install all needed libraries if it is not present
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gbm)) install.packages("gbm")
if(!require(dplyr)) install.packages("dplyr")
if(!require(caret)) install.packages("caret")
if(!require(xgboost)) install.packages("xgboost")
if(!require(e1071)) install.packages("e1071")
if(!require(class)) install.packages("class")
if(!require(ROCR)) install.packages("ROCR")
if(!require(randomForest)) install.packages("randomForest")
if(!require(PRROC)) install.packages("PRROC")
if(!require(reshape2)) install.packages("reshape2")


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE-----
# Loading all needed libraries
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(ggplot2)
library(gbm)
library(caret)
library(xgboost)
library(e1071)
library(class)
library(lightgbm)
library(ROCR)
library(randomForest)
library(PRROC)
library(reshape2)
library(dplyr)
library(ggplot2)
library(lubridate)
library(h2o)
library(stringr)
library(formatR)
library(recosystem)
library(knitr)
library(scales)
library(knitr)


#loading files
if(!file.exists("parkinsons.data")){
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data", "parkinsons.data")
}

pd <- read.csv("parkinsons.data") # Point to local copy


## overview data set
str(pd)


## --------------------------------------------------------------------
summary(pd)



#Separate data into Parkinsons and healthy individuals
hth <- subset(pd, status=="0")
pdi <- subset(pd, status=="1")
str(pdi)
summary(pdi)





#overview the healthy group
str(hth)
summary(hth)

#plotting
pd$status <- as.factor(pd$status)
p <- ggplot(pd, aes(x=status, y=Jitter.DDP)) + geom_boxplot()
p + ggtitle("Jitter.DDP") +
  xlab("Status") + ylab("Hz") + labs(title="Measure of variation in fundamental frequency(Jitter.DDP)",
        x ="Status", y = "Hz")



#boxplot
pds <- select(pd, MDVP.Fo.Hz., MDVP.Fhi.Hz., MDVP.Flo.Hz., status)
df.m <- melt(pds, id.var = "status")
p <- ggplot(data = df.m, aes(x=variable, y=value)) 
p <- p + geom_boxplot(aes(fill = status))
# if you want color for points replace group with colour=Label
p <- p + geom_point(aes(y=value, group=status), position = position_dodge(width=0.75))
p <- p + facet_wrap( ~ variable, scales="free")
p <- p + xlab("") + ylab("Hz") + ggtitle("Vocal fundamental frequencys")
p <- p + guides(fill=guide_legend(title="Status"))
p 

#facet boxplot
## ----fig.height=4, fig.width=7---------------------------------------
pds <- select(pd, spread1, spread2, PPE, status)
df.m <- melt(pds, id.var = "status")
p <- ggplot(data = df.m, aes(x=variable, y=value)) 
p <- p + geom_boxplot(aes(fill = status))
# if you want color for points replace group with colour=Label
p <- p + geom_point(aes(y=value, group=status), position = position_dodge(width=0.75))
p <- p + facet_wrap( ~ variable, scales="free")
p <- p + xlab("") + ylab("Hz") + ggtitle("Overview important variables")
p <- p + guides(fill=guide_legend(title="Status"))
p 



## --------------------------------------------------------------------
# Filled Density Plot

pd %>% 
  ggplot(aes(spread1, fill=status)) +
  geom_density(alpha=0.2) + ggtitle("Comparation of nonlinear measures of fundamental frequency variation(spread1)")




## --------------------------------------------------------------------
#GG plotting
pd %>% ggplot(aes(MDVP.Jitter.Abs., Jitter.DDP, label = status, color=status)) + geom_label() + ggtitle("Relationship of Jitter.DDP and MDVP.jitter.Abs")

#correlations og the two variables
## --------------------------------------------------------------------
cor(pd$MDVP.Jitter.Abs.,pd$Jitter.DDP)


## --------------------------------------------------------------------
cor(pdi$MDVP.Jitter.Abs.,pdi$Jitter.DDP)


## --------------------------------------------------------------------
cor(hth$MDVP.Jitter.Abs.,hth$Jitter.DDP)

#t-test of spread 1
## ----echo=FALSE------------------------------------------------------
shapiro.test(pd$spread1)


## --------------------------------------------------------------------
res <- t.test(spread1 ~ status, data = pd, var.equal = TRUE)
res

#annexes
## --------------------------------------------------------------------
str(pdi)


## --------------------------------------------------------------------
summarise(pdi)


## --------------------------------------------------------------------
str(hth)


## --------------------------------------------------------------------
summarise(hth)


## --------------------------------------------------------------------
knitr::purl("parkinsons1.Rmd", documentation = 2)


