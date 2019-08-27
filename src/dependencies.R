#reading in the data, some cleaning and loading packages

# install the required packages (uncomment in necessary)
#install.packages("psych")
#install.packages("lavaan")
#install.packages("semTools")
#install.packages("semPlot")
#install.packages("compute.es")
#install.packages("MBESS")
#install.packages("tidyverse")

# loading required packages
library(psych)
library(lavaan)
library(semTools)
library(semPlot)
library(compute.es)
library(MBESS)
library(tidyverse)

# changing expoenential notation
options(scipen = 999)

# loading the data
ICU2 <-read.csv("impfullfinal.csv",
                 header = T,
                 sep = ",",
                 dec = ".")

#For details on how variables were computed see datacompilation.R


#computing a sumscore
ICUtotal <-  ICU2$Callous4 + ICU2$Careless4 + ICU2$Careless4 + ICU2$Unemotional4

#defining a subset of relevant variables
myvars <-c("alter" ,
          "geschl" ,
          "trait01",
          "trait02",
          "trait03",
          "trait04",
          "trait05",
          "trait06",
          "trait07",
          "trait08",
          "trait09",
          "trait10",
          "trait11",
          "trait12",
          "trait13",
          "trait14",
          "trait15",
          "trait16",
          "trait17",
          "trait18",
          "trait19",
          "trait20",
          "trait21",
          "trait22",
          "trait23",
          "trait24"
        )

items <- myvars[3:26]
#subsetting the data
newdata <- ICU2[myvars]




