#Descriptives

source('data.R')

names(ICU_full)

# gender descriptives
table(ICU_full$geschl)

# migration background descriptives
migration_table<- table(ICU_full$ethnieend2)
prop.table(migration_table)

#age descritives
ICU_full$alter2 <- recode (ICU_full$alter, '99' = NA_integer_)
summary(ICU_full$alter2)
table(ICU_full$alter)


ICU_data <- ICU2[,-c(1,3)]

# item and scales mean SD male
desM <- describe(ICU_data[which(ICU_data$geschl == "1"), ])
desMmean <- desM$mean
desMsd <- desM$sd
desMn <- desM$n
desMfin <- cbind(desMmean, desMsd, desMn)
View(desMfin)

#item and scales mean SD female
desF <-  describe(ICU_data[which(ICU_data$geschl == "2"), ])
desFmean <- desF$mean
desFsd <- desF$sd
desFn <- desF$n
desFfin <- cbind(desFmean, desFsd, desFn)
View(desFfin)

#item and scales mean SD all
desA <- describe(ICU_data)
desAmean <- desA$mean
desAsd <- desA$sd
desAn <- desA$n
desAfin <- cbind(desAmean, desAsd, desAn)

DES <- cbind(desAfin, desMfin, desFfin)
View(DES)





# Effect sizes gender differences items


#defining empty variables
dfval <- c() #degrees of freedom
pval <- c() # p-values
Tval <- c() #T-values
gval <- c() #g
gvallow <- c() #lower end confidence interval g
gvalup <- c() #upper end confidence interval g

# comparing item and scales values for male and female
for (i in 1:34){
  # t-tests
  a <-  t.test(ICU_data[,(i+3)] ~ ICU_data$geschl)
  Tval[i] <- a$statistic
  dfval[i] <- a$parameter
  pval[i] <- a$p.value

  # effect sizes
  b <- tes(a$statistic, 1886,1992)
  gval[i] <- b$g
  gvallow[i] <- b$l.g
  gvalup[i] <- b$u.g
}


Finaldescriptives <-  cbind(Tval, pval, gval, gvallow, gvalup)
View(Finaldescriptives)
class(Finaldescriptives)

write.csv2(DES, file = "DES.csv")
write.csv2(Finaldescriptives, file = "Finaldescriptives.csv")

#compiling table 1

# combine descriptives and effectsizes
ICU_descriptives <- cbind(DES[(4:37),], Finaldescriptives)
rownames(ICU_descriptives) <- names(ICU_data)[(4:37)]
View(ICU_descriptives)
