#how the data were compiled

#Imputation

set.seed(427)
#########################################MICE
IMPtrait <- mice(newdata2, m = 1, method = "pmm")
dat_imp <- complete(IMPtrait)
impfinal2 <- cbind(ICU2$ï..lfdnr, dat_imp)

# scales

attach(ICU2)

#4factor subscales
Callous4 <-   trait02 + trait05 + trait09 + trait13 + trait16 + trait18
Careless4 <-   trait03 + trait07 + trait11 +  trait15 + trait20 + trait23
Uncaring4 <-   trait04 +  trait08 + trait12 + trait17 + trait21 + trait24
Unemotional4 <-  trait01 + trait06 +   trait10 +  trait14 + trait19  + trait22


#3 factor subscales
Callous3 <-   trait02 + trait10 + trait04 + trait07 + trait08  + trait09 + trait11 +
              trait12 + trait18 + trait20 + trait21

Uncaring3 <-  trait03 + trait05 + trait13 + trait15 + trait16 + trait17 + trait23 + trait24

Unemotional3 <-  trait01 + trait06 + trait14 + trait19  + trait22

#totalscore
Bi <- trait02 + trait10 + trait04 + trait07 + trait08  + trait09 + trait11 + trait12 +
      trait18 + trait20 + trait21 + trait03 + trait05 + trait13 + trait15 + trait16 +
      trait17 + trait23 + trait24 + trait01 + trait06 + trait14 + trait19  + trait22


#callousness subscale Kimonis
CallousKimm <-   trait04 + trait07 + trait08  + trait09 + trait11 + trait12 + trait18 +
                 trait20 + trait21

#total score Kimmonnis

BiKimm <-   trait04 + trait07 + trait08  + trait09 + trait11 + trait12 + trait18 +
            trait20 + trait21 + trait03 + trait05 + trait13 + trait15 + trait16 +
            trait17 + trait23 + trait24 + trait01 + trait06 + trait14 + trait19  + trait22

impfull <- cbind(ICU2$ï..lfdnr,
                newdata,
                Callous4,
                Careless4,
                Uncaring4,
                Unemotional4,
                Callous3,
                Uncaring3,
                Unemotional3,
                Bi,
                CallousKimm,
                BiKimm
              )

summary(impfull)
write.csv(impfull, file = "impfullfinal.csv")

# loading the additional migration backgroud data
ICU_mig <-read.csv("ICU_with_migration.csv",
                 header = T,
                 sep = ",",
                 dec = ".")

mig_data <- ICU_mig[1:5]

#renaming the old dataframes
ICU_data <- dplyr::rename(ICU_data, lfdnr = ICU2.ï..lfdnr)
mig_data <- dplyr::rename(mig_data, lfdnr = ï..lfdnr)

#merging the migration data to the other data
ICU_full <- dplyr::left_join(ICU_data, mig_data, by = "lfdnr")
