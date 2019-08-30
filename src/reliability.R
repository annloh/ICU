# reliability analysis

source('dependencies.R')
source('models.R')

#Cronbach's alpha for Essau Subscales just for comparability in the methods section
#3 factor subscales
Callous3_items <-   cbind(trait02, trait10, trait04, trait07, trait08, trait09 , trait11 ,
                      trait12 , trait18 , trait20 , trait21)

Uncaring3_items <- cbind(trait03 , trait05 , trait13 , trait15 , trait16 , trait17 , trait23 , trait24)

Unemotional3_items <-  cbind(trait01 , trait06 , trait14 , trait19  , trait22)


psych::alpha(Callous3_items) # callous subscale .72
psych::alpha(Uncaring3_items) #uncaring subscale .79
psych::alpha(Unemotional3_items) #unemotional subscale .71
psych::alpha(ICU_full[,4:27]) #all items .82
