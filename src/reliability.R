# reliability analysis

# To further test the reliability of the models, coefficient omega
# (McDonald, 1999) was computed for each model with that exhibited at
# least acceptable model fit.

source('data.R')
source('models.R')

# Form the Help file:

# The first and the second coefficients omega will have the same value when the
# model has simple structure, but different values when there are (for example)
# cross-loadings or method factors. The first coefficient omega can be viewed as
# the reliability controlling for the other factors (like ??^2_partial in ANOVA).
# The second coefficient omega can be viewed as the unconditional reliability
# The third coefficient omega (McDonald, 1999), which is sometimes referred to
# hierarchical omega  If the model fits the data well, the third coefficient
# omega will be similar to the omega_2

# In conclusion, omega_1, omega_2, and omega_3 are different in the denominator.
# The denominator of the first formula assumes that a model is congeneric factor
# model where measurement errors are not correlated.
# The second formula accounts for correlated measurement errors.
# However, these two formulas assume that the model-implied covariance matrix
# explains item relationships perfectly. The residuals are subject to sampling error.
# The third formula use observed covariance matrix instead of model-implied covariance
# matrix to calculate the observed total variance.
# This formula is the most conservative method in calculating coefficient omega.


# Omega = Raykov's omega, omega2 = bentler's omega, omega3 = McDonald's omega
# Note that alpha is model based on polychoric correlations and thus will differ
# from traditional alpha (in the above table) computed using Pearson correlations
# source: https://osf.io/r9fwk/ 04_reliability.pdf

#3 factor mtmm kimonis
Mod_3F_MTMM_kim_res

semTools::reliability(Mod_3F_MTMM_kim_res) %>% round(2) #this gives results from manuscript (omega)

round(semTools::reliability(fit_m1),2)

#4 factor mtmm

Mod_4F_MTMM_res

semTools::reliability(Mod_4F_MTMM_res) %>% round(2) #this gives results from manuscript (omega)

#Cronbach's alpha for Essau Subscales
#3 factor subscales
Callous3_items <-   cbind(trait02, trait10, trait04, trait07, trait08, trait09 , trait11 ,
                      trait12 , trait18 , trait20 , trait21)

Uncaring3_items <- cbind(trait03 , trait05 , trait13 , trait15 , trait16 , trait17 , trait23 , trait24)

Unemotional3_items <-  cbind(trait01 , trait06 , trait14 , trait19  , trait22)


psych::alpha(Callous3_items) # callous subscale .72
psych::alpha(Uncaring3_items) #uncaring subscale .79
psych::alpha(Unemotional3_items) #unemotional subscale .71
psych::alpha(ICU_full[,4:27]) #all items .82
