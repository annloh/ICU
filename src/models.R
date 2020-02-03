source('dependencies.R')

################################################################################
## CFA - CONFIRMATORY FACTOR ANALYSES
################################################################################

# This file specifies all CFA models.


#-------------------------------------------------------------------------------
# MODEL 1 CFA 1-Factor Model
#-------------------------------------------------------------------------------

# specifying the one factorial model
one <- "ICU =~ trait02 + trait05 + trait09 + trait13 + trait16 + trait18+
              trait03 + trait07 + trait11 +  trait15 + trait20 + trait23+
              trait04 +  trait08 + trait12 + trait17 + trait21 + trait24+
              trait01 + trait06 +   trait10 +  trait14 + trait19  + trait22
              "

# estimating the model
oneorderd <-cfa(one,
                data = newdata,
                missing = "listwise",
                std.lv = T,
                estimator = "wlsmv",
                se = "robust",
                test = "Satorra.Bentler",
                ordered = items)

# model summary
# summary(oneorderd,
#   standardized = TRUE,
#   fit.measures = TRUE,
#   rsq = FALSE)

################################################################################
################# 3 factor models ##############################################
################################################################################

#-------------------------------------------------------------------------------
# MODEL 2: CFA Essau 3-factor model correlated factors
#-------------------------------------------------------------------------------

Mod_3F_corr_essau <- "Callous =~  trait02 + trait 10 + trait04 + trait07 + trait08  +
                                  trait09 + trait11 + trait12 + trait18 + trait20 + trait21
                      Uncaring =~ trait03 + trait05 + trait13 + trait15 + trait16 +
                                  trait17 + trait23 + trait24
                      Unemotional =~ trait01 + trait06 + trait14 + trait19  + trait22
                      "

Mod_3F_corr_essau_res <-cfa(Mod_3F_corr_essau,
                            data = newdata,
                            missing = "listwise",
                            std.lv = TRUE,
                            estimator = "wlsmv",
                            se = "robust",
                            test = "Satorra.Bentler",
                            ordered = items
                          )


# summary(Mod_3F_corr_essau_res,
#         standardized = TRUE,
#         fit.measures = TRUE,
#         rsq = FALSE
#       )

#-------------------------------------------------------------------------------
# MODEL 3 : Model Kimonis et al 2008 3 correlated factors
#-------------------------------------------------------------------------------

# specifying the Kimonis et al 2008 3-factor model
Mod_3F_corr_kim <- "Callous =~  trait04 + trait07 + trait08  + trait09 + trait11 +
                                trait12 + trait18 + trait20 + trait21
                    Uncaring =~ trait03 + trait05 + trait13 + trait15 + trait16 +
                                trait17 + trait23 + trait24
                    Unemotional =~ trait01 + trait06 + trait14 + trait19  + trait22
                    "
# fitting the Kimonis et al 2008 3-factor model
Mod_3F_corr_kim_res <-cfa(Mod_3F_corr_kim,
                          data = newdata,
                          missing = "listwise",
                          std.lv = TRUE,
                          estimator = "wlsmv",
                          se = "robust",
                          test = "Satorra.Bentler",
                          ordered = items
                        )

# summary of the Kimonis et al 2008 3-factor model
# summary(Mod_3F_corr_kim_res,
#         standardized = TRUE,
#         fit.measures = TRUE,
#         rsq = FALSE)


#-------------------------------------------------------------------------------
# MODEL 4:  CFA Essau 3-factor bifactor model
#-------------------------------------------------------------------------------

Mod_3F_bif_essau <- "Callous =~  trait02 + trait 10 + trait04 + trait07 + trait08 +
                                 trait09 + trait11 + trait12 + trait18 + trait20 + trait21
                    Uncaring =~ trait03 + trait05 + trait13 + trait15 + trait16 +
                                trait17 + trait23 + trait24
                    Unemotional =~ trait01 + trait06 + trait14 + trait19  + trait22

                    Bi =~ trait02 + trait 10 + trait04 + trait07 + trait08 + trait09 +
                          trait11 + trait12 + trait18 + trait20 + trait21+
                          trait03 + trait05 + trait13 + trait15 + trait16 + trait17 +
                          trait23 + trait24 + trait01 + trait06 + trait14 + trait19  +
                          trait22

                    Uncaring ~~ 0*Callous
                    Uncaring ~~ 0*Unemotional
                    Unemotional ~~ 0*Callous

                    Bi ~~ 0*Callous
                    Bi ~~ 0*Uncaring
                    Bi ~~ 0*Unemotional
                    "

Mod_3F_bif_essau_res <-cfa(Mod_3F_bif_essau,
                          data = newdata,
                          missing = "listwise",
                          std.lv = TRUE,
                          estimator = "wlsmv",
                          se = "robust",
                          test = "Satorra.Bentler",
                          ordered = items
                        )

# summary(Mod_3F_bif_essau_res,
#         standardized = TRUE,
#         fit.measures = TRUE,
#         rsq = FALSE
#       )


#-------------------------------------------------------------------------------
# MODEL 5:  Model Kimonis et al 2015 (bifactor)
#-------------------------------------------------------------------------------


# specifying the Kimonis et al 2008 3-factor bifactor model
Mod_3F_bif_kim <- "Callous =~  trait04 + trait07 + trait08  + trait09 + trait11 +
                               trait12 + trait18 + trait20 + trait21
                   Uncaring =~ trait03 + trait05 + trait13 + trait15 + trait16 +
                               trait17 + trait23 + trait24
                   Unemotional =~ trait01 + trait06 + trait14 + trait19  + trait22

                   Bi =~ trait04 + trait07 + trait08  + trait09 + trait11 + trait12 +
                         trait18 + trait20 + trait21 + trait03 + trait05 + trait13 +
                         trait15 + trait16 + trait17 + trait23 + trait24 +
                         trait01 + trait06 + trait14 + trait19  + trait22

                   Uncaring ~~ 0*Callous
                   Uncaring ~~ 0*Unemotional
                   Unemotional ~~ 0*Callous

                   Bi ~~ 0*Callous
                   Bi ~~ 0*Uncaring
                   Bi ~~ 0*Unemotional
                   "

# fitting the Kimonis et al 2008 3-factor bifactor model
Mod_3F_bif_kim_res <-cfa(Mod_3F_bif_kim,
                        data = newdata,
                        missing = "listwise",
                        std.lv = TRUE,
                        estimator = "wlsmv",
                        se = "robust",
                        test = "Satorra.Bentler",
                        ordered = items
                      )

# summary of the Kimonis et al 2008 3-factor bifactor model
# summary(Mod_3F_bif_kim_res,
#         standardized = TRUE,
#         fit.measures = TRUE,
#         rsq = FALSE
#       )


#-------------------------------------------------------------------------------
# MODEL 6: CFA Essau 3-factor MTMM model
#-------------------------------------------------------------------------------


Mod_3F_MTMM_essau <- "Callous =~  trait02 + trait 10 + trait04 + trait07 + trait08  +
                                  trait09 + trait11 + trait12 + trait18 + trait20 + trait21
                      Uncaring =~ trait03 + trait05 + trait13 + trait15 + trait16 +
                                  trait17 + trait23 + trait24
                      Unemotional =~ trait01 + trait06 + trait14 + trait19  + trait22

                      Neg =~ trait01 + trait03 + trait05 + trait08 + trait13 + trait14 +
                             trait15 + trait16 + trait17 + trait19 + trait23 + trait24
                      Pos =~ trait02 + trait04 + trait06 + trait07 + trait09 + trait10 +
                             trait11 + trait12 + trait18 + trait20 +  trait21 + trait22

                      Neg ~~ 0*Callous
                      Neg ~~ 0*Uncaring
                      Neg ~~ 0*Unemotional

                      Pos ~~ 0*Callous
                      Pos ~~ 0*Uncaring
                      Pos ~~ 0*Unemotional

                      Pos ~~ Neg
                      "

Mod_3F_MTMM_essau_res <-cfa(Mod_3F_MTMM_essau,
                            data = newdata,
                            missing = "listwise",
                            std.lv = TRUE,
                            estimator = "wlsmv",
                            se = "robust",
                            test = "Satorra.Bentler",
                            ordered = items
                          )

# summary(Mod_3F_MTMM_essau_res,
#         standardized = TRUE,
#         fit.measures = TRUE,
#         rsq = FALSE
#       )


#-------------------------------------------------------------------------------
# MODEL 7: Model Kimonis et al 2015 (3 factors, MTMM)
#-------------------------------------------------------------------------------

# specifying the Kimonis et al 2008 3-factor MTMM model
Mod_3F_MTMM_kim  <-"Callous =~  trait04 + trait07 + trait08  + trait09 + trait11
                                + trait12 + trait18 + trait20 + trait21
                    Uncaring =~ trait03 + trait05 + trait13 + trait15 + trait16
                                + trait17 + trait23 + trait24
                    Unemotional =~ trait01 + trait06 + trait14 + trait19  + trait22

                    Neg =~ trait01 + trait03 + trait05 + trait08 + trait13 + trait14
                            + trait15 + trait16 + trait17 + trait19 + trait23 + trait24
                    Pos =~ trait04 + trait06 + trait07 + trait09 + trait11 + trait12
                            + trait18 + trait20 +  trait21 + trait22

                    Neg ~~ 0*Callous
                    Neg ~~ 0*Uncaring
                    Neg ~~ 0*Unemotional

                    Pos ~~ 0*Callous
                    Pos ~~ 0*Uncaring
                    Pos ~~ 0*Unemotional
                    Pos ~~ Neg
                    "

Mod_3F_MTMM_kim_res <- cfa(Mod_3F_MTMM_kim,
                          data = newdata,
                          estimator = "wlsmv",
                          se = "robust",
                          test = "Satorra.Bentler",
                          ordered = items
                        )

# summary(Mod_3F_MTMM_kim_res,
#         standardized = TRUE,
#         fit.measures = TRUE,
#         rsq = FALSE
#       )

################################################################################
################# 4 factor models ##############################################
################################################################################


#-------------------------------------------------------------------------------
# MODEL 8: 4-factor correlated
#-------------------------------------------------------------------------------

#specifying a 4-factor model with correlated factors

Mod_4F <- "Callous =~  trait02 + trait05 + trait09 + trait13 + trait16 + trait18
          Careless =~ trait03 + trait07 + trait11 +  trait15 + trait20 + trait23
          Uncaring =~ trait04 +  trait08 + trait12 + trait17 + trait21 + trait24
          Unemotional =~ trait01 + trait06 +   trait10 +  trait14 + trait19  + trait22
          "

# estimating the model
Mod_4F_res <-cfa(Mod_4F,
                  data = newdata,
                  missing = "listwise",
                  std.lv = TRUE,
                  estimator = "wlsmv",
                  se = "robust",
                  test = "Satorra.Bentler",
                  ordered = items
                )

# model summarry
# summary(Mod_4F_res,
#         standardized = TRUE,
#         fit.measures = TRUE,
#         rsq = FALSE
#       )



#-------------------------------------------------------------------------------
# MODEL 9: 4-factor bi-factor #########################################
#-------------------------------------------------------------------------------

#specifying a 4-factor bi-factor model
Mod_4F_bif <- "Callous =~  trait02 + trait05 + trait09 + trait13 + trait16 + trait18
              Careless =~ trait03 + trait07 + trait11 +  trait15 + trait20 + trait23
              Uncaring =~ trait04 +  trait08 + trait12 + trait17 + trait21 + trait24
              Unemotional =~ trait01 + trait06 +  trait10 +  trait14 + trait19  + trait22
              Bi =~ trait02 + trait05 + trait09 + trait13 + trait16 + trait18 +
              trait03 + trait07 + trait11 +  trait15 + trait20 + trait23 +
              trait04 +  trait08 + trait12 + trait17 + trait21 + trait24 +
              trait01 + trait06 +   trait10 +  trait14 + trait19  + trait22

              Uncaring ~~ 0*Callous
              Uncaring ~~ 0*Unemotional
              Uncaring ~~ 0*Careless
              Unemotional ~~ 0*Callous
              Unemotional ~~ 0*Careless
              Callous ~~ 0*Careless

              Bi ~~ 0*Callous
              Bi ~~ 0*Uncaring
              Bi ~~ 0*Unemotional
              Bi ~~ 0*Careless
              "
#estimating the 4-factor bi-factor model
Mod_4F_bif_res <-cfa(Mod_4F_bif,
                     data = newdata,
                     missing = "listwise",
                     std.lv = TRUE,
                     estimator = "wlsmv",
                     se = "robust",
                     test = "Satorra.Bentler",
                     ordered = items
                    )

# ##mmodel summary 4-factor bi-factor model
# summary(Mod_4F_bif_res,
#         standardized = TRUE,
#         fit.measures = TRUE,
#         rsq = FALSE
#       )

#-------------------------------------------------------------------------------
# MODEL 10: 4-factor MTMM model ########################################
#-------------------------------------------------------------------------------

#specifying a 4-factor MTMM model
Mod_4F_MTMM <- "Callous =~  trait02 + trait05 + trait09 + trait13 + trait16 + trait18
               Careless =~ trait03 + trait07 + trait11 +  trait15 + trait20 + trait23
               Uncaring =~ trait04 +  trait08 + trait12 + trait17 + trait21 + trait24
               Unemotional =~ trait01 + trait06 +   trait10 +  trait14 + trait19  + trait22

               Neg =~ trait01 + trait03 + trait05 + trait08 + trait13 + trait14 +
                      trait15 + trait16 + trait17 + trait19 + trait23 + trait24
               Pos =~ trait02 + trait04 + trait06 + trait07 + trait09 + trait10 +
                      trait11 + trait12 + trait18 + trait20 +  trait21 + trait22

               Neg ~~ 0*Callous
               Neg ~~ 0*Careless
               Neg ~~ 0*Uncaring
               Neg ~~ 0*Unemotional

               Pos ~~ 0*Callous
               Pos ~~ 0*Careless
               Pos ~~ 0*Uncaring
               Pos ~~ 0*Unemotional

               Pos ~~ Neg
               "

#estimating the 4-factor MTMM model
Mod_4F_MTMM_res <- cfa(Mod_4F_MTMM,
                      data = newdata,
                      missing = "listwise",
                      estimator = "wlsmv",
                      se = "robust",
                      test = "Satorra.Bentler",
                      ordered = items)

#model summary 4-factor MTMM model
# summary(Mod_4F_MTMM_res,
#         standardized = TRUE,
#         fit.measures = TRUE,
#         rsq = FALSE)
#




#Modellist
CFA_models <- list(mod1 = one, #1: one factor model
            mod2 = Mod_3F_corr_essau, #2: essau 2006 3 facor correlated
            mod3 = Mod_3F_corr_kim, #3: Kimonis 2015 3 factor correlated
            mod4 = Mod_3F_bif_essau, #4: Essau 2006 3 factor bifactor
            mod5 = Mod_3F_bif_kim, #5: Kimonis 3 factor bifactor
            mod6 = Mod_3F_MTMM_essau, #6: essau 3 factor MTMM
            mod7 = Mod_3F_MTMM_kim, #7: Kimonis 3 factor MTMM
            mod8 = Mod_4F, #8: 4 factor correlated
            mod9 = Mod_4F_bif, #9: 4 factor bifactor
            mod10 = Mod_4F_MTMM #10: 4 factor MTMM)
            )

#Compiling  Results of Table 2
fitted_models <- list(mod1 = oneorderd, #1: one factor model
            mod2 = Mod_3F_corr_essau_res, #2: essau 2006 3 facor correlated
            mod3 = Mod_3F_corr_kim_res, #3: Kimonis 2015 3 factor correlated
            mod4 = Mod_3F_bif_essau_res, #4: Essau 2006 3 factor bifactor
            mod5 = Mod_3F_bif_kim_res, #5: Kimonis 3 factor bifactor
            mod6 = Mod_3F_MTMM_essau_res, #6: essau 3 factor MTMM
            mod7 = Mod_3F_MTMM_kim_res, #7: Kimonis 3 factor MTMM
            mod8 = Mod_4F_res, #8: 4 factor correlated
            mod9 = Mod_4F_bif_res, #9: 4 factor bifactor
            mod10 = Mod_4F_MTMM_res #10: 4 factor MTMM)
            )


my_CFA_fitmeasures <- c("chisq.scaled" ,"df","pvalue", "rmsea.robust",
                    "rmsea.ci.lower.robust" , "rmsea.ci.upper.robust",
                    "cfi.robust", "tli.robust")


table2 <- map(fitted_models, function(x) fitmeasures(x, fit.measures = my_CFA_fitmeasures)) %>% #extracting fitmeasures
              { bind_rows(!!! .) } %>% cbind(models = names(fitted_models), .) #turning into nice df




















