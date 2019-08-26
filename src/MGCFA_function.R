# MGCFA Measurement Invariance testing



#-------------------------------------------------------------------------------
# MGCFA group = gender
# model = 4 factor MTMM (all items)
#-------------------------------------------------------------------------------
my_MGCFA <- function(my_model, my_data, my_items, my_group){

# estimating model for configural MI
conf <- cfa(my_model,
             data = my_data,
             group = my_group,
             std.lv = TRUE,
             parameterization = "theta",
             estimator = "wlsmv",
             se = "robust",
             test = "Satorra.Bentler",
             ordered = my_items
            )


# estimating model for weak MI
weak <- cfa(my_model,
              data = my_data,
              group = my_group,
              group.equal = c("loadings"),
              parameterization = "theta",
              estimator = "wlsmv",
              se = "robust",
              test = "Satorra.Bentler",
              ordered = my_items)

# estimating model for strong MI
strong <-cfa(my_model,
              data = my_data,
              group = my_group,
              group.equal = c("loadings", "thresholds"),
              parameterization = "theta",
              estimator = "wlsmv",
              se = "robust",
              test = "Satorra.Bentler",
              ordered = my_items
            )

# estimating model for strict MI
strict <- cfa(my_model,
              data = my_data,
              group = my_group,
              group.equal = c("loadings", "thresholds", "residuals"),
              parameterization = "theta",
              estimator = "wlsmv",
              se = "robust",
              test = "Satorra.Bentler",
              ordered = my_items
            )


# anova_fit <- anova(conf, weak, strong, strict)

# Comparison of model fit for each MI level
compare_fit <- compareFit(strict = strict,
          scalar = strong,
          metric = weak,
          config = conf,
          argsLRT = list(asymptotic = TRUE, indices = T, method = "satorra.bentler.2010")
          )


# (identical solutions to above except fpr the configural invariance)
# measurementInvarianceCat(model = my_model,
#                         group = my_group,
#                         strict = T,
#                         data = my_data,
#                         missing = "listwise",
#                         estimator = "wlsmv",
#                         se = "robust",
#                         test = "Satorra.Bentler",
#                         parameterization = "theta",
#                         ordered = my_items
#                        )

my_fitmeasures <- c("chisq" , "chisq.scaled", "df" , "cfi.robust", "rmsea.robust")

levels <- list(conf = conf, weak = weak, strong = strong, strict = strict)

fit <- map(levels, function(x) fitmeasures(x, fit.measures = my_fitmeasures)) %>% #extracting fitmeasures
              { bind_rows(!!! .) } %>% cbind(models = names(levels), .) %>% #turning them into df
              cbind(delta_CFI = c(NA,diff(.$cfi.robust))) %>% #add diff cfi
              cbind(delta_RMSEA = c(NA,diff(.$rmsea.robust)) ) %>% #add diff rmsea
              cbind(delta_chisq = compare_fit@nested$"Chisq diff" ) %>%  # add chisq diff
              cbind(delta_df = compare_fit@nested$"Df diff" ) %>% #add df diff
              cbind(p = compare_fit@nested$"Pr(>Chisq)" ) #add p

return(list(fit = fit))

}
# MGCFA 4 factor MTMM model group = gender

MGCFA_4F_MTMM_gender <- my_MGCFA(my_model = Mod_4F_MTMM,
                                 my_data = newdata,
                                 my_items = items,
                                 my_group = "geschl")

# MGCFA 3 factor MTMM model kimonis group = gender


MGCFA_3F_MTMM_kim_gender <- my_MGCFA(my_model = Mod_3F_MTMM_kim,
                                      my_data = newdata,
                                      my_items = items,
                                      my_group = "geschl")

# MGCFA 4 factor MTMM model group = migration status

MGCFA_4F_MTMM_migration <- my_MGCFA(my_model = Mod_4F_MTMM,
                                     my_data = ICU_full,
                                     my_items = items,
                                     my_group = "ethnieend2")

write.table(x = round(MGCFA_4F_MTMM_migration$fit[,-1], digits = 3),
            file = "MGCFA_4F_MTMM_migration.csv",
            sep = ";")

# MGCFA 3 factor MTMM model kimonis group = migration status

MGCFA_3F_MTMM_kim_migration <- my_MGCFA(my_model = Mod_3F_MTMM_kim,
                                         my_data = ICU_full,
                                         my_items = items,
                                         my_group = "ethnieend2")

write.table(x = round(MGCFA_3F_MTMM_kim_migration$fit[,-1], digits = 3),
            file = "MGCFA_3F_MTMM_kim_migration.csv",
            sep = ";")
