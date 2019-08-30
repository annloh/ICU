# MTMM reliabilty and consistency

# This file computes reliability and consistency coefficients based on
# Ding, C. G., & Jane, T.-D. (2012).
# On the reliability, consistency, and method-specificity based on the CT-C(M-1) model.
# Behavior Research Methods, 44(2), 546-557. https://doi.org/10.3758/s13428-011-0169-6

#4F MTMM Model
#specifying a 4-factor MTMM model
MTMM_4F <- "Callous =~  trait02 + trait05 + trait09 + trait13 + trait16 + trait18
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
MTMM_4F_res <- cfa(MTMM_4F,
                      data = newdata,
                      missing = "listwise",
                      estimator = "wlsmv",
                      std.lv = TRUE,
                      se = "robust",
                      test = "Satorra.Bentler",
                      ordered = items)

#model summary 4-factor MTMM model
summary(MTMM_4F_res,
        standardized = TRUE,
        fit.measures = TRUE,
        rsq = FALSE)

# all the magic numbers below stem from the corresponding coefficients of the above model

  callous_rel <- (
                  6^2 * mean(0.061, 0.605, 0.402, 0.100, 0.758, 0.379)^2 + #trait variance
                    6^2 *
                    (
                      (0.5 * mean(0.328, 0.513, 0.317))^2 + (0.5* mean(0.208, 0.350, 0.368))^2 +
                       0.5 * mean(0.328, 0.513, 0.317) * 0.5 * mean(0.208, 0.350, 0.368) * -0.233
                      ) #method variance
                  )/
                   (
                  6^2 * mean(0.061, 0.605, 0.402, 0.100, 0.758, 0.379)^2 + #trait variance
                    6^2 *
                    (
                      (0.5*mean(0.328, 0.513, 0.317))^2 + (0.5* mean(0.208, 0.350, 0.368))^2 +
                       0.5*mean(0.328, 0.513, 0.317) * 0.5* mean(0.208, 0.350, 0.368) * -0.233
                      )  + #method variance
                    6 * mean(0.888, 0.591, 0.575, 0.868, 0.291, 0.756)
                  ) #error variance

  callous_cons <- (6^2 * mean(0.061, 0.605, 0.402, 0.100, 0.758, 0.379)^2
                      )/
                   (
                  6^2 * mean(0.061, 0.605, 0.402, 0.100, 0.758, 0.379)^2 + #trait variance
                    6^2 *
                    (
                      (0.5 * mean(0.328, 0.513, 0.317))^2 + (0.5 * mean(0.208, 0.350, 0.368))^2 +
                       (0.5 * mean(0.328, 0.513, 0.317) * 0.5 * mean(0.208, 0.350, 0.368) * -0.233)
                      )  + #method variance
                    6 * mean(0.888, 0.591, 0.575, 0.868, 0.291, 0.756)
                  ) #error variance

  careless_rel <- (
                    6^2 * mean(0.677, 0.536, 0.601, 0.668, 0.377, 0.609)^2 +
                  6^2 * (
                    (0.5 * mean(0.200, 0.476, 0.522))^2 + (0.5 * mean(0.357, 0.356, 0.329))^2 +
                    0.5 * mean(0.200, 0.476, 0.522) * 0.5 * mean(0.357, 0.356, 0.329) * -0.233
                    )
                    )/(
                  6^2 * mean(0.677, 0.536, 0.601, 0.668, 0.377, 0.609)^2 +
                  6^2 * (
                    (0.5 * mean(0.200, 0.476, 0.522))^2 + (0.5 * mean(0.357, 0.356, 0.329))^2 +
                    0.5 * mean(0.200, 0.476, 0.522) * 0.5 * mean(0.357, 0.356, 0.329) * -0.233
                    ) +
                  6 * mean(0.502, 0.586, 0.512, 0.328, 0.750, 0.357))

  careless_cons <- (
                    6^2 * mean(0.677, 0.536, 0.601, 0.668, 0.377, 0.609)^2
                    )/(
                  6^2 * mean(0.677, 0.536, 0.601, 0.668, 0.377, 0.609)^2 +
                  6^2 * (
                    (0.5 * mean(0.200, 0.476, 0.522))^2 + (0.5 * mean(0.357, 0.356, 0.329))^2 +
                    0.5 * mean(0.200, 0.476, 0.522) * 0.5 * mean(0.357, 0.356, 0.329) * -0.233
                    ) +
                  6 * mean(0.502, 0.586, 0.512, 0.328, 0.750, 0.357))



uncaring_rel <- (
                      6^2 * mean(0.611, 0.683, 0.451, 0.746, 0.756, 0.430)^2 +
                      6^2 * (
                        (0.5 * mean(0.245, 0.401, 0.547))^2 + (0.5 * mean(0.309, 0.500, 0.238))^2 +
                        0.5 * mean(0.245, 0.401, 0.547) * 0.5 * mean(0.309, 0.500, 0.238) * -0.233
                        )
                      )/
                  (
                    6^2 * mean(0.611, 0.683, 0.451, 0.746, 0.756, 0.430)^2 +
                  6^2*(
                    (0.5 * mean(0.245, 0.401, 0.547))^2 + (0.5 * mean(0.309, 0.500, 0.238))^2 +
                    0.5 * mean(0.245, 0.401, 0.547) * 0.5 * mean(0.309, 0.500, 0.238) * -0.233) +
                  6 * mean(0.531, 0.473, 0.547, 0.283, 0.371, 0.516)
                  )

    uncaring_cons <- (
                      6^2 * mean(0.611, 0.683, 0.451, 0.746, 0.756, 0.430)^2
                      )/
                  (
                    6^2 * mean(0.611, 0.683, 0.451, 0.746, 0.756, 0.430)^2 +
                  6^2*(
                    (0.5 * mean(0.245, 0.401, 0.547))^2 + (0.5 * mean(0.309, 0.500, 0.238))^2 +
                    0.5 * mean(0.245, 0.401, 0.547) * 0.5 * mean(0.309, 0.500, 0.238) * -0.233) +
                  6 * mean(0.531, 0.473, 0.547, 0.283, 0.371, 0.516)
                  )

    unemotional_rel <- (
                        6^2 * mean(0.801, 0.730, 0.160, 0.519, 0.366, 0.606)^2 +
                        6^2 * (
                          (0.5 * mean(0.160, 0.317, 0.364))^2 + (0.5 * mean(0.433, 0.410, 0.460))^2 +
                          0.5 * mean(0.160, 0.317, 0.364) * 0.5 * mean(0.433, 0.410, 0.460) * -0.233
                          )
                        )/
                        (
                        6^2 * mean(0.801, 0.730, 0.160, 0.519, 0.366, 0.606)^2 +
                        6^2 * (
                              (0.5 * mean(0.160, 0.317, 0.364))^2 + (0.5 * mean(0.433, 0.410, 0.460))^2 +
                              0.5 * mean(0.160, 0.317, 0.364) * 0.5 * mean(0.433, 0.410, 0.460) * -0.233
                              ) +
                        6 * mean(0.333, 0.279, 0.807, 0.631, 0.733, 0.421)
                        )



    unemotional_cons <- (
                        6^2 * mean(0.801, 0.730, 0.160, 0.519, 0.366, 0.606)^2
                          )/
                        (
                        6^2 * mean(0.801, 0.730, 0.160, 0.519, 0.366, 0.606)^2 +
                        6^2 * (
                              (0.5 * mean(0.160, 0.317, 0.364))^2 + (0.5 * mean(0.433, 0.410, 0.460))^2 +
                              0.5 * mean(0.160, 0.317, 0.364) * 0.5 * mean(0.433, 0.410, 0.460) * -0.233
                              ) +
                        6 * mean(0.333, 0.279, 0.807, 0.631, 0.733, 0.421)
                        )


#3FMTMM

# specifying the Kimonis et al 2008 3-factor MTMM model
MTMM_3F_kim  <-"Callous =~  trait04 + trait07 + trait08  + trait09 + trait11
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

MTMM_3F_kim_res <- cfa(MTMM_3F_kim,
                          data = newdata,
                          estimator = "wlsmv",
                          std.lv = TRUE,
                          se = "robust",
                          test = "Satorra.Bentler",
                          ordered = items
                        )

 summary(MTMM_3F_kim_res,
        standardized = TRUE,
        fit.measures = TRUE,
        rsq = FALSE
      )


#all the following "magic numbers" stem from the corresponding coefficients in the above model

callous3F_rel <- (
                  9^2 * mean(0.530, 0.470, 0.444, 0.339, 0.497, 0.043, 0.308, 0.311, 0.607)^2 +
                  9^2 * (
                    (1/9 * mean(0.648))^2 + (8/9 * mean(0.442, 0.340, 0.422, 0.380, 0.839, 0.298, 0.564))^2 +
                    1/9 * mean(0.648)* 8/9 * mean(0.442, 0.340, 0.422, 0.380, 0.839, 0.298, 0.564) * 0.421)
                      )/
                      (
                      9^2 * mean(0.530, 0.470, 0.444, 0.339, 0.497, 0.043, 0.308, 0.311, 0.607)^2 +
                      9^2 * (
                        (1/9 * mean(0.648))^2 + (8/9 * mean(0.442, 0.340, 0.422, 0.380, 0.839,0.340,  0.298, 0.564))^2 +
                          1/9 * mean(0.648)* 8/9 * mean(0.442, 0.340, 0.422, 0.380, 0.839, 0.340, 0.298, 0.564) * 0.421) +
                        9 * mean(0.524, 0.663, 0.383, 0.707, 0.608, 0.295, 0.790, 0.814, 0.314)
                      )

        callous3F_cons <- (
                      9^2 * mean(0.530, 0.470, 0.444, 0.339, 0.497, 0.043, 0.308, 0.311, 0.607)^2
                      )/
                      (
                      9^2 * mean(0.530, 0.470, 0.444, 0.339, 0.497, 0.043, 0.308, 0.311, 0.607)^2 +
                      9^2 * (
                        (1/9 * mean(0.648))^2 + (8/9 * mean(0.442, 0.340, 0.422, 0.380, 0.839, 0.340, 0.298, 0.564))^2 +
                          1/9 * mean(0.648)* 8/9 * mean(0.442, 0.340, 0.422, 0.380, 0.839, 0.340, 0.298, 0.564) * 0.421) +
                        9 * mean(0.524, 0.663, 0.383, 0.707, 0.608, 0.295, 0.790, 0.814, 0.314)
                      )

    uncaring3F_rel <- (
                      8^2 * mean(0.604, 0.371, 0.115, 0.794, 0.512, 0.501, 0.771, 0.435)^2 +
                      8^2 * mean(0.235, 0.497, 0.231, 0.196, 0.638, 0.672, 0.184, 0.429)^2
                      )/
                      (
                      8^2 * mean(0.604, 0.371, 0.115, 0.794, 0.512, 0.501, 0.771, 0.435)^2 +
                        8^2 * mean(0.235, 0.497, 0.231, 0.196, 0.638, 0.672, 0.184, 0.429)^2 +
                      8* mean(0.580, 0.615, 0.934, 0.332, 0.330, 0.298, 0.371, 0.627))

        uncaring3F_cons <- (
                      8^2 * mean(0.604, 0.371, 0.115, 0.794, 0.512, 0.501, 0.771, 0.435)^2
                      )/
                      (
                      8^2 * mean(0.604, 0.371, 0.115, 0.794, 0.512, 0.501, 0.771, 0.435)^2 +
                        8^2 * mean(0.235, 0.497, 0.231, 0.196, 0.638, 0.672, 0.184, 0.429)^2 +
                      8* mean(0.580, 0.615, 0.934, 0.332, 0.330, 0.298, 0.371, 0.627))

    unemotional3F_rel <- (
                          5^2* mean(0.659, 0.734, 0.351, 0.219, 0.576)^2  +
                            5^2 *(
                            (3/5 * mean(0.450, 0.450, 0.396))^2 + (2/5 * mean(0.484, 0.481))^2 +
                            3/5 * mean(0.450, 0.450, 0.396) * 2/5 * mean(0.484, 0.481) *  0.421)
                            )/
                        (
                          5^2* mean(0.659, 0.734, 0.351, 0.219, 0.576)^2  +
                            5^2 *(
                            (3/5 * mean(0.450, 0.450, 0.396))^2 + (2/5 * mean(0.484, 0.481))^2 +
                            3/5 * mean(0.450, 0.450, 0.396) * 2/5 * mean(0.484, 0.481) *  0.421) +
                            5 * mean(0.363, 0.226, 0.674, 0.796, 0.438 )
                        )


        unemotional3F_cons <- (
                          5^2 * mean(0.659, 0.734, 0.351, 0.219, 0.576)^2
                            )/
                        (
                          5^2* mean(0.659, 0.734, 0.351, 0.219, 0.576)^2  +
                            5^2 *(
                            (3/5 * mean(0.450, 0.450, 0.396))^2 + (2/5 * mean(0.484, 0.481))^2 +
                            3/5 * mean(0.450, 0.450, 0.396) * 2/5 * mean(0.484, 0.481) *  0.421) +
                            5 * mean(0.363, 0.226, 0.674, 0.796, 0.438)
                        )
