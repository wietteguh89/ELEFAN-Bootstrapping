######################         ELEFAN Simulated Annealing in TropFishR                ##################
###                                       Wiwiet Teguh Taufani                                       ###
###                   Graduate School of Global Food Resources, Hokkaido University                  ###
### Department of Aquatic Resources, Faculty of Fisheries and Marine Science, Universitas Diponegoro ###
### last updated December 2023


install.packages("TropFishR")
library(TropFishR)

##### DATA PREPARATION #####
setwd("your directory")

Fishes <- read.csv(file = "your_file.csv", header = T)             # select you csv data
dates <- colnames(Fishes) [-1]                                     # to recognising dates data
dates <- strsplit(dates, "X")                                      # to recognising dates data
dates <- unlist(lapply(dates, function(x) x[2]))                   # to recognising dates data
dates <- as.Date(dates, "%Y.%d.%m")                                # to recognising dates data
Fishes_data <- list(dates = dates,
                         midLengths = Fishes$lengthClass,
                         catch = as.matrix(Fishes[,-1]))           # to make length frequency distribution
class(Fishes_data) <- "lfq"
Fishes_data$catch[is.na(Fishes_data$catch)] <- 0
plot(Fishes_data, Fname = "catch")                                 # to see the plot of length frequency distribution


Fishes_data <- lfqRestructure(Fishes_data, MA=5)                   # to restructurising length frequency data
plot(Fishes_data, Fname = "rcounts")                               # to see the restructured length frequency data

##### ELEFAN_SA in TropFishR with Seasonal Oscillation#####
Fishes_SA <- ELEFAN_SA(                                            # function of ELEFAN_SA
  lfq = Fishes_data, 
  seasonalised = TRUE,                                             # indicating the seasonalised of VBGF should be applied
  init_par = list(Linf=185, K=0.7, t_anchor=0.5, ts=0.5, C=0.0),   # list of the initial values for the components to be optimized
  low_par = list(Linf=120, K=0.01, t_anchor=0.0, ts=0.0, C=0.0),   # list of the lower bounds for components
  up_par = list(Linf=250, K=1.5, t_anchor=1.0, ts=1.0, C=0.1),     # list of the upper bounds for components
  SA_temp = 1e5,                                                   # initial value for temperature (default : 1e5)
  SA_time = 60,                                                    # maximum running time in seconds (default : 60*1)
  maxit = NULL,                                                    # maximum number of iterations of the algorithm (default : NULL)
  MA = 5,                                                          # number indicating over how many length classes the moving average should be performed (default : 5)
  plot.score = TRUE,                                               # plot simulated annealing score progression (default : TRUE)
  verbose = FALSE                                                  # means that messages from the algorithm are not shown
)
unlist(Fishes_SA$par)                                              # to see the list of parameters results
Fishes_SA$Rn_max                                                   # to see the Rn score 
plot(Fishes_SA)                                                    # to plot the analysis result

plot(Fishes_data, Fname = "catch")                                 # to see the length frequency plot

Fishes_tmp <- lfqFitCurves(Fishes_data,                            # to combine the length frequency with the curve of growth curve parameters
                           par = list(Linf=Fishes_SA$par$Linf,
                                      K=Fishes_SA$par$K,
                                      t_anchor=0.5), 
                           draw = TRUE, col=4, lty=2)

##### Estimating MOrtality #####
Fishes_mor <- M_empirical(Linf = Fishes_SA$par$Linf,               # to calculate the empirical natural mortality by Pauly, 1979
                          K_l = Fishes_SA$par$K, 
                          tmax = Fisehs_SA$agemax, 
                          temp = 28.5,                             # describing the temperature
                          method = c("Pauly_Linf"))
Fishes_data$M <- as.numeric(Fishes_mor)                            # to show the natural mortality value

# show results
paste("M =", as.numeric(Fishes_mor))                              # to show the natural mortality value
Fishes_mor_Then = 4.118*                                          # to calculate the natural mortality by using Then, 2015 (doi:10.1093/icesjms/fsu136)
  ((Fishes_SA$par$K^0.73)*(Fishes_SA$par$Linf^-0.33))
Fishes_mor_Tanaka = 2.5/11.1                                      # to calculate the natural mortality by using Tanaka, 1960 
                                                                  # Tanaka, S. 1960. Studies on the dynamics and management of fish populations. Bulletin of Tokai Regional Fisheries Research Laboratory 28: 1-200

# estimation t0
Fishes_t0 = -10^(-0.3922-(0.2752*log10(Fishes_SA$par$Linf))-      # to calculate t_0 by using Pauly, 1979
                   (1.038*log10(Fishes_SA$par$K)))
print(Fishes_t0)                                                  # to show t_0 value


## estimation of exploitation level##
#input data#
Fishes_2 <- read.csv(file = "your csv file.csv", header = TRUE)
Fishes <- synLFQ3
Fishes$midLengths <- Fishes_2[,1]
Fishes$midLengths <- as.numeric(Fishes$midLengths)
Fishes$catch <- as.numeric(Fishes$catch)
Fishes$Linf <- Fishes_SA$par$Linf
Fishes$K <- Fishes_SA$par$K
Fishes$t0 <- Fishes_t0
Fishes$catch <- as.matrix(Fishes_2[,-1])
Fishes$catch <- as.numeric(Fishes$catch)

# run catch curve 
Fishes_res_cc <- catchCurve(Fishes, calc_ogive = TRUE, reg_int = NULL)
Fishes_res_cc

# assign estimates to the data list
Fishes$Z <- Fishes_res_cc$Z
Fishes$M <- Fishes_mor_Then                                      # to estimate YPR by using natural mortality from Then (changed as you need)
Fishes$FM <- as.numeric(Fishes$Z - Fishes_mor_Then)
Fishes$E <- Fishes$FM / Fishes$Z

#Display result
Fishes_res <- Fishes_res_cc
Fishes_res$M <- Fishes_mor_Then
Fishes_res$FM <- Fishes$FM
Fishes_res$E <- Fishes$E
Fishes_res$t0 <- Fishes$t0
Fishes_res$midLengths <- NULL; Fishes_res$catch <- NULL; Fishes_res$t_midL <- NULL
Fishes_res$lnC_dt <- NULL; Fishes_res$reg_int <- NULL; Fishes_res$linear_mod <- NULL
Fishes_res$confidenceInt <- NULL; Fishes_res$intercept <- NULL; Fishes_res$linear_mod_sel <- NULL
Fishes_res$Sobs <- NULL; Fishes_res$ln_1_S_1 <- NULL; Fishes_res$Sest <- NULL
unlist(Fishes_res)

## Recruitment pattern ##
# add growth
Fishes_data$Linf <- Fishes_SA$par$Linf
Fishes_data$K <- Fishes_SA$par$K
Fishes_data$t0 <- Fishes$t0
Fishes_data$ts <- Fishes_SA$par$ts

# retrieve sampling times from catch matrix
Fishes_s_dates <- as.POSIXlt(Fishes_data$dates, format="%Y-%d-%m")
Fishes_data$dates

Fishes_rec <- recruitment(param = Fishes_data, tsample = Fishes_s_dates$yday/365, plot = TRUE)

plot(Fishes_rec$per_recruits, type = "l", 
     ylab = "recruitment (%)", 
     xlab = "Month", 
     xaxt = 'n', 
     las = 1)
axis(1, 1:12, Fishes_rec$months_abb)

## Yield per recruit modelling ##

#input parameter#
Fishes$a <- 0.000052                                          # put your own a (intercept) value from length-weight regression
Fishes$b <- 2.78                                              # put your own b (slope) value from length-weight regression
Fishes$mk <- Fishes_res$M/Fishes_res$K
Fishes$Lmat <- (Fishes$b/(Fishes$mk+Fishes$b))*Fishes_res$Linf
Fishes$Wmat <- (Fishes$a*(Fishes$Lmat^Fishes$b))*100  # 100 for gr to kg
Fishes$Lr <- Fishes_res$L50

## list with selectivity parameters
Fishes_selectivity_list <- list(selecType = "trawl_ogive",
                                     L50 = Fishes_res_cc$L50, L75 = Fishes_res_cc$L75)

## Thompson and Bell model with changes in F
Fishes_TB1 <- predict_mod(Fishes, type = "ThompBell",
                               FM_change = seq(0,2.5,0.1),
                               stock_size_1 = 1,
                               curr.E = Fishes$E,
                               curr.Lc = Fishes_res_cc$L50,
                               s_list = Fishes_selectivity_list,
                               plot = TRUE, hide.progressbar = TRUE)

#> [1] Fishing mortality per length class not provided, using selectivity information to derive fishing mortality per length class.

## Thompson and Bell model with changes in F and Lc
Fishes_TB2 <- predict_mod(Fishes, type = "ThompBell",
                               FM_change = seq(0,2.5,0.1),
                               Lc_change = seq(100,200,10),
                               stock_size_1 = 1,
                               curr.E = Fishes$E,
                               curr.Lc = Fishes_res_cc$L50,
                               s_list = Fishes_selectivity_list,
                               plot = TRUE, hide.progressbar = TRUE)

## plot results
par(mfrow = c(1,1), mar = c(4,5,2,4.5), oma = c(1,0,0,0))
plot(Fishes_TB1, mark = TRUE)
mtext("(a)", side = 3, at = -0.1, line = 0.6)
plot(Fishes_TB2, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
mtext("(b)", side = 3, at = -0.1, line = 0.6)

## Biological reference levels
Fishes_TB1$df_Es

## Current yield and biomass levels
Fishes_TB1$currents

mfrow = c(1,1)
