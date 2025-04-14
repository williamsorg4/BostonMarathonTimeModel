library(lubridate)
library(tidyverse)
library(ranger)
library(Metrics)

# Data Restriction --------------------------------------------------------------
timeModelData <- runnerresults %>% 
  select(-c(fname, lname, division, city, country, country_iso)) %>% 
  remove_missing() %>% 
  filter(sex != "X")

selected <- sample(1:nrow(timeModelData), replace = FALSE, floor(nrow(timeModelData) * .7))
traindata <- #timeModelData[selected, ]
  timeModelData %>% filter(year != 2024)
testdata <- #timeModelData[-selected,  ] # 
  timeModelData %>% filter(year == 2024)


dnfModelData <- runnerresults %>% 
  select(sex, age, class, finisher, fiveK, tenK, fifteenK, twentyK, HALF,
         twentyfiveK, thirtyK, twentyM, twentyoneM, thirtyfiveK, fortyK,
         twentyfivetwoM) %>% 
  filter(sex != "X")

# -----------------------------------------------------------------------
# Finish Time Forests ------------------------------------------------------------------
# ---------------------------------------------------------------------
# Create list of all pairs of splits
split_cols <- names(runnerresults)[16:28]
pairs <- combn(split_cols, 2, simplify = FALSE)

rf_creation <- function(pairs, data) {    # pairs are split combinations and data is training data
  # Select Independent Variables from column 10 (5k) up to current distance
  independent_vars <- c(colnames(data)[10:which(colnames(data) == pairs[1])], 
                        "age", "sex", "class")
  n_features <- length(independent_vars)
  
  dependentvariable <- paste0(pairs[1], "to", pairs[2])     # time between pairs that's predicted
  formula <- as.formula(paste(dependentvariable, "~", paste(independent_vars, collapse = " + ")))
  
  
  assign(paste0("rf", pairs[1], "to", pairs[2], "time"),
         ranger(formula,
                data = data,
                importance = 'permutation',
                scale.permutation.importance = TRUE,
                quantreg = TRUE,
                keep.inbag = TRUE,
                mtry = ceiling(n_features / 3),
                num.trees = 500),
         envir = globalenv())
  print(paste0(pairs[1], "to", pairs[2]))
}

map(pairs[12], rf_creation, data = timeModelData)

# Separate models for saving
modelnames <- map(pairs, function(x) print(paste0("rf", x[1], "to", x[2], "time")))
modelnames[grepl("toFINISHtime", modelnames)] %>% 
  unlist() %>% 
  paste0(collapse = ", ")


save(rffiveKtoFINISHtime, rftenKtoFINISHtime, rffifteenKtoFINISHtime, rftwentyKtoFINISHtime, rfHALFtoFINISHtime, rftwentyfiveKtoFINISHtime, 
     rfthirtyKtoFINISHtime, rftwentyMtoFINISHtime, rftwentyoneMtoFINISHtime, rfthirtyfiveKtoFINISHtime, rffortyKtoFINISHtime, rftwentyfivetwoMtoFINISHtime,
     file = "finish_time_models.RData")



save(rffiveKtotenKtime, rffiveKtofifteenKtime, rffiveKtotwentyKtime, 
     rffiveKtoHALFtime, rffiveKtotwentyfiveKtime, rffiveKtothirtyKtime, 
     rffiveKtotwentyMtime, rffiveKtotwentyoneMtime, rffiveKtothirtyfiveKtime, 
     rffiveKtofortyKtime, rffiveKtotwentyfivetwoMtime, rffiveKtoFINISHtime,
     file = "fiveK_models.RData")

save(rftenKtofifteenKtime, rftenKtotwentyKtime, rftenKtoHALFtime, 
     rftenKtotwentyfiveKtime, rftenKtothirtyKtime, rftenKtotwentyMtime, 
     rftenKtotwentyoneMtime, rftenKtothirtyfiveKtime, rftenKtofortyKtime, 
     rftenKtotwentyfivetwoMtime, rftenKtoFINISHtime,
     file = "tenK_models.RData")

save(rffifteenKtotwentyKtime, rffifteenKtoHALFtime, rffifteenKtotwentyfiveKtime, 
     rffifteenKtothirtyKtime, rffifteenKtotwentyMtime, rffifteenKtotwentyoneMtime, 
     rffifteenKtothirtyfiveKtime, rffifteenKtofortyKtime, rffifteenKtotwentyfivetwoMtime, 
     rffifteenKtoFINISHtime,
     file = "fifteenK_models.RData")

save(rftwentyKtoHALFtime, rftwentyKtotwentyfiveKtime, rftwentyKtothirtyKtime, 
     rftwentyKtotwentyMtime, rftwentyKtotwentyoneMtime, rftwentyKtothirtyfiveKtime, 
     rftwentyKtofortyKtime, rftwentyKtotwentyfivetwoMtime, rftwentyKtoFINISHtime,
     file = "twentyK_models.RData")

save(rfHALFtotwentyfiveKtime, rfHALFtothirtyKtime, rfHALFtotwentyMtime, 
     rfHALFtotwentyoneMtime, rfHALFtothirtyfiveKtime, rfHALFtofortyKtime, 
     rfHALFtotwentyfivetwoMtime, rfHALFtoFINISHtime,
     file = "HALF_models.RData")

save(rftwentyfiveKtothirtyKtime, rftwentyfiveKtotwentyMtime, 
     rftwentyfiveKtotwentyoneMtime, rftwentyfiveKtothirtyfiveKtime, 
     rftwentyfiveKtofortyKtime, rftwentyfiveKtotwentyfivetwoMtime, 
     rftwentyfiveKtoFINISHtime,
     file = "twentyfiveK_models.RData")

save(rfthirtyKtotwentyMtime, rfthirtyKtotwentyoneMtime, rfthirtyKtothirtyfiveKtime, 
     rfthirtyKtofortyKtime, rfthirtyKtotwentyfivetwoMtime, rfthirtyKtoFINISHtime,
     file = "thirtyK_models.RData")

save(rftwentyMtotwentyoneMtime, rftwentyMtothirtyfiveKtime, rftwentyMtofortyKtime, 
     rftwentyMtotwentyfivetwoMtime, rftwentyMtoFINISHtime,
     file = "twentyM_models.RData")

save(rftwentyoneMtothirtyfiveKtime, rftwentyoneMtofortyKtime, 
     rftwentyoneMtotwentyfivetwoMtime, rftwentyoneMtoFINISHtime,
     file = "twentyoneM_models.RData")

save(rfthirtyfiveKtofortyKtime, rfthirtyfiveKtotwentyfivetwoMtime, 
     rfthirtyfiveKtoFINISHtime,
     file = "thirtyfiveK_models.RData")

save(rffortyKtotwentyfivetwoMtime, rffortyKtoFINISHtime,
     file = "fortyK_models.RData")

save(rftwentyfivetwoMtoFINISHtime,
     file = "twentyfivetwoM_models.RData")






# --------------------------------------------------------------------
# Goal Predictor Model -----------------------------------------------
# --------------------------------------------------------------------

for (split in split_cols[c(1:7, 10:11)]) {
  assign(paste0("rfFINISHto", split), 
         ranger(get(split) ~ age + sex + class + FINISH, data = timeModelData, num.trees = 200))
  print(paste0("rfFINISHto", split))
}

rfGoalTimeFortyKtoFINISH <- ranger(fortyKtoFINISH ~ ., data = timeModelData[, c(split_cols[c(1:7, 10:11)], "fortyKtoFINISH")], num.trees = 200, mtry = 3)

save(rfFINISHtofiveK, rfFINISHtotenK, rfFINISHtofifteenK, rfFINISHtotwentyK, 
     rfFINISHtoHALF, rfFINISHtotwentyfiveK, rfFINISHtothirtyK,rfFINISHtothirtyfiveK, 
     rfFINISHtofortyK, rfGoalTimeFortyKtoFINISH,
     file = "goal_time_models.RData")




# --------------------------------------------------------------------------
# Finish or Not ----------------------------------------------------------------
# ---------------------------------------------------------------------------
locations <- as.list(colnames(dnfModelData)[5:16])


dnf_model_creation <- function(loc, data) {
  independent_vars <- c(colnames(data)[c(1:3, 5:which(colnames(data) == loc))])
  
  formula <- as.formula(paste("finisher", "~", paste(independent_vars, collapse = " + ")))
  
  assign(paste0("rf", loc, "DNF"),
         ranger(formula,
                data = data,
                na.action = 'na.omit',
                probability = TRUE),
         envir = globalenv())
  print(loc)
}


map(locations, dnf_model_creation, data = dnfModelData)


save(rffiveKDNF, rftenKDNF, rffifteenKDNF, rftwentyKDNF, rfHALFDNF, 
     rftwentyfiveKDNF, rfthirtyKDNF, rftwentyMDNF, rftwentyoneMDNF, 
     rfthirtyfiveKDNF, rffortyKDNF, rftwentyfivetwoMDNF,
     file = 'DNF_models.RData')
