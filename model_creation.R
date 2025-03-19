library(lubridate)
library(tidyverse)
library(ranger)

# Data Restriction --------------------------------------------------------------
timeModelData <- runnerresults %>% 
  select(-c(fname, lname, division, city, country, country_iso)) %>% 
  remove_missing()

selected <- sample(1:nrow(timeModelData), replace = FALSE, floor(nrow(timeModelData) * .7))
traindata <- #timeModelData[selected, ]
  timeModelData %>% filter(year != 2024)
testdata <- #timeModelData[-selected,  ] # 
  timeModelData %>% filter(year == 2024)


# Finish Time Forests ------------------------------------------------------------------
# Create list of all pairs of splits
split_cols <- names(runnerresults)[16:28]
pairs <- combn(split_cols, 2, simplify = FALSE)

rf_creation <- function(pairs, data) {    # pairs are split combinations and data is training data
  # Select Independent Variables from column 10 (5k) up to current distance
  independent_vars <- c(colnames(traindata)[10:which(colnames(traindata) == pairs[1])], 
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
                mtry = ceiling(n_features / 3)),
         envir = globalenv())
  print(paste0(pairs[1], "to", pairs[2]))
}

# Create all time models
map(pairs, rf_creation, data = traindata)

# Separate models for saving
modelnames <- map(pairs, function(x) print(paste0("rf", x[1], "to", x[2], "time")))
modelnames[!grepl("toFINISHtime", modelnames)] %>% 
  unlist() %>% 
  paste0(collapse = ", ")

save(rffiveKtoFINISHtime, rftenKtoFINISHtime, rffifteenKtoFINISHtime, rftwentyKtoFINISHtime, rfHALFtoFINISHtime, rftwentyfiveKtoFINISHtime, 
     rfthirtyKtoFINISHtime, rftwentyMtoFINISHtime, rftwentyoneMtoFINISHtime, rfthirtyfiveKtoFINISHtime, rffortyKtoFINISHtime, rftwentyfivetwoMtoFINISHtime,
     file = "finish_time_models.RData")

save(rffiveKtotenKtime, rffiveKtofifteenKtime, rffiveKtotwentyKtime, rffiveKtoHALFtime, rffiveKtotwentyfiveKtime, rffiveKtothirtyKtime, 
     rffiveKtotwentyMtime, rffiveKtotwentyoneMtime, rffiveKtothirtyfiveKtime, rffiveKtofortyKtime, rffiveKtotwentyfivetwoMtime, 
     rftenKtofifteenKtime, rftenKtotwentyKtime, rftenKtoHALFtime, rftenKtotwentyfiveKtime, rftenKtothirtyKtime, rftenKtotwentyMtime, 
     rftenKtotwentyoneMtime, rftenKtothirtyfiveKtime, rftenKtofortyKtime, rftenKtotwentyfivetwoMtime, rffifteenKtotwentyKtime, 
     rffifteenKtoHALFtime, rffifteenKtotwentyfiveKtime, rffifteenKtothirtyKtime, rffifteenKtotwentyMtime, rffifteenKtotwentyoneMtime, 
     rffifteenKtothirtyfiveKtime, rffifteenKtofortyKtime, rffifteenKtotwentyfivetwoMtime, rftwentyKtoHALFtime, rftwentyKtotwentyfiveKtime, 
     rftwentyKtothirtyKtime, rftwentyKtotwentyMtime, rftwentyKtotwentyoneMtime, rftwentyKtothirtyfiveKtime, rftwentyKtofortyKtime, 
     rftwentyKtotwentyfivetwoMtime, rfHALFtotwentyfiveKtime, rfHALFtothirtyKtime, rfHALFtotwentyMtime, rfHALFtotwentyoneMtime, 
     rfHALFtothirtyfiveKtime, rfHALFtofortyKtime, rfHALFtotwentyfivetwoMtime, rftwentyfiveKtothirtyKtime, rftwentyfiveKtotwentyMtime, 
     rftwentyfiveKtotwentyoneMtime, rftwentyfiveKtothirtyfiveKtime, rftwentyfiveKtofortyKtime, rftwentyfiveKtotwentyfivetwoMtime, 
     rfthirtyKtotwentyMtime, rfthirtyKtotwentyoneMtime, rfthirtyKtothirtyfiveKtime, rfthirtyKtofortyKtime, rfthirtyKtotwentyfivetwoMtime, 
     rftwentyMtotwentyoneMtime, rftwentyMtothirtyfiveKtime, rftwentyMtofortyKtime, rftwentyMtotwentyfivetwoMtime, rftwentyoneMtothirtyfiveKtime, 
     rftwentyoneMtofortyKtime, rftwentyoneMtotwentyfivetwoMtime, rfthirtyfiveKtofortyKtime, rfthirtyfiveKtotwentyfivetwoMtime, rffortyKtotwentyfivetwoMtime,
     file = "split_time_models.RData")


# Finish or not ----------------------------------------------------------------
rf5kfinisher <- ranger(finisher ~ age + sex + class + fiveK,
                             data = runnerresults %>% 
                               select(finisher, age, sex, class, fiveK) %>% 
                               remove_missing(),
                       probability = TRUE)









importance(rf30ktime)

example <- runnerresults %>% 
  filter(pid == "R234TNAW")

prediction <- predict(m1, 
        newdata = example %>%
          select(age, sex, class, fiveK))

prediction <- predict(rf15ktime,
                      data = example %>% 
                        select(age, sex, class, fiveK, tenK, fifteenK),
                      type = 'quantiles')



prediction$predictions




# Finish or Not --------------------------------------------------------------------

