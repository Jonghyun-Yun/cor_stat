source("prerequisite.R")
source("custom_function.R")
source("preprocess_rural.R")
## to create urbanization index
y <- df$u_r
## y = 1 * (df$logy > 0)

ranger_final <- readRDS("cor_ranger_pred_rural.rds")
## ranger_final <- readRDS("cor_ranger_rural_ff.rds")
## urban_ranger <- ranger_final$predictions[, 2]
rrdd = model.matrix(~ 0 + ., data = dx)
urban_ranger = predict(ranger_final, rrdd)$predictions[, 2]
## source("cor_train_base_prepro.R")
y <- 1 * (df$logy > 0)
dx[, "ncat_Vendor_Name"] <- NULL
## dx <- cbind(dx, urban_ranger)
dx <- data.frame(dx, u_r = df$u_r)

source("cor_train_base_prepro.R")
source("cor_train_ranger.R")
