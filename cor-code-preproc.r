source('prerequisite.R')
source('custom_function.R')
source('preprocess_rural.R')
## to create urbanization index
y = df$u_r
## y = 1 * (df$logy > 0)

tt = readr::read_csv("data/urban_index.csv")
udex = numeric(nrow(df))
for (dd in 1:nrow(df)) {
  for (cc in 1:6) {
    if (df$Landuse_lv_2[dd] %in% unlist(tt[,cc]))
      udex[dd] = cc}
}

ranger_final = readRDS("cor_ranger_pred_rural.rds")
urban_ranger = ranger_final$predictions[, 2]
y = 1 * (df$logy > 0)
dx[,'ncat_Vendor_Name'] = NULL
dx = cbind(dx, urban_ranger)

source("cor_train_base_prepro.R")
source("cor_train_ranger.R")
