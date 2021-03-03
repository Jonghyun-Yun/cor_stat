library('magrittr')
library(dplyr)
dpro <- readr::read_csv(file = "data/FDOT_10mi_Coordinates.csv", col_names = T)
names(dpro) <- make.names(names(dpro))
dpro$railcross[is.na(dpro$railcross)] <- 0
dpro$bridge[is.na(dpro$bridge)] <- 0
dpro <- na.omit(dpro)

## dd <- dpro %>% select(-c(roadway, start_yr,  end_yr)) %>%
##   mutate(mov_unemp = 0.6*unemp_2019 + 0.3*unemp_2018 + 0.1*unemp_2017 ) %>%
##   mutate(mov_avg_temp = 0.6*avg_temp_2019 + 0.3*avg_temp_2018 + 0.1*avg_temp_2017 ) %>%
##   mutate(mov_max_temp = 0.6*max_temp_2019 + 0.3*max_temp_2018 + 0.1*max_temp_2017 ) %>%
##   mutate(mov_prec = 0.6*prec_2019 + 0.3*prec_2018 + 0.1*prec_2017 ) %>%
##   mutate(mov_gdp = 0.6*gdp_2018 + 0.3*gdp_2017 + 0.1*gdp_2016 )

coef <- exp(c(0,  -1/2,  -1))
coef <- coef / sum(coef)

dd <- dpro %>%
  mutate(unemp = coef[1] * get(paste0("unemp_", start_yr)) +
coef[2] * get(paste0("unemp_", start_yr - 1)) +
coef[3] * get(paste0("unemp_", start_yr - 2))) %>%
    mutate(avg_temp = coef[1] * get(paste0("avg_temp_", start_yr)) +
coef[2] * get(paste0("avg_temp_", start_yr - 1)) +
coef[3] * get(paste0("avg_temp_", start_yr - 2))) %>%
  mutate(max_temp = coef[1] * get(paste0("max_temp_", start_yr)) +
coef[2] * get(paste0("max_temp_", start_yr - 1)) +
coef[3] * get(paste0("max_temp_", start_yr - 2))) %>%
  mutate(prec = coef[1] * get(paste0("prec_", start_yr)) +
coef[2] * get(paste0("prec_", start_yr - 1)) +
coef[3] * get(paste0("prec_", start_yr - 2))) %>%
  mutate(gdp = coef[1] * get(paste0("gdp_", start_yr)) +
coef[2] * get(paste0("gdp_", start_yr - 1)) +
coef[3] * get(paste0("gdp_", start_yr - 2))) %>%
select(-c(roadway, start_yr,  end_yr))

cnames = names(dd)

no_drop <- !grepl("gdp_.", cnames) &
 !grepl("prec_.", cnames) &
 !grepl("max_temp_.", cnames) &
 !grepl("avg_temp_.", cnames) &
 !grepl("unemp_.", cnames)

## df = dd[no_drop] %>% filter(pjt_type=="X3-Resurfacing")
df <- dd[no_drop]
df$logy <- log(df$modified_amounts) - log(df$orig_amounts)

df$pjt_type <- as.factor(df$pjt_type)
df$road_side <- as.factor(df$road_side)
df$funclass <- as.factor(df$funclass)
df$access_con <- as.factor(df$access_con)

cat_df <- df %>% select(pjt_type, road_side, funclass, access_con)

df <- df %>%
  mutate(ncat_pjt_type = as.numeric(pjt_type)) %>%
  mutate(ncat_road_side = as.numeric(road_side)) %>%
  mutate(ncat_funclass = as.numeric(funclass)) %>%
  mutate(ncat_access_con = as.numeric(access_con)) %>%
  select(-c(pjt_type, road_side, funclass, access_con))

dx = df %>% select(-c(pjt_id, x, y, cor, sor, modified_days, actual_days, modified_amounts, logy, actual_amounts))
dxx = dx %>% select(-c(ncat_pjt_type, ncat_road_side, ncat_funclass, ncat_access_con)) %>%
  cbind(cat_df)

X = model.matrix(~ 0 + ., dxx)
vn <- colnames(X) <- colnames(X) %>% make.names()
