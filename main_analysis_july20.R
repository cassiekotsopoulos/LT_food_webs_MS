##Main analysis for lake trout intraspecific variation manuscript
##By: Cassandra Kotsopoulos
##Date: July 20/ 2025


#set wd
setwd("~/Desktop/BSM")

#load packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(car)
library(mgcv)
library(visreg)


#load in the individual variation analysis data
reg_df<- read.csv("bsm_analysis_datJune24.csv")
#28 lakes total

# Get a vector of unique lake names
lake_names <- reg_df %>%
  distinct(Lake_Name) %>%
  pull(Lake_Name)

# Print the list of lake names
print(lake_names)


###run GAMMs

####dTP and dNC

#dTP 
#GAMM
gamm1 <- gam(
  log_dY_range ~ summer_air_temp + log_area_ha + log_LaTro_CUE + s(sampling_year, bs = "re"),
  data = reg_df,
  method = "REML"
)

summary(gamm1)


#Plot

#Area
dy_area <- ggplot() +
  geom_ribbon(data = dy_area_data$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr), fill = "grey", alpha = 0.5) +
  geom_line(data = dy_area_data$fit, aes(x = log_area_ha, y = visregFit), color = "red", linetype = "dashed", size = 1) +
  geom_point(data = dy_area_data$res, aes(x = log_area_ha, y = visregRes), size = 2) +
  theme_classic() +
  labs(x = "Lake Area (ln(ha))", y = "dTP (ln)") +
  theme(
    axis.text = element_text(size = 14),   # bigger numbers on axes
    axis.title = element_text(size = 16)   # bigger axis labels
  )
dy_area

#Temp
dy_temp <- ggplot(temp_points, aes(x = summer_air_temp, y = visregRes)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature (C)", y = "dTP (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )
dy_temp

#LT CPUE
dy_lt <- ggplot(lt_points, aes(x = log_LaTro_CUE, y = visregRes)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (ln)", y = "dTP (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )
dy_lt


#arrange 
library(ggpubr)

gamm1_plot <- ggarrange(dy_area, dy_temp, dy_lt, 
                        nrow = 1, ncol = 3)

gamm1_plot




###dNC 
gamm2 <- gam(
  log_dX_range ~ summer_air_temp + log_area_ha + log_LaTro_CUE + s(sampling_year, bs = "re"),
  data = reg_df,
  method = "REML"
)

summary(gamm2)



#Plots
# area
dx_data <- visreg(gamm2, "log_area_ha", partial = TRUE)

dx_area <- ggplot() +
  geom_ribbon(data = dx_data$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr), fill = "grey", alpha = 0.5) +
  geom_line(data = dx_data$fit, aes(x = log_area_ha, y = visregFit), color = "red", size = 1) +
  geom_point(data = dx_data$res, aes(x = log_area_ha, y = visregRes), size = 2) +
  theme_classic() +
  labs(x = "Lake Area (ln(ha))", y = "dNC (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )


# temp
dx_data <- visreg(gamm2, "summer_air_temp", partial = TRUE)

dx_temp <- ggplot() +
  geom_ribbon(data = dx_data$fit, aes(x = summer_air_temp, ymin = visregLwr, ymax = visregUpr),
              fill = "grey", alpha = 0.5) +
  geom_line(data = dx_data$fit, aes(x = summer_air_temp, y = visregFit),
            color = "red", size = 1) +
  geom_point(data = dx_data$res, aes(x = summer_air_temp, y = visregRes), size = 2) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature (C)", y = "dNC (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )


# lake trout CPUE
vis_lt <- visreg(gamm2, "log_LaTro_CUE", partial = TRUE)
lt_points <- vis_lt$res

dx_lt <- ggplot(lt_points, aes(x = log_LaTro_CUE, y = visregRes)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (ln)", y = "dNC (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )





#arrange 
gamm2_plot <- ggarrange(dx_area, dx_temp, dx_lt, 
                        nrow = 1, ncol = 3)

gamm2_plot



##combine these two 
final_6panel <- ggarrange(
  gamm1_plot,  # top row
  gamm2_plot,  # bottom row
  ncol = 1, nrow = 2,
  heights = c(1, 1) 
)

# Display it
final_6panel





###SDNND and SEAc

#SEAc
gamm3 <- gam(
  log_SEAc ~ summer_air_temp + log_area_ha + log_LaTro_CUE + s(sampling_year, bs = "re"),
  data = reg_df,
  method = "REML"
)

summary(gamm3)


##Plot

# sumemr air temp
seac_temp_data <- visreg(gamm3, "summer_air_temp", partial = TRUE)
seac_temp <- ggplot() +
  geom_ribbon(data = seac_temp_data$fit, aes(x = summer_air_temp, ymin = visregLwr, ymax = visregUpr),
              fill = "grey", alpha = 0.5) +
  geom_line(data = seac_temp_data$fit, aes(x = summer_air_temp, y = visregFit),
            color = "red", linetype = "dashed", size = 1) +
  geom_point(data = seac_temp_data$res, aes(x = summer_air_temp, y = visregRes), size = 2) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature (C)", y = "SEAc (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

# lake area
seac_area_data <- visreg(gamm3, "log_area_ha", partial = TRUE)
seac_area <- ggplot() +
  geom_ribbon(data = seac_area_data$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr),
              fill = "grey", alpha = 0.5) +
  geom_line(data = seac_area_data$fit, aes(x = log_area_ha, y = visregFit),
            color = "red", size = 1) +
  geom_point(data = seac_area_data$res, aes(x = log_area_ha, y = visregRes), size = 2) +
  theme_classic() +
  labs(x = "Lake Area (ln(ha))", y = "SEAc (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

#Lake trout CPUE
seac_lt_data <- visreg(gamm3, "log_LaTro_CUE", partial = TRUE)
seac_lt <- ggplot(seac_lt_data$res, aes(x = log_LaTro_CUE, y = visregRes)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (ln)", y = "SEAc (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )



#arrange 
gamm3_plot <- ggarrange(seac_area, seac_temp, seac_lt, 
                        nrow = 1, ncol = 3)

gamm3_plot


#SDNND
gamm4 <- gam(
  log_SDNND ~ summer_air_temp + log_area_ha + log_LaTro_CUE + s(sampling_year, bs = "re"),
  data = reg_df,
  method = "REML"
)

summary(gamm4)


###Plot
#Summer air temp
sdnnd_temp_data <- visreg(gamm4, "summer_air_temp", partial = TRUE)
sdnnd_temp <- ggplot(sdnnd_temp_data$res, aes(x = summer_air_temp, y = visregRes)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature (C)", y = "SDNND (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

#Lake area
sdnnd_area_data <- visreg(gamm4, "log_area_ha", partial = TRUE)
sdnnd_area <- ggplot(sdnnd_area_data$res, aes(x = log_area_ha, y = visregRes)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Lake Area (ln(ha))", y = "SDNND (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

#Lake trout CPUE
sdnnd_lt_data <- visreg(gamm4, "log_LaTro_CUE", partial = TRUE)
sdnnd_lt <- ggplot() +
  geom_ribbon(data = sdnnd_lt_data$fit, aes(x = log_LaTro_CUE, ymin = visregLwr, ymax = visregUpr),
              fill = "grey", alpha = 0.5) +
  geom_line(data = sdnnd_lt_data$fit, aes(x = log_LaTro_CUE, y = visregFit),
            color = "red", linetype = "dashed", size = 1) +
  geom_point(data = sdnnd_lt_data$res, aes(x = log_LaTro_CUE, y = visregRes), size = 2) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (ln)", y = "SDNND (ln)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )


#arrange 
gamm4_plot <- ggarrange(sdnnd_area, sdnnd_temp, sdnnd_lt, 
                        nrow = 1, ncol = 3)

gamm4_plot

##combine these two 
final_6panel <- ggarrange(
  gamm3_plot,  # top row
  gamm4_plot,  # bottom row
  ncol = 1, nrow = 2,
  heights = c(1, 1)  # optional: balance row height
)

# Display it
final_6panel






#Mean Analysis
#load in the data for mean analysis

reg_df<- read.csv("bsm_analysis_meandatJune24.csv")


#logit transform the LC
library(boot)

reg_df <- reg_df %>%
  mutate(logit_lc_pred = logit(mean_LC_pred))

###run regressions 


####Mean TP and LC
#TP  

gamm1 <- gam(
  mean_TP_pred ~ summer_air_temp + log_area_ha + log_LaTro_CUE + s(sampling_year, bs = "re"),
  data = reg_df,
  method = "REML"
)

summary(gamm1)


#Plot


# Summer air temp 
dy_temp_data <- visreg(gamm1, "summer_air_temp", partial = TRUE)
dy_temp <- ggplot() +
  geom_ribbon(data = dy_temp_data$fit, aes(x = summer_air_temp, ymin = visregLwr, ymax = visregUpr),
              fill = "grey", alpha = 0.5) +
  geom_line(data = dy_temp_data$fit, aes(x = summer_air_temp, y = visregFit),
            color = "red", linetype = "dashed", size = 1) +
  geom_point(data = dy_temp_data$res, aes(x = summer_air_temp, y = visregRes), size = 2) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature (C)", y = "Mean TP") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

# Lake area 
dy_area_data <- visreg(gamm1, "log_area_ha", partial = TRUE)
dy_area <- ggplot(dy_area_data$res, aes(x = log_area_ha, y = visregRes)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Lake Area (ln(ha))", y = "Mean TP") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

# Lake trout CPUE 
dy_lt_data <- visreg(gamm1, "log_LaTro_CUE", partial = TRUE)
dy_lt <- ggplot(dy_lt_data$res, aes(x = log_LaTro_CUE, y = visregRes)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (ln)", y = "Mean TP") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )





#arrange 
library(ggpubr)

gamm1_plot <- ggarrange(dy_area, dy_temp, dy_lt, 
                        nrow = 1, ncol = 3)

gamm1_plot


###LC Mean
gamm2 <- gam(
  logit_lc_pred ~ summer_air_temp + log_area_ha + log_LaTro_CUE + s(sampling_year, bs = "re"),
  data = reg_df,
  method = "REML"
)

summary(gamm2)

#Plot

# Summer air temp
dx_temp_data <- visreg(gamm2, "summer_air_temp", partial = TRUE)
dx_temp <- ggplot() +
  geom_ribbon(data = dx_temp_data$fit, aes(x = summer_air_temp, ymin = visregLwr, ymax = visregUpr),
              fill = "grey", alpha = 0.5) +
  geom_line(data = dx_temp_data$fit, aes(x = summer_air_temp, y = visregFit),
            color = "red", size = 1) +
  geom_point(data = dx_temp_data$res, aes(x = summer_air_temp, y = visregRes), size = 2) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature (C)", y = "Mean PNC (logit)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

#Lake area
dx_area_data <- visreg(gamm2, "log_area_ha", partial = TRUE)
dx_area <- ggplot() +
  geom_ribbon(data = dx_area_data$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr),
              fill = "grey", alpha = 0.5) +
  geom_line(data = dx_area_data$fit, aes(x = log_area_ha, y = visregFit),
            color = "red", size = 1) +
  geom_point(data = dx_area_data$res, aes(x = log_area_ha, y = visregRes), size = 2) +
  theme_classic() +
  labs(x = "Lake Area (ln(ha))", y = "Mean PNC (logit)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

#Lake trout CPUE
dx_lt_data <- visreg(gamm2, "log_LaTro_CUE", partial = TRUE)
dx_lt <- ggplot(dx_lt_data$res, aes(x = log_LaTro_CUE, y = visregRes)) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (ln)", y = "Mean PNC (logit)") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )



#arrange 
gamm2_plot <- ggarrange(dx_area, dx_temp, dx_lt, 
                        nrow = 1, ncol = 3)

gamm2_plot



##combine these two 
final_6panel <- ggarrange(
  gamm1_plot,  # top row
  gamm2_plot,  # bottom row
  ncol = 1, nrow = 2,
  heights = c(1, 1)  # optional: balance row height
)

# Display it
final_6panel




