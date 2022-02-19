# R script for running BOCPD with ocp on the lg_since_50 data and create posterior plots
library(dplyr)
library(ggplot2)
library(ocp)
library(reshape2)
library(gridExtra)
# load  format, and scale data
lg_since_50 = readr::read_csv("https://raw.githubusercontent.com/ahawl42/changepoints-mlb-offence/main/data/fg_lg_since50.csv")
colnames(lg_since_50)[8:9] = c("K.", "BB")

ocp_data = lg_since_50 %>% 
  mutate(RperPA = R/PA,
         K. = as.numeric(gsub("%", "", K., fixed = TRUE))) %>% 
  select(RperPA, OBP, ISO, K.)
ocp_names = colnames(ocp_data)
ocp_data = scale(ocp_data)
# run change point analysis
ocp = onlineCPD(ocp_data, hazard_func = function(x, lambda)const_hazard(x, lambda=24), 
                getR = TRUE, multivariate = TRUE)
cpts = ocp$changepoint_lists$maxCPs[[1]][-1] #max probability change points
# reshape posterior matrix for plotting
Rt = melt(ocp$R) %>% 
  mutate(value = round(value, 7)) %>% 
  na_if(y=0) %>%
  group_by(Var2) %>%
  mutate(r_max = Var1[which(value == max(value, na.rm = TRUE))]) %>%
  ungroup()
colnames(Rt) = c("rt", "t", "prob", "r_max")

##Plots - These are ggplot recreations of the built-in ocp plots
# Posterior probability plot with change points
post_plot = ggplot(data = Rt, mapping = aes(x=t, y=rt)) + 
  geom_tile(aes(fill = prob), na.rm = TRUE) + 
  scale_fill_gradient(trans = scales::log_trans(), low="white", high="black", 
                      na.value = NA, labels = function(x) signif(x, 2)) +
  geom_point(aes(x=t, y=r_max), color = "red", size = 1) + 
  geom_vline(data=as.data.frame(cpts), aes(xintercept = cpts), linetype = 4, size = 1) +
  labs(title = "Posterior Run Length Probabilities",
       x = "Time",
       y = "Run Length") +
  theme_bw()
# plot of scaled offensive league statistics with changepoints
ocp_data = as.data.frame(ocp_data); colnames(ocp_data) = ocp_names
ocp_data$yr = c(1950:2021)
cpt_plot = ggplot(data = ocp_data, mapping = aes(x=yr, y = RperPA)) + 
  geom_point(aes(color = "#CC79A7")) +
  geom_point(aes(y=ISO, color = "#D55E00")) + 
  geom_point(aes(y=OBP, color = "#009E73")) +
  geom_point(aes(y=K., color = "#0072B2")) +
  geom_vline(data=as.data.frame(cpts), aes(xintercept = cpts+1949), linetype = 4, size = 1) +
  scale_color_manual("", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), 
                     labels = c("K%", "OBP", "R/PA", "ISO")) +
  labs(title = "Maximum Probability Change Points",
       x = "Year",
       y = " ") +
  theme_bw() + 
  theme(legend.key.width = unit(1.22, "cm"))
# stack the plots
grid.arrange(cpt_plot, post_plot, ncol = 1)
cat("Maximum Probability Change Points: ", cpts+1949)
