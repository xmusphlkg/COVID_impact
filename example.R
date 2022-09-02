
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(jsonlite)

library(stats)
library(tseries)
library(astsa)
library(forecast)
library(greyforecasting)
library(opera)

# loadfonts("pdf")
library(patchwork)
library(Cairo)

set.seed(202208)

# data load ---------------------------------------------------------------

source('theme_set.R')

datafile_manual <- read.xlsx('./data/df_load_202205.xlsx', sheet = "Sheet 1")
datafile_manual$date <- convertToDate(datafile_manual$date)

datafile_analysis <- datafile_manual %>% 
     filter(religion == '全国' & type == 'inci' & disease_1 != 'remove')

split_date <- as.Date("2019/12/1")
train_length <- 12*10
test_length <- 12*2

disease_list <- c('百日咳', '丙肝', '戊肝', '病毒性肝炎', '布病', '登革热', 
                  '肺结核', '风疹', '急性出血性结膜炎', '甲肝', 
                  '痢疾', '淋病', '流行性出血热', '流行性感冒',
                  '流行性腮腺炎', '麻疹', '梅毒', '疟疾', '其它感染性腹泻病',
                  '伤寒+副伤寒', '乙肝', '手足口病', '猩红热',
                  '乙脑', '包虫病', '斑疹伤寒')
disease_name <- c('Pertussis', 'HCV', 'HEV','Viral hepatitis',
                  'Brucellosis', 'Dengue fever', 'Tuberculosis',
                  'Rubella', 'Acute hemorrhagic conjunctivitis', 'HAV',
                  'Dysentery', 'Gonorrhea', 'HFRS',
                  'Influenza', 'Mumps', 'Measles',
                  'Syphilis', 'Malaria', 'Other infectious diarrhea',
                  'Typhoid fever and paratyphoid fever', 'HBV', 'HFMD',
                  'scarlet fever', 'Japanese encephalitis', 'Hydatidosis', 'Typhus')

# data clean --------------------------------------------------------------

i <- 22

datafile_single <- datafile_analysis %>% 
     filter(disease_1 == disease_list[i]) %>% 
     select(date, disease_1, value) %>% 
     complete(
          date = seq.Date(
               from = min(date),
               to = max(date),
               by = 'month'
          ),
          fill = list(value = 0,
                      disease_1 = disease_list[i])
     )

## simulate date before 2020
df_simu <- datafile_single  %>% 
     arrange(date) %>% 
     unique() %>% 
     filter(date <= split_date)%>% 
     select(value)

ts_obse_1 <- df_simu %>% 
     ts(frequency = 12,
        start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                  as.numeric(format(min(datafile_single$date), "%m"))))

ts_train_1 <- head(ts_obse_1, )

## simulate date after 2020
df_simu <- datafile_single  %>% 
     arrange(date) %>% 
     unique() %>%
     # filter(date >= split_date)%>% 
     select(value)

ts_simu_2 <- df_simu %>% 
     # ts(frequency = 12,
     #    start = c(2019, 12))
     ts(frequency = 12,
        start = c(as.numeric(format(min(datafile_single$date), "%Y")),
                  as.numeric(format(min(datafile_single$date), "%m"))))
max_date <- max(datafile_single$date)

# ARIMA -------------------------------------------------------------------

mod <- auto.arima(ts_simu_1, seasonal = T)
mod_2 <- auto.arima(ts_simu_2, seasonal = T)

outcome <- forecast(mod, h = 30 + 35)
outcome_1 <- forecast(mod_2, h = 36)

df_out_temp <- list(
     disease_name = c(disease_list[i], disease_name[i]),
     outcome_before = summary(mod),
     # outcome_all_defore = mod,
     shapiro_before = shapiro.test(mod$residuals)$p.value,
     residual_before = Box.test(mod$residuals, lag = 24, type = "Ljung-Box")$p.value,
     outcome_after = summary(mod_2),
     # outcome_all_after = mod_2,
     shapiro_after = shapiro.test(mod_2$residuals)$p.value,
     residual_after = Box.test(mod_2$residuals, lag = 24, type = "Ljung-Box")$p.value
)

outcome_plot_1 <- data.frame(
     date = zoo::as.Date(time(outcome$x)),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted))
)
outcome_plot_2 <- data.frame(
     date = zoo::as.Date(time(outcome$mean)),
     mean = as.matrix(outcome$mean),
     lower_80 = as.matrix(outcome$lower[,1]),
     lower_95 = as.matrix(outcome$lower[,2]),
     upper_80 = as.matrix(outcome$upper[,1]),
     upper_95 = as.matrix(outcome$upper[,2])
)
# outcome_plot_2[outcome_plot_2 < 0] <- 0

outcome_plot_3 <- data.frame(
     date = zoo::as.Date(time(outcome_1$x)),
     simu = as.numeric(as.matrix(outcome_1$x)),
     fit = as.numeric(as.matrix(outcome_1$fitted))
)
outcome_plot_4 <- data.frame(
     date = zoo::as.Date(time(outcome_1$mean)),
     mean = as.matrix(outcome_1$mean),
     lower_80 = as.matrix(outcome_1$lower[,1]),
     lower_95 = as.matrix(outcome_1$lower[,2]),
     upper_80 = as.matrix(outcome_1$upper[,1]),
     upper_95 = as.matrix(outcome_1$upper[,2])
)
# outcome_plot_4[outcome_plot_4 < 0] <- 0

outcome_plot_1_2_link <- data.frame(
     date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
     value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
               outcome_plot_2[1, 'mean'])
)

outcome_plot_3_4_link <- data.frame(
     date = c(max(outcome_plot_3$date), min(outcome_plot_4$date)),
     value = c(outcome_plot_3[nrow(outcome_plot_3), 'fit'],
               outcome_plot_4[1, 'mean'])
)

max_value <- max(c(max(outcome_plot_1[,-1]),
                   max(outcome_plot_2[,-1]),
                   max(outcome_plot_3[,-1]),
                   max(outcome_plot_4[,-1])))

min_value <- min(c(min(outcome_plot_1[,-1]),
                   min(outcome_plot_2[,-1]),
                   min(outcome_plot_3[,-1]),
                   min(outcome_plot_4[,-1])))

fig1 <- ggplot()+
     # geom_line(mapping = aes(x = date, y = simu, colour = 'Real'), 
     #           size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
               size = 0.7, data = filter(datafile_single, date <= split_date))+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_2)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                 data = outcome_plot_2, alpha = 0.3, show.legend = F)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                 data = outcome_plot_2, alpha = 0.3, show.legend = F)+
     geom_vline(xintercept = min(outcome_plot_1_2_link$date), show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     # Link = 'black',
                     # Link = '#DC0000B2',
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 0))+
     labs(x = "Date",
          y = 'Cases',
          color = '',
          title = paste0('Fig. ', LETTERS[i], '1:', disease_name[i]))
fig2 <- ggplot()+
     geom_line(mapping = aes(x = date, y = simu, colour = 'Observed'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_3_4_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_4, show.legend = F)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                 data = outcome_plot_4, alpha = 0.3, show.legend = F)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                 data = outcome_plot_4, alpha = 0.3, show.legend = F)+
     geom_vline(xintercept = max_date, show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 20))+
     labs(x = "Date",
          y = NULL,
          color = '',
          title = paste0('Fig. ', LETTERS[i], '2'))

# assign(paste0('fig_outcome_2_', i), fig2)

fig_arima_1 <- fig1
fig_arima_2 <- fig2

# GreyModel ---------------------------------------------------------------

mod_3 <- gm(ts_simu_1, term = 30 + 35)
mod_4 <- gm(ts_simu_2, term = 36)

outcome_plot_1 <- data.frame(
     date = zoo::as.Date(time(mod_3$data)),
     simu = as.numeric(as.matrix(mod_3$data)),
     fit = as.numeric(as.matrix(mod_3$fitted))
)
outcome_plot_2 <- data.frame(
     date = seq.Date(as.Date('2020/01/01'), by = 'month', length.out = 30 + 35),
     mean = as.matrix(mod_3$forecasts)
)
# outcome_plot_2[outcome_plot_2 < 0] <- 0

outcome_plot_3 <- data.frame(
     date = zoo::as.Date(time(mod_4$data)),
     simu = as.numeric(as.matrix(mod_4$data)),
     fit = as.numeric(as.matrix(mod_4$fitted))
)
outcome_plot_4 <- data.frame(
     date = seq.Date(as.Date('2022/06/01'), by = 'month', length.out = 36),
     mean = as.matrix(mod_4$forecasts)
)
# outcome_plot_4[outcome_plot_4 < 0] <- 0

outcome_plot_1_2_link <- data.frame(
     date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
     value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
               outcome_plot_2[1, 'mean'])
)

outcome_plot_3_4_link <- data.frame(
     date = c(max(outcome_plot_3$date), min(outcome_plot_4$date)),
     value = c(outcome_plot_3[nrow(outcome_plot_3), 'fit'],
               outcome_plot_4[1, 'mean'])
)

max_value <- max(c(max(outcome_plot_1[,-1]),
                   max(outcome_plot_2[,-1]),
                   max(outcome_plot_3[,-1]),
                   max(outcome_plot_4[,-1])))

min_value <- min(c(min(outcome_plot_1[,-1]),
                   min(outcome_plot_2[,-1]),
                   min(outcome_plot_3[,-1]),
                   min(outcome_plot_4[,-1])))

fig1 <- ggplot()+
     # geom_line(mapping = aes(x = date, y = simu, colour = 'Real'), 
     #           size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
               size = 0.7, data = filter(datafile_single, date <= split_date))+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_2)+
     geom_vline(xintercept = min(outcome_plot_1_2_link$date), show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     # Link = 'black',
                     # Link = '#DC0000B2',
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 0))+
     labs(x = "Date",
          y = 'Cases',
          color = '',
          title = paste0('Fig. ', LETTERS[i], '1:', disease_name[i]))
fig2 <- ggplot()+
     geom_line(mapping = aes(x = date, y = simu, colour = 'Observed'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_3_4_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_4, show.legend = F)+
     geom_vline(xintercept = max_date, show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 20))+
     labs(x = "Date",
          y = NULL,
          color = '',
          title = paste0('Fig. ', LETTERS[i], '2'))

# assign(paste0('fig_outcome_2_', i), fig2)

fig_grey_1 <- fig1
fig_grey_2 <- fig2

# NNET --------------------------------------------------------------------

mod_5 <- nnetar(ts_simu_1)
mod_6 <- nnetar(ts_simu_2)

outcome_2 <- forecast(mod_5, h = 30 + 35)
outcome_3 <- forecast(mod_6, h = 36)

summary(outcome)
summary(outcome_1)

outcome_plot_1 <- data.frame(
     date = zoo::as.Date(time(outcome_2$x)),
     simu = as.numeric(as.matrix(outcome_2$x)),
     fit = as.numeric(as.matrix(outcome_2$fitted))
)
outcome_plot_2 <- data.frame(
     date = zoo::as.Date(time(outcome_2$mean)),
     mean = as.matrix(outcome_2$mean)
)
# outcome_plot_2[outcome_plot_2 < 0] <- 0

outcome_plot_3 <- data.frame(
     date = zoo::as.Date(time(outcome_3$x)),
     simu = as.numeric(as.matrix(outcome_3$x)),
     fit = as.numeric(as.matrix(outcome_3$fitted))
)
outcome_plot_4 <- data.frame(
     date = zoo::as.Date(time(outcome_3$mean)),
     mean = as.matrix(outcome_3$mean)
)
# outcome_plot_4[outcome_plot_4 < 0] <- 0

outcome_plot_1_2_link <- data.frame(
     date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
     value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
               outcome_plot_2[1, 'mean'])
)

outcome_plot_3_4_link <- data.frame(
     date = c(max(outcome_plot_3$date), min(outcome_plot_4$date)),
     value = c(outcome_plot_3[nrow(outcome_plot_3), 'fit'],
               outcome_plot_4[1, 'mean'])
)

max_value <- max(c(max(outcome_plot_1[,-1], na.rm = T),
                   max(outcome_plot_2[,-1], na.rm = T),
                   max(outcome_plot_3[,-1], na.rm = T),
                   max(outcome_plot_4[,-1], na.rm = T)))

min_value <- min(c(min(outcome_plot_1[,-1], na.rm = T),
                   min(outcome_plot_2[,-1], na.rm = T),
                   min(outcome_plot_3[,-1], na.rm = T),
                   min(outcome_plot_4[,-1], na.rm = T)))

fig1 <- ggplot()+
     # geom_line(mapping = aes(x = date, y = simu, colour = 'Real'), 
     #           size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
               size = 0.7, data = filter(datafile_single, date <= split_date))+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_2)+
     geom_vline(xintercept = min(outcome_plot_1_2_link$date), show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     # Link = 'black',
                     # Link = '#DC0000B2',
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 0))+
     labs(x = "Date",
          y = 'Cases',
          color = '',
          title = paste0('Fig. ', LETTERS[i], '1:', disease_name[i]))
fig2 <- ggplot()+
     geom_line(mapping = aes(x = date, y = simu, colour = 'Observed'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_3_4_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_4, show.legend = F)+
     geom_vline(xintercept = max_date, show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 20))+
     labs(x = "Date",
          y = NULL,
          color = '',
          title = paste0('Fig. ', LETTERS[i], '2'))

# assign(paste0('fig_outcome_2_', i), fig2)

fig_nnet_1 <- fig1
fig_nnet_2 <- fig2

# STL ---------------------------------------------------------------------

outcome <- stlf(ts_simu_1, lambda=0, h=30 + 35)
outcome_1 <- stlf(ts_simu_2, lambda=0, h=36)

outcome_plot_1 <- data.frame(
     date = zoo::as.Date(time(outcome$x)),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted))
)
outcome_plot_2 <- data.frame(
     date = zoo::as.Date(time(outcome$mean)),
     mean = as.matrix(outcome$mean),
     lower_80 = as.matrix(outcome$lower[,1]),
     lower_95 = as.matrix(outcome$lower[,2]),
     upper_80 = as.matrix(outcome$upper[,1]),
     upper_95 = as.matrix(outcome$upper[,2])
)
# outcome_plot_2[outcome_plot_2 < 0] <- 0

outcome_plot_3 <- data.frame(
     date = zoo::as.Date(time(outcome_1$x)),
     simu = as.numeric(as.matrix(outcome_1$x)),
     fit = as.numeric(as.matrix(outcome_1$fitted))
)
outcome_plot_4 <- data.frame(
     date = zoo::as.Date(time(outcome_1$mean)),
     mean = as.matrix(outcome_1$mean),
     lower_80 = as.matrix(outcome_1$lower[,1]),
     lower_95 = as.matrix(outcome_1$lower[,2]),
     upper_80 = as.matrix(outcome_1$upper[,1]),
     upper_95 = as.matrix(outcome_1$upper[,2])
)
# outcome_plot_4[outcome_plot_4 < 0] <- 0

outcome_plot_1_2_link <- data.frame(
     date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
     value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
               outcome_plot_2[1, 'mean'])
)

outcome_plot_3_4_link <- data.frame(
     date = c(max(outcome_plot_3$date), min(outcome_plot_4$date)),
     value = c(outcome_plot_3[nrow(outcome_plot_3), 'fit'],
               outcome_plot_4[1, 'mean'])
)

max_value <- max(c(max(outcome_plot_1[,-1]),
                   max(outcome_plot_2[,-1]),
                   max(outcome_plot_3[,-1]),
                   max(outcome_plot_4[,-1])))

min_value <- min(c(min(outcome_plot_1[,-1]),
                   min(outcome_plot_2[,-1]),
                   min(outcome_plot_3[,-1]),
                   min(outcome_plot_4[,-1])))

fig1 <- ggplot()+
     # geom_line(mapping = aes(x = date, y = simu, colour = 'Real'), 
     #           size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
               size = 0.7, data = filter(datafile_single, date <= split_date))+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_2)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                 data = outcome_plot_2, alpha = 0.3, show.legend = F)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                 data = outcome_plot_2, alpha = 0.3, show.legend = F)+
     geom_vline(xintercept = min(outcome_plot_1_2_link$date), show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     # Link = 'black',
                     # Link = '#DC0000B2',
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 0))+
     labs(x = "Date",
          y = 'Cases',
          color = '',
          title = paste0('Fig. ', LETTERS[i], '1:', disease_name[i]))
fig2 <- ggplot()+
     geom_line(mapping = aes(x = date, y = simu, colour = 'Observed'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_3_4_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_4, show.legend = F)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                 data = outcome_plot_4, alpha = 0.3, show.legend = F)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                 data = outcome_plot_4, alpha = 0.3, show.legend = F)+
     geom_vline(xintercept = max_date, show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 20))+
     labs(x = "Date",
          y = NULL,
          color = '',
          title = paste0('Fig. ', LETTERS[i], '2'))

# assign(paste0('fig_outcome_2_', i), fig2)

fig_stl_1 <- fig1
fig_stl_2 <- fig2

# ETS ---------------------------------------------------------------------

outcome <- forecast(ets(ts_simu_1), h=30 + 35)
outcome_1 <- forecast(ets(ts_simu_2), h=36)

outcome_plot_1 <- data.frame(
     date = zoo::as.Date(time(outcome$x)),
     simu = as.numeric(as.matrix(outcome$x)),
     fit = as.numeric(as.matrix(outcome$fitted))
)
outcome_plot_2 <- data.frame(
     date = zoo::as.Date(time(outcome$mean)),
     mean = as.matrix(outcome$mean),
     lower_80 = as.matrix(outcome$lower[,1]),
     lower_95 = as.matrix(outcome$lower[,2]),
     upper_80 = as.matrix(outcome$upper[,1]),
     upper_95 = as.matrix(outcome$upper[,2])
)
# outcome_plot_2[outcome_plot_2 < 0] <- 0

outcome_plot_3 <- data.frame(
     date = zoo::as.Date(time(outcome_1$x)),
     simu = as.numeric(as.matrix(outcome_1$x)),
     fit = as.numeric(as.matrix(outcome_1$fitted))
)
outcome_plot_4 <- data.frame(
     date = zoo::as.Date(time(outcome_1$mean)),
     mean = as.matrix(outcome_1$mean),
     lower_80 = as.matrix(outcome_1$lower[,1]),
     lower_95 = as.matrix(outcome_1$lower[,2]),
     upper_80 = as.matrix(outcome_1$upper[,1]),
     upper_95 = as.matrix(outcome_1$upper[,2])
)
# outcome_plot_4[outcome_plot_4 < 0] <- 0

outcome_plot_1_2_link <- data.frame(
     date = c(max(outcome_plot_1$date), min(outcome_plot_2$date)),
     value = c(outcome_plot_1[nrow(outcome_plot_1), 'fit'],
               outcome_plot_2[1, 'mean'])
)

outcome_plot_3_4_link <- data.frame(
     date = c(max(outcome_plot_3$date), min(outcome_plot_4$date)),
     value = c(outcome_plot_3[nrow(outcome_plot_3), 'fit'],
               outcome_plot_4[1, 'mean'])
)

max_value <- max(c(max(outcome_plot_1[,-1]),
                   max(outcome_plot_2[,-1]),
                   max(outcome_plot_3[,-1]),
                   max(outcome_plot_4[,-1])))

min_value <- min(c(min(outcome_plot_1[,-1]),
                   min(outcome_plot_2[,-1]),
                   min(outcome_plot_3[,-1]),
                   min(outcome_plot_4[,-1])))

fig1 <- ggplot()+
     # geom_line(mapping = aes(x = date, y = simu, colour = 'Real'), 
     #           size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
               size = 0.7, data = filter(datafile_single, date <= split_date))+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_1)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_1_2_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_2)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                 data = outcome_plot_2, alpha = 0.3, show.legend = F)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                 data = outcome_plot_2, alpha = 0.3, show.legend = F)+
     geom_vline(xintercept = min(outcome_plot_1_2_link$date), show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     # Link = 'black',
                     # Link = '#DC0000B2',
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 0))+
     labs(x = "Date",
          y = 'Cases',
          color = '',
          title = paste0('Fig. ', LETTERS[i], '1:', disease_name[i]))
fig2 <- ggplot()+
     geom_line(mapping = aes(x = date, y = simu, colour = 'Observed'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = fit, colour = 'Fitted'), 
               size = 0.7, data = outcome_plot_3, show.legend = F)+
     geom_line(mapping = aes(x = date, y = value), color = '#DC0000B2', 
               size = 0.7, data = outcome_plot_3_4_link, show.legend = F)+
     geom_line(mapping = aes(x = date, y = mean, colour = 'Forecasted'),
               size = 0.7, data = outcome_plot_4, show.legend = F)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_80, ymax = upper_80, fill = 'red'),
                 data = outcome_plot_4, alpha = 0.3, show.legend = F)+
     geom_ribbon(mapping = aes(x = date, ymin = lower_95, ymax = upper_95, fill = 'red'),
                 data = outcome_plot_4, alpha = 0.3, show.legend = F)+
     geom_vline(xintercept = max_date, show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Fitted = "#00A087B2",
                     Forecasted = "#DC0000B2",
                     Observed = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 20))+
     labs(x = "Date",
          y = NULL,
          color = '',
          title = paste0('Fig. ', LETTERS[i], '2'))

# assign(paste0('fig_outcome_2_', i), fig2)

fig_ets_1 <- fig1
fig_ets_2 <- fig2

# Combined plot ------------------------------------------------------------

df_mods <- data.frame(
     grey = as.matrix(mod_3$forecasts),
     arima = as.numeric(forecast(mod, h = 30 + 35)$mean),
     nnet = as.numeric(outcome_2$mean),
     ets = as.numeric(forecast(ets(ts_simu_1), h=30 + 35)$mean),
     stl = as.numeric(stlf(ts_simu_1, lambda=0, h=30 + 35)$mean),
     date = seq.Date(as.Date('2020/01/01'), by = 'month', length.out = 30 + 35)
)

fig1 <- ggplot(data = df_mods)+
     geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
               size = 0.7, data = filter(datafile_single, date <= split_date))+
     geom_line(mapping = aes(x = date, y = arima, colour = 'ARIMA'),
               size = 0.7)+
     geom_line(mapping = aes(x = date, y = grey, colour = 'GreyModel'),
               size = 0.7)+
     geom_line(mapping = aes(x = date, y = nnet, colour = 'NNET'),
               size = 0.7)+
     geom_line(mapping = aes(x = date, y = ets, colour = 'ETS'),
               size = 0.7)+
     geom_line(mapping = aes(x = date, y = stl, colour = 'STL'),
               size = 0.7)+
     geom_vline(xintercept = min(outcome_plot_1_2_link$date), show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Observed = 'black',
                     GreyModel = '#9F248FFF',
                     STL = "#00A087B2",
                     NNET = '#DC0000B2',
                     ETS = "#FFCE4EFF",
                     ARIMA = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 0))+
     labs(x = "Date",
          y = 'Cases',
          color = '',
          title = paste0('Fig. ', LETTERS[i], '1:', disease_name[i]))+
     guides(color = guide_legend(nrow = 1))

df_mods <- data.frame(
     grey = as.matrix(mod_4$forecasts),
     arima = as.numeric(forecast(mod_2, h = 36)$mean),
     nnet = as.numeric(outcome_3$mean),
     ets = as.numeric(forecast(ets(ts_simu_2), h=36)$mean),
     stl = as.numeric(stlf(ts_simu_2, lambda=0, h=36)$mean),
     date = seq.Date(as.Date('2022/06/01'), by = 'month', length.out = 36)
)

fig2 <- ggplot(data = df_mods)+
     geom_line(mapping = aes(x = date, y = value, colour = 'Observed'), 
               size = 0.7, data = datafile_single)+
     geom_line(mapping = aes(x = date, y = arima, colour = 'ARIMA'),
               size = 0.7)+
     geom_line(mapping = aes(x = date, y = grey, colour = 'GreyModel'),
               size = 0.7)+
     geom_line(mapping = aes(x = date, y = nnet, colour = 'NNET'),
               size = 0.7)+
     geom_line(mapping = aes(x = date, y = ets, colour = 'ETS'),
               size = 0.7)+
     geom_line(mapping = aes(x = date, y = stl, colour = 'STL'),
               size = 0.7)+
     geom_vline(xintercept = max_date, show.legend = F,
                linetype = 'longdash')+
     geom_hline(yintercept = 0, show.legend = F)+
     coord_cartesian(ylim = c(0, NA))+
     scale_x_date(expand = c(0, 31),
                  date_breaks = "2 years",
                  date_labels = '%Y')+
     scale_y_continuous(expand = c(0, 0),
                        breaks = pretty(c(min_value, max_value, 0)),
                        limits = range(pretty(c(min_value, max_value, 0))))+
     scale_color_manual(
          values = c(Observed = 'black',
                     GreyModel = '#9F248FFF',
                     STL = "#00A087B2",
                     NNET = '#DC0000B2',
                     ETS = "#FFCE4EFF",
                     ARIMA = '#3C5488B2')
     )+
     theme_set()+
     theme(legend.position = 'bottom',
           plot.margin = margin(0, 0, 0, 0))+
     labs(x = "Date",
          y = NULL,
          color = '',
          title = paste0('Fig. ', LETTERS[i], '2'))+
     guides(color = guide_legend(nrow = 1))

fig_com <- (fig1 + fig2) + 
     plot_layout(ncol = 2, byrow = F, width = c(19, 19), tag_level = 'new', guides = 'collect')&
     theme(legend.position = 'bottom')

# save --------------------------------------------------------------------

fig_ts <- fig_grey_1 + fig_grey_2 + 
     fig_stl_1 + fig_stl_2+
     fig_nnet_1 + fig_nnet_2+
     fig_ets_1 + fig_ets_2+
     fig_arima_1 + fig_arima_2+
     plot_layout(ncol = 2, guides = 'collect')&
     theme(legend.position = 'bottom')

cowplot::plot_grid(fig_ts, fig_com, ncol = 1, rel_heights = c(5, 1))

ggsave(filename = './fig/202208303.pdf',
       width = 12, height = 21,
       limitsize = FALSE, device = cairo_pdf)
