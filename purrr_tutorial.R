head(airquality)

FtoC <- function(F) {
  C <- (F-32)*5/9
 return(C)
}

FtoC(63)

map(airquality$Temp, FtoC)
#map always returns a list

airquality2 <- airquality %>% 
  mutate(TempC = map(Temp, FtoC),
         TempC = unlist(TempC)) 
  

str(airquality2)

#same as
airquality3 <- airquality %>% 
  mutate(TempC = map_dbl(Temp, FtoC))

str(airquality3)


mymean <- function(x) {
  mean(x, na.rm = TRUE)
}

aqm <- airquality3 %>% 
  select(Ozone:Temp) %>% 
  map(., mymean) 


aqm.d <- airquality3 %>% 
  select(Ozone:Temp) %>% 
  map_dbl(., mymean) #_dbl returns output as doubles


#################### map2 enables two input columns/datasets/lists

histogram <- function(data, var) {
  theme_set(theme_bw())
  plot <- ggplot(data, aes(x = .data[[var]])) + 
    geom_histogram(color = "white", fill = "lightblue",
    binwidth = function(x)2*IQR(x)/(length(x)^(1/3)))
  return(plot)
}

airquality_num <- select(airquality, Ozone:Temp)
airquality_num

histograms <- map2(.x = list(airquality_num),
                   .y = names(airquality_num), 
                   .f = histogram)
histograms
library(patchwork)
wrap_plots(histograms) +
  plot_layout(ncol = length(histograms))
  #plot_layout(nrow = length(histograms))

#################################### nested dataframes
airquality_nest <- airquality %>% 
  group_by(Month) %>% 
  nest()

airquality_nest

#define a function to compute the mean of the Ozone variable
compute_mean_Ozone <- function(data) {
  data %>% 
    dplyr::select(Ozone) %>% 
    dplyr::summarize(MeanOzone = mean(Ozone, na.rm = TRUE)) %>% 
    dplyr::pull(MeanOzone)
}

#apply defined function to each element of the data column of 
#airquality_nest
airquality_nest %>% 
  mutate(MeanOzone = map_dbl(data, compute_mean_Ozone)) %>% #do str(airquality_nest) to see that the data are in a column called data in this nested dataframe
  mutate(Date = make_date(year = 1973, month = Month, day = 1)) %>% 
  ggplot() + 
  aes(x = Date, y = MeanOzone) +
  geom_point(size = 3) +
  scale_x_date(date_labels = "%b") +
  ylab("Mean Ozone (ppb)") + xlab("Month") + ggtitle("Year:1973")
