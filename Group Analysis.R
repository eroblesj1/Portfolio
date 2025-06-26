#install.packages("pacman") 
#LOADING PACKAGES  
pacman::p_load(tsibble, readxl, remotes, tidyverse, tsbox, forecast ,tseries, urca, ggplot2, vars, zoo, tidyr, dplyr) 

#Read Excel file  
prod_data <- read_excel("/Users/eli/Downloads/UK_Comp_US_GBRPROINDMISMEI.xlsx") 
colnames(prod_data) <- c("DATE", "UK_Prod", "US_Prod")  
prod_data$DATE <- as.Date(prod_data$DATE) 
#Convert to time series data  
uk_ts <- ts(prod_data$UK_Prod, start = c(1997, 1), end = c(2024, 3), frequency = 12)  
us_ts <- ts(prod_data$US_Prod, start = c(1997, 1), end = c(2024, 3), frequency = 12) 

#Create a long format data frame for plotting 
plot_data <- prod_data %>% dplyr::select(DATE, UK_Prod, US_Prod) %>% pivot_longer(cols = c("UK_Prod", "US_Prod"), names_to = "Country", values_to = "Production_Index") 

#Plot 
ggplot(plot_data, aes(x = DATE, y = Production_Index, color = Country)) + geom_line(size = 1.1) + scale_color_manual(values = c("UK_Prod" = "#0072B2", "US_Prod" = "#D55E00"), labels = c("United Kingdom", "United States")) + labs(title = "UK vs US Industrial Production (1948–2024)", x = "Year", y = "Industrial Production Index", color = "Country") + theme_minimal(base_size = 14) + theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold")) 

prod_data <- prod_data %>% mutate(log_UK = log(UK_Prod), log_US = log(US_Prod)) 
uk_ts_log <- ts(prod_data$log_UK, start = c(1997, 1), end = c(2024, 3), frequency = 12)  
us_ts_log <- ts(prod_data$log_US, start = c(1997, 1), end = c(2024, 3), frequency = 12)

#Decompose and plot without specifying 'main' 
plot(decompose(uk_ts_log)) + title(main = "Decomposition of UK Industrial Production (Log)") 
plot(decompose(us_ts_log)) + title(main = "Decomposition of US Industrial Production (Log)") 

#Run a dickey fuller-test to check stationarity  
adf.test(uk_ts_log) #With a p-value of 0.7786, which is much higher than the common threshold of 0.05, we fail to reject the null hypothesis — so the log-transformed UK production series is not stationary. # #NB: nuLL says series is non-stationary #alternate says series is stationary 
adf.test(us_ts_log) #Also not stationary. p-value = 0.7428 

#First differencing to achieve stationarity  
uk_ts_log_diff1 <- diff(uk_ts_log)  
us_ts_log_diff1<- diff(us_ts_log) 

#Second differencing 
uk_ts_log_diff <- diff(uk_ts_log_diff1)  
us_ts_log_diff <- diff(us_ts_log_diff1) 

#check with dickey-fuller  
adf.test(uk_ts_log_diff)  
adf.test(us_ts_log_diff) 

#Add differenced series to your data frame 
prod_data_diff <- prod_data %>% mutate( log_UK = log(UK_Prod), log_US = log(US_Prod), diff_log_UK = c(NA, diff(log_UK)), diff_log_US = c(NA, diff(log_US)) ) 

#Pivot to long format for ggplot 
diff_long <- prod_data_diff %>% dplyr::select(DATE, diff_log_UK, diff_log_US) %>% pivot_longer(cols = starts_with("diff_log"), names_to = "Country", values_to = "Differenced_Log") %>% mutate(Country = recode(Country, diff_log_UK = "United Kingdom", diff_log_US = "United States")) 

#Plot 
ggplot(diff_long, aes(x = DATE, y = Differenced_Log, color = Country)) + geom_line(na.rm = TRUE, size = 1) + labs(title = "Second-Differenced Log Industrial Production: UK vs US", x = "Year", y = "Differenced Log Production", color = "Country") + theme_minimal(base_size = 14) + theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold")) 

#Generate ACF and PACF plots for UK 
acf_uk <- ggAcf(uk_ts_log_diff, lag.max = 40) + ggtitle("ACF - UK")  
acf_uk 
pacf_uk <- ggPacf(uk_ts_log_diff, lag.max = 40) + ggtitle("PACF - UK") 
pacf_uk 

#Generate ACF and PACF plots for US 
acf_us <- ggAcf(us_ts_log_diff, lag.max = 40) + ggtitle("ACF - US")  
acf_us 
pacf_us <- ggPacf(us_ts_log_diff, lag.max = 40) + ggtitle("PACF - US") 
pacf_us 
#PACF Detects AR terms (lags of the variable itself)  
#ACF Detects MA terms (lags of the residuals/errors) 
#If the ACF declines faster than PACF we are likely having a moving average process #if the PACF declines faster than the ACF, then we are likely having a AR process  
#an examination of the ACFs and PACFs of both UK and US shows a moving average. In all ACF tends to decline faster after the first lag 1 and the PACF shows a gradual decay.  

us_arima <- auto.arima(us_ts_log)  
summary(us_arima) 
#WE COULD TRY OTHERS BUT THIS GIVES US A STRAIGHT FORWARD EASY ANSWER SO WE CAN JUST PICK IT. (0,2,1) 
uk_arima <- auto.arima(uk_ts_log)  
summary(uk_arima) 

#CHECK RESIDUALS  
checkresiduals(us_arima)  
checkresiduals(uk_arima) 

#FORECASTING  
us_forecast <- forecast(us_arima, h = 60) # number helps depend forecast period  
uk_forecast <- forecast(uk_arima, h = 60) 
autoplot(us_forecast) + ggtitle("US Industrial Production Forecast") + ylab("Log Industrial Production Index") 
autoplot(uk_forecast) + ggtitle("UK Industrial Production Forecast") + ylab("Log Industrial Production Index")

# Plot original log time series for both countries
autoplot(cbind(UK = uk_ts_log, US = us_ts_log)) +
  autolayer(uk_forecast, series = "UK Forecast") +
  autolayer(us_forecast, series = "US Forecast") +
  labs(title = "UK vs US Industrial Production (with Forecasts)",
       x = "Year", y = "Log Industrial Production Index", color = "Series") +
  scale_color_manual(values = c("UK" = "#0072B2", "US" = "#D55E00",
                                "UK Forecast" = "#56B4E9", "US Forecast" = "#E69F00")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5))


#**************UK Industries Forecasts

#LOADING PACKAGES  
pacman::p_load(tsibble, readxl, remotes, tidyverse, tsbox, forecast ,tseries, urca, ggplot2, vars, zoo, tidyr, dplyr) 
#CLEANING
#Read Excel file  
UKIndexOfProd <- read.csv("/Users/eli/Downloads/UKIndexOfProduction-15Aug2024.csv")
#selects the columns/variables we will work with 
UKIndexOfProd <- UKIndexOfProd[, c( 1, 228, 229, 238, 250, 252, 254, 255, 256, 258, 260, 270, 271, 275, 278, 284, 280, 282, 286, 289, 292, 293, 235)] 
#rename column names 
colnames(UKIndexOfProd) <- c("Date", "Consumer Goods", "Durables", "Food and Tobacco", "Textiles", "Leather", "Wood", "Paper", "Printing", "Petroleum", "Chemical", "Plastics", "Nonmetallic Mineral", "Basic Metals", "Fabricated Metals", "Machinery", "Computer and Electronic", "Electrical Equipment", "Motor Vehicles and Parts", "Aerospace Equipment", "Furniture", "Other", "Mining") 
#removes annual and quarterly data  
UKIndexOfProd <- UKIndexOfProd %>%  
  slice(389:1303)  
#convert to ISO dates 
UKIndexOfProd$Date <- as.Date(paste0(UKIndexOfProd$Date, " 01"), format = "%Y %b %d") 
#converts values to numerical 
UKIndexOfProd <- UKIndexOfProd %>% mutate(across(-Date, as.numeric)) 
#converts to tsibble 
UKIndexOfProd <- as_tsibble(UKIndexOfProd, index = Date) 
#filter from 1997-01-01 to 2024-03-01 
UKprod_data<- UKIndexOfProd %>% slice(589:915) 
UKprod_data
#Scale down Mining to be comparable
UKprod_data$Mining <- UKprod_data$Mining/100

#Convert to time series data  
UK_ConsumerGoods <- ts(UKprod_data$`Consumer Goods`, start = c(1997, 1), frequency = 12)
UK_Durables <- ts(UKprod_data$`Durables`, start = c(1997, 1), frequency = 12)
UK_FoodTobacco <- ts(UKprod_data$`Food and Tobacco`, start = c(1997, 1), frequency = 12)
UK_Textiles <- ts(UKprod_data$`Textiles`, start = c(1997, 1), frequency = 12)
UK_Leather <- ts(UKprod_data$`Leather`, start = c(1997, 1), frequency = 12)
UK_Wood <- ts(UKprod_data$`Wood`, start = c(1997, 1), frequency = 12)
UK_Paper <- ts(UKprod_data$`Paper`, start = c(1997, 1), frequency = 12)
UK_Printing <- ts(UKprod_data$`Printing`, start = c(1997, 1), frequency = 12)
UK_Petroleum <- ts(UKprod_data$`Petroleum`, start = c(1997, 1), frequency = 12)
UK_Chemical <- ts(UKprod_data$`Chemical`, start = c(1997, 1), frequency = 12)
UK_Plastics <- ts(UKprod_data$`Plastics`, start = c(1997, 1), frequency = 12)
UK_NonmetallicMineral <- ts(UKprod_data$`Nonmetallic Mineral`, start = c(1997, 1), frequency = 12)
UK_BasicMetals <- ts(UKprod_data$`Basic Metals`, start = c(1997, 1), frequency = 12)
UK_FabricatedMetals <- ts(UKprod_data$`Fabricated Metals`, start = c(1997, 1), frequency = 12)
UK_Machinery <- ts(UKprod_data$`Machinery`, start = c(1997, 1), frequency = 12)
UK_ComputerElectronic <- ts(UKprod_data$`Computer and Electronic`, start = c(1997, 1), frequency = 12)
UK_ElectricalEquipment <- ts(UKprod_data$`Electrical Equipment`, start = c(1997, 1), frequency = 12)
UK_MotorVehicles <- ts(UKprod_data$`Motor Vehicles and Parts`, start = c(1997, 1), frequency = 12)
UK_AerospaceEquipment <- ts(UKprod_data$`Aerospace Equipment`, start = c(1997, 1), frequency = 12)
UK_Furniture <- ts(UKprod_data$`Furniture`, start = c(1997, 1), frequency = 12)
UK_Other <- ts(UKprod_data$`Other`, start = c(1997, 1), frequency = 12)
UK_Mining <- ts(UKprod_data$`Mining`, start = c(1997, 1), frequency = 12)

#Create a named list of the time series
ts_list <- list(
  ConsumerGoods = UK_ConsumerGoods, Durables = UK_Durables, 
  FoodTobacco = UK_FoodTobacco, Textiles = UK_Textiles, Leather = UK_Leather, 
  Wood = UK_Wood, Paper = UK_Paper, Printing = UK_Printing, Petroleum = UK_Petroleum, 
  Chemical = UK_Chemical, Plastics = UK_Plastics, NonmetallicMineral = UK_NonmetallicMineral,
  BasicMetals = UK_BasicMetals, FabricatedMetals = UK_FabricatedMetals,
  Machinery = UK_Machinery, ComputerElectronic = UK_ComputerElectronic, ElectricalEquipment = UK_ElectricalEquipment,
  MotorVehicles = UK_MotorVehicles, AerospaceEquipment = UK_AerospaceEquipment,
  Furniture = UK_Furniture, Other = UK_Other,Mining = UK_Mining
)

#Loop through and plot each
for (name in names(ts_list)) {
  plot(ts_list[[name]], main = name, col = "blue", bty = "l", ylab = "Production Index", xlab = "Time")
}

#Check stationary with ADF and KPSS
UK.adf <- lapply(ts_list, function(series) {
  adf.test(series)
})
UK.kpss <- lapply(ts_list, function(series) {
  kpss.test(series)
})
for (name in names(ts_list)) {
  cat("\nADF and KPSS Test for:", name, "\n")
  print(UK.adf[[name]]$p.value) #If the p-value is less than 0.05, data is stationary.
  print(UK.kpss[[name]]$p.value) #If the p-value for the kpss test is greater than 0.05, data is stationary.
}

#Apply log transformation to all columns except the first one
UKprod_data_log <- UKprod_data[-1] %>%
  mutate(across(everything(), log, .names = "log_{.col}"))
log_ts_list <- list()
for (col_name in names(UKprod_data_log)) {
  clean_name <- sub("log_", "", col_name)
  log_ts_list[[clean_name]] <- ts(UKprod_data_log[[col_name]], 
                                  start = c(1997, 1), 
                                  frequency = 12)
}

#Decompose and plot each time series in log_ts_list
for (name in names(log_ts_list)) {
  plot(decompose(log_ts_list[[name]]))
  title(main = paste(name, "(Log)"))
}

#Generate ACF and PACF plots for UK 
for (name in names(log_ts_list)) {
  print(ggAcf(log_ts_list[[name]]) +
          ggtitle(paste("ACF of UK", name, "(Log)"))
  )
}
for (name in names(log_ts_list)) {
  print(ggPacf(log_ts_list[[name]]) +
          ggtitle(paste("PACF of UK", name, "(Log)"))
  )
}

#Generate ARIMA models
arima_models <- list()
for (name in names(log_ts_list)) {
  cat("Fitting ARIMA model for", name, "\n")
  arima_models[[name]] <- auto.arima(log_ts_list[[name]])
  print(summary(arima_models[[name]]))
}

#Check residuals
for (name in names(arima_models)) {
  cat("Residual diagnostics for", name, "\n")
  checkresiduals(arima_models[[name]])
}

#Generate forecasts
forecast_list <- list()
for (name in names(arima_models)) {
  forecast_list[[name]] <- forecast(arima_models[[name]], h = 60)
  print(autoplot(forecast_list[[name]]) + ggtitle(paste("Forecast for UK", name)) + ylab("Log Production Index") + xlab("Time")
  )
}


#US DATASET*** 
USTotalProd <- read.csv("/Users/eli/Downloads/USTotalProd.csv", header = TRUE)
#selects the columns/variables we will work with
USTotalProd <- USTotalProd[, c(1, 5, 8, 10:28, 37)]
#rename column names
colnames(USTotalProd) <- c("Date", "Consumer Goods", "Durables", 
                           "Food and Tobacco", "Textiles", "Leather", "Wood", "Paper",
                           "Printing", "Petroleum", "Chemical", "Plastics", 
                           "Nonmetallic Mineral", "Basic Metals", "Fabricated Metals",
                           "Machinery", "Computer and Electronic", 
                           "Electrical Equipment", 
                           "Motor Vehicles and Parts", "Aerospace Equipment", 
                           "Furniture", "Other", "Mining")
#filters dates from 1948 to 2024
USTotalProd <- USTotalProd %>% slice(349:1266)
#convert to ISO dates
USTotalProd$Date <- as.Date(USTotalProd$Date)  # ensure it's Date class
#converts to tsibble
USTotalProd <- as_tsibble(USTotalProd, index = Date)
#filter from 1997-01-01 to 2024-06-01
US_filtered <- USTotalProd %>%
  slice(589:918)

#List of variables to plot 
#US_variables <- names(US_filtered)[-1] # Assuming first column is Date 
industries <- c("Consumer Goods", "Durables", "Food and Tobacco", "Textiles",
                "Leather", "Wood", "Paper", "Printing", "Petroleum", "Chemical",
                "Plastics", "Nonmetallic Mineral", "Basic Metals", "Fabricated Metals",
                "Machinery", "Computer and Electronic", "Electrical Equipment",
                "Motor Vehicles and Parts", "Aerospace Equipment", "Furniture",
                "Other", "Mining")

#Convert to time series for each industry
log_transformed_data <- list()
for (industry in industries) {
  ts_data <- ts(US_filtered[[industry]], start = c(1997, 1), frequency = 12)
  log_transformed_data[[industry]] <- log(ts_data)
}

#Stationarity Check (ADF Test)
adf_results <- list()
for (industry in industries) {
  #ADF test for log-transformed data
  adf_result <- adf.test(log_transformed_data[[industry]])
  adf_results[[industry]] <- adf_result$p.value
}

#Print ADF p-values
print(adf_results)

#ACF and PACF plots
acf_pacf_plots <- list()
for (industry in industries) {
  #ACF and PACF plots for differenced series (if differencing is done)
  ts_log_diff <- log_transformed_data[[industry]]
  acf_pacf_plots[[industry]] <- list(
    acf = Acf(ts_data, main = paste("ACF for", industry)),
    pacf = Pacf(ts_data, main = paste("PACF for", industry))
  )
}

#Model Fitting: Fit ARIMA model
#Fit ARIMA model directly on log-transformed data (not differenced)
arima_models_us <- list()
for (industry in industries) {
  cat("Fitting ARIMA model for", industry, "\n")
  arima_model <- auto.arima(log_transformed_data[[industry]], seasonal = TRUE)
  arima_models_us[[industry]] <- arima_model
  print(summary(arima_model))
}

#Check residuals for each model
residual_diagnostics <- list()
for (industry in industries) {
  cat("Checking residuals for", industry, "\n")
  residual_diagnostics[[industry]] <- checkresiduals(arima_models_us[[industry]])
}

#Generate forecasts for US
forecast_list_us <- list()
for (name in names(arima_models_us)) {  
  forecast_list_us[[name]] <- forecast(arima_models_us[[name]], h = 60) 
  print(
    autoplot(forecast_list_us[[name]]) + 
      ggtitle(paste("Forecast for US", name)) + 
      ylab("Log Production Index") + 
      xlab("Time") +
      theme_minimal()
  )
}

#************** Combine forecasts for UK and US
combined_forecasts <- list()
for (name in names(arima_models)) {
  uk_fc <- forecast_list[[name]]
  us_fc <- forecast_list_us[[name]]
 #Combine forecast data
  uk_df <- data.frame(
    time = as.numeric(time(uk_fc$mean)),
    value = as.numeric(uk_fc$mean),
    region = "UK"
  )
  us_df <- data.frame(
    time = as.numeric(time(us_fc$mean)),
    value = as.numeric(us_fc$mean),
    region = "US"
  )
  combined_df <- rbind(uk_df, us_df)
 
#Plot
  p <- ggplot(combined_df, aes(x = time, y = value, color = region)) +
    geom_line(size = 1) +
    labs(
      title = paste("UK vs US Forecast for", name),
      x = "Time",
      y = "Log Production Index"
    ) +
    theme_minimal()
  print(p)
  combined_forecasts[[name]] <- combined_df
}



#************Differences
forecast_differences <- list()

for (name in names(forecast_list)) {
  uk_fc <- forecast_list[[name]]$mean
  us_fc <- forecast_list_us[[name]]$mean
  
#Make sure both have the same length
  n <- min(length(uk_fc), length(us_fc))
  
  diff_values <- as.numeric(uk_fc[1:n]) - as.numeric(us_fc[1:n])
  time_vals <- time(uk_fc)[1:n]
  keep <- time_vals >= 2025 & time_vals <= 2030
  forecast_differences[[name]] <- data.frame(
    Industry = name,
    Time = time_vals,
    Difference = diff_values
  )
}
combined_differences <- do.call(rbind, forecast_differences)
summary_table <- combined_differences %>%
  group_by(Industry) %>%
  summarise(
    Mean_Diff = mean(Difference),
  )

print(summary_table, n=22)
