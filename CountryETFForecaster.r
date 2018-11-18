library(scales) # Percentage formatting
library(quantmod) # Getting financial data
library(tidyverse) # Data wrangling, rownames_to_columns

# Input CAPE calculation date
date_cape <- as.Date("2018-10-31")

# Read CAPE values from csv & combine with countries and tickers
capes <- read.csv("capes.csv", header = F)
colnames(capes)[1] <- "Country"

countries <- data.frame(c("Greece", "Russia", "Ireland", "Argentina", "Italy", "Austria", "Portugal",
                          "Israel", "Spain", "Brazil", "Turkey", "Singapore", "Poland", "Belgium",
                          "China", "Norway", "Finland", "Netherlands", "United Kingdom",
                          "New Zealand", "France", "Egypt", "Thailand", "Taiwan", "Australia",
                          "South Korea", "Hong Kong", "Germany", "Sweden", "Chile", "India",
                          "Switzerland", "Canada", "Mexico", "Peru", "South Africa", "Malaysia",
                          "Japan", "Philippines", "Colombia", "Denmark", "USA", "Indonesia"),
                        c("GREK", "ERUS", "EIRL", "ARGT", "EWI", "EWO", "PGAL", "EIS", "EWP",
                          "EWZ", "TUR", "EWS", "EPOL", "EWK", "MCHI", "NORW", "EFNL", "EWN",
                          "EWU", "ENZL", "EWQ", "EGPT", "THD", "EWT", "EWA", "EWY", "EWH", "EWG",
                          "EWD", "ECH", "INDY", "EWL", "EWC", "EWW", "EPU", "EZA", "EWM", "EWJ",
                          "EPHE", "GXG", "EDEN", "VTI", "EIDO"))

colnames(countries) <- c("Country", "Ticker")

# Join & rename
countries <- full_join(countries, capes)
colnames(countries)[4] <- "CAPE"


data <- as.data.frame(NULL)
# Put the return data to a data frame
for(i in 1:nrow(countries)){
  tick <- as.character(countries$Ticker[i])
  temp <- get(getSymbols(tick, from = "2013-09-24"))[, 6]
  # Remove clutter from column name
  names(temp) <- substr(names(temp), 1, gregexpr("\\.", names(temp))[[1]] - 1)
  # Add rownames to column for joining
  temp <- rownames_to_column(as.data.frame(temp))
  assign(tick, temp)
  if(i != 1){
    data <- suppressMessages(full_join(as.data.frame(data), temp))
  } else {
    data <- rbind(data, temp)
  }
}

disp <- function(){
    for(i in 1:nrow(countries)){
      ticker <- as.character(countries$Ticker[i])
      # Get country name & CAPE returns estimate from ticker
      country <- as.character(countries$Country[which(countries$Ticker == ticker)])
      cape <- countries$CAPE[which(countries$Ticker == ticker)]
      return_estimate <- round(-0.075 * log(cape) + 0.2775, 3)
      
      # Find the value of CAPE measurement date
      cape_value <- data[which.min(abs(as.Date(data$rowname) - date_cape)),
                        which(colnames(data) == ticker)]
      
      # Find the levels which to calculate CAPE from
      max_value <- max(get(ticker)[, 2])
      min_value <- min(get(ticker)[, 2])
      # Calculate max 10 values for vertical lines
      pre_values_max <- cumprod(c(cape_value, rep(1.1, log(max_value/cape_value) / log(1.1))))
      pre_values_min <- cumprod(c(cape_value, rep(0.9, log(min_value/cape_value) / log(0.9))))
      # Delete first index value
      pre_values_max <- tail(pre_values_max, -1)
      pre_values_min <- tail(pre_values_min, -1)
      # Calculate max and min CAPEs and corresponding returns
      for(j in 1:10){
        max_cape[j] <- round(cape * 1.1 ^ j, 2)
        min_cape[j] <- round(cape * 0.9 ^ j, 2)
        max_estimate[j] <- round(-0.075 * log(max_cape[j]) + 0.2775, 3)
        min_estimate[j] <- round(-0.075 * log(min_cape[j]) + 0.2775, 3)
      }
      
      # Plot
        gg <- ggplot(data, aes(x = as.Date(rowname), y = get(ticker))) +
          geom_line() +
          # CAPE calulation date vline
          geom_vline(xintercept = date_cape, col = "Gray", size = 1) +
          # Index value at CAPE calculation date hline
          geom_hline(yintercept = cape_value, col = "Gray", size = 1) +
          # Max values hlines
          geom_hline(yintercept = pre_values_max, col = "Red", size = 1) +
          # CAPE and return estimate for the CAPE calculation date
          annotate("text", as.Date("2013-12-31"), cape_value * 1.02,
                   label = paste0(cape, ", ", percent(return_estimate)), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_max[1] * 1.02,
                   label = paste0(max_cape[1], ", ", percent(max_estimate[1])), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_max[2] * 1.02 ^ 1,
                   label = paste0(max_cape[2], ", ", percent(max_estimate[2])), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_max[3] * 1.02,
                   label = paste0(max_cape[3], ", ", percent(max_estimate[3])), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_max[4] * 1.02,
                   label = paste0(max_cape[4], ", ", percent(max_estimate[4])), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_max[5] * 1.02,
                   label = paste0(max_cape[5], ", ", percent(max_estimate[5])), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_min[1] * 1.02,
                   label = paste0(min_cape[1], ", ", percent(min_estimate[1])), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_min[2] * 1.02,
                   label = paste0(min_cape[2], ", ", percent(min_estimate[2])), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_min[3] * 1.02,
                   label = paste0(min_cape[3], ", ", percent(min_estimate[3])), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_min[4] * 1.02,
                   label = paste0(min_cape[4], ", ", percent(min_estimate[4])), color = "Blue") +
          annotate("text", as.Date("2013-12-31"), pre_values_min[5] * 1.02,
                   label = paste0(min_cape[5], ", ", percent(min_estimate[5])), color = "Blue") +
          # Min values hlines
          geom_hline(yintercept = pre_values_min, col = "Green", size = 1) +
          ggtitle(paste0(country, ", ", ticker)) +
          xlab("Date") + ylab("Index")
        print(gg)
        Sys.sleep(0.1)
    }
}