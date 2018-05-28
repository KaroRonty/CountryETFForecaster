library(quantmod)
library(tools)

# Function to plot countries and naive forecasts for ETFs using CAPE (Shiller P/E)
cntr <- function(value = 0, lastcountry = "Indonesia") {
	# Suppress unnecessary warnings
    options(warn = -1)
    options(getSymbols.yahoo.warning = FALSE)
    options(getSymbols.warning4.0 = FALSE)
	
    country <- "placeholder"
    
    plotvalues <- function(tick, country, cape, day, trueOrFalse) {
        getSymbols(tick, from = "2013-09-24")
		
		# Put index data and dates into separate columns
        x <- na.approx(data.matrix(as.data.frame(get(tick))))
        t <- as.Date(data.matrix(as.data.frame(index(get(tick)))))
        
		# Plot adjusted close
        plot(t, x[, 6], type = "l", main = paste(toTitleCase(country), tick, sep = ", "), xlab = "", 
            ylab = "")
		# Add CAPE calculation date to the plot as a red circle
        points(day, x[match(day, t), 6], pch = "O", cex = 1.5, col = "red")
        
        if (trueOrFalse == 0) {
		# Add horizontal lines indicating if the index was below or above current level
		# Calculate CAPEs and naive forecasts for each index level
            for (y in seq(3, 0.1, by = -0.1)) {
                if ((x[match(day, t), 6] * y) == (x[match(day, t), 6])) {
                  abline(h = x[match(day, t), 6], col = "dimgray")
                  text(mean.Date(c(as.Date("2013-09-24"), Sys.Date())), x[match(day, t), 6], format(paste(cape * 
                    y, "; ", format((-0.075 * log(cape * y) + 0.2775) * 100, digits = 2)), digits = 3))
                } else if (((x[match(day, t), 6] * y) < max(x[, 6])) && ((x[match(day, t), 6] * y) > x[match(day, 
                  t), 6])) {
                  abline(h = x[match(day, t), 6] * y, col = "red")
                  text(mean.Date(c(as.Date("2013-09-24"), Sys.Date())), x[match(day, t), 6] * y, format(paste(cape * 
                    y, "; ", format((-0.075 * log(cape * y) + 0.2775) * 100, digits = 3)), digits = 2))
                } else if (((x[match(day, t), 6] * y) > min(x[, 6])) && ((x[match(day, t), 6] * y) < x[match(day, 
                  t), 6])) {
                  abline(h = x[match(day, t), 6] * y, col = "green")
                  text(mean.Date(c(as.Date("2013-09-24"), Sys.Date())), x[match(day, t), 6] * y, format(paste(cape * 
                    y, "; ", format((-0.075 * log(cape * y) + 0.2775) * 100, digits = 3)), digits = 2))
                }
            }
        } else {
            abline(h = x[match(day, t), 6], col = "dimgray")
        }
        
        if (value > 0) {
			# To prevent errors
            Sys.sleep(value - 1)
        }
    }
    
    if (value >= 1) {
        i <- 0
		# capes.csv must contain country names, ETF tickers and CAPEs for each country
        csv <- read.csv("capes.csv", header = FALSE, colClasses = c("character", "character", "numeric"), 
            fileEncoding = "UTF-8-BOM")
        countries <- csv[[1]]
        ticks <- csv[[2]]
        capes <- csv[[3]]
        for (i in seq(1, match(toTitleCase(lastcountry), countries), by = 1)) {
            tick <- ticks[i]
            country <- countries[i]
            cape <- capes[i]
			# Hardcoded date when the CAPEs were calculated
            day <- as.Date("2018-03-29")
            if (capes[i] == 0) {
                trueOrFalse <- 1
            } else {
                trueOrFalse <- 0
            }
            plotvalues(tick, country, cape, day, trueOrFalse)
        }
    } else {
        country <- toTitleCase(readline(prompt = "Country: "))
        cape <- as.numeric(readline(prompt = "CAPE: "))
        day <- as.Date(readline(prompt = "Date: "))
        trueOrFalse <- 0
        tick <- switch(country, Fi = "EFNL", Br = "EWZ", Ru = "ERUS", No = "ENOR", Kr = "EWY", Pl = "EPOL", 
            Sg = "EWS", It = "EWI", Cn = "MCHI", Greece = "GREK", Russia = "ERUS", Ireland = "EIRL", 
            Argentina = "ARGT", Italy = "EWI", Austria = "EWO", Portugal = "PGAL", Israel = "EIS", Spain = "EWP", 
            Brazil = "EWZ", Turkey = "TUR", Singapore = "EWS", Poland = "EPOL", Belgium = "EWK", China = "MCHI", 
            Norway = "NORW", Finland = "EFNL", Netherlands = "EWN", `United Kingdom` = "EWU", `New Zealand` = "ENZL", 
            France = "EWQ", Egypt = "EGPT", Thailand = "THD", Taiwan = "EWT", Australia = "EWA", `South Korea` = "EWY", 
            `Hong Kong` = "EWH", Germany = "EWG", Sweden = "EWD", Chile = "EWC", India = "INDY", Switzerland = "EWL", 
            Canada = "EWC", Mexico = "EWW", Peru = "EPU", `South Africa` = "EZA", Malaysia = "EWM", Japan = "EWJ", 
            Philippines = "EPHE", Colombia = "GXG", Denmark = "EDEN", USA = "VTI", Indonesia = "EIDO")
        plotvalues(tick, country, cape, day, trueOrFalse)
    }
    
}
