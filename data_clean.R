library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(magrittr)
library(BatchGetSymbols)
library(rvest)
library(xts)
library(plotly)
library(scales)
library(zoo)
library(quantmod)
library(qrmdata)
library(stringr)
library(TTR)
library(DT)
library(RColorBrewer)
library(crypto)

data_filepath <- paste(getwd(), "/", sep = "")

first_date <- Sys.Date() - 365
last_date <- Sys.Date()
frequency_1 <- "daily"

 #### PAGE 2 DATA ####
page2_tickers <- c('^GSPC','^DJI')
page2_financial <- BatchGetSymbols(tickers = page2_tickers, 
                                       first.date = first_date,
                                       last.date = last_date, 
                                       freq.data = frequency_1,
                                       cache.folder = file.path(tempdir(), 
                                                                'BGS_Cache')) 

SP_500_prices <- page2_financial$df.tickers %>%
  filter(ticker == '^GSPC') %>%
  select(., ref.date, price.open, price.close, price.high, price.low, price.adjusted) %>%
  rename(date = ref.date, "Opening Price" = price.open, "Closing Price" = price.close, "High Price" = price.high, "Low Price" = price.low,
         "Adjusted Price" = price.adjusted) %>%
  mutate(type_of_data = "prices") %>%
  mutate(date = as.Date(date)) %>%
  gather(., `Opening Price`:`Adjusted Price`, key = "Variable", value = "Value")

DJI_prices <- page2_financial$df.tickers %>%
  filter(ticker == '^DJI') %>%
  select(., ref.date, price.open, price.close, price.high, price.low, price.adjusted) %>%
  rename(date = ref.date, "Opening Price" = price.open, "Closing Price" = price.close, "High Price" = price.high, "Low Price" = price.low,
         "Adjusted Price" = price.adjusted) %>%
  mutate(type_of_data = "prices") %>%
  mutate(date = as.Date(date)) %>%
  gather(., `Opening Price`:`Adjusted Price`, key = "Variable", value = "Value")

#ETF's
ETF_tickers <- c('VCN.TO', 'XUU.TO', 'VEE.TO')

invisible(getSymbols(ETF_tickers, auto.assign = TRUE, warnings = FALSE, from = first_date,
                     to = last_date))

VCN_TO_prices <- VCN.TO %>%
  data.frame(date=index(.)) %>%
  rename(`Opening Price` = VCN.TO.Open, `High Price` = VCN.TO.High, `Low Price` = VCN.TO.Low,
         `Closing Price` = VCN.TO.Close, `Adjusted Price` = VCN.TO.Adjusted) %>%
  select(., date, everything()) %>%
  select(., -VCN.TO.Volume) %>%
  gather(., `Opening Price`:`Adjusted Price`, key = "Variable", value = "Value") %>%
  mutate(type_of_data = "prices") %>%
  mutate(firm = "VCN") %>%
  select(., date, type_of_data, firm, Variable, Value)

VEE_TO_prices <- VEE.TO %>%
  data.frame(date=index(.)) %>%
  rename(`Opening Price` = VEE.TO.Open, `High Price` = VEE.TO.High, `Low Price` = VEE.TO.Low,
         `Closing Price` = VEE.TO.Close, `Adjusted Price` = VEE.TO.Adjusted) %>%
  select(., date, everything()) %>%
  select(., -VEE.TO.Volume) %>%
  gather(., `Opening Price`:`Adjusted Price`, key = "Variable", value = "Value") %>%
  mutate(type_of_data = "prices") %>%
  mutate(firm = "VEE") %>%
  select(., date, type_of_data, firm, Variable, Value)

XUU_TO_prices <- XUU.TO %>%
  data.frame(date=index(.)) %>%
  rename(`Opening Price` = XUU.TO.Open, `High Price` = XUU.TO.High, `Low Price` = XUU.TO.Low,
         `Closing Price` = XUU.TO.Close, `Adjusted Price` = XUU.TO.Adjusted) %>%
  select(., date, everything()) %>%
  select(., -XUU.TO.Volume) %>%
  gather(., `Opening Price`:`Adjusted Price`, key = "Variable", value = "Value") %>%
  mutate(type_of_data = "prices") %>%
  mutate(firm = "XUU") %>%
  select(., date, type_of_data, firm, Variable, Value)

ETF_full <- rbind(VCN_TO_prices, VEE_TO_prices, XUU_TO_prices)

#save data
write_csv(SP_500_prices,paste0(data_filepath,"SP_500_prices.csv"),na="")
write_csv(DJI_prices,paste0(data_filepath,"DJI_prices.csv"),na="")
write_csv(ETF_full,paste0(data_filepath,"ETF_full.csv"),na="")

#### PAGE 3 DATA ####
### dow jones components ###
url <- "https://markets.businessinsider.com/index/components/dow_jones"
dowjones <- url %>%
  read_html() %>%
  html_nodes(., "table") %>%
  .[1] %>%
  html_table(fill = TRUE)

dowjones_components <- as.data.frame(dowjones[[1]]) %>%
  select(., Name) %>%
  mutate(Name = ifelse(Name == "IBM", "International Business Machines",
                       Name)) %>%
  mutate(Name = ifelse(Name == "JPMorgan Chase &", "JP Morgan Chase & Co",
                       Name)) %>%
  mutate(name = paste(Name, "Common Stock"))

#get tickers
dowjones_tickers <- TTR::stockSymbols()[,c('Name', 'Symbol')]

dowjones_tickers <- cbind(dowjones_tickers, clean = gsub(' Incorporated| Corporated| Corporation', '', dowjones_tickers$Name))
dowjones_tickers$clean <- gsub(', Inc|, Inc.| Inc| Inc.| Corp|, Corp| Corp.|, Corp.| Ltd.| Ltd', '', dowjones_tickers$clean)
dowjones_tickers$clean <- gsub('\\(The\\)|[.]|\'|,', '', dowjones_tickers$clean)
dowjones_tickers <- dowjones_tickers %>%
  filter(str_detect(clean, "Common Stock")) %>%
  filter(Name != "Maui Land & Pineapple Company, Inc. Common Stock") %>%
  filter(Name != "Coca-Cola Consolidated, Inc. - Common Stock") %>%
  filter(Name != "Churchill Downs, Incorporated - Common Stock") %>%
  filter(Name != "Goldman Sachs BDC, Inc. Common Stock") %>%
  filter(Name != "Intelligent Systems Corporation Common Stock") %>%
  filter(Name != "Intellicheck, Inc. - Common Stock") %>%
  filter(Name != "Intellia Therapeutics, Inc. - Common Stock") %>%
  filter(Name != "Merck & Company, Inc. Common Stock Ex-Distribution When Issued")

current_dow_jones <- data.frame(name = c(dowjones_components$Name), stringsAsFactors = F)

current_dow_jones$ticker <- sapply(X = current_dow_jones$name, FUN = function(YOUR.NAME) {
  dowjones_tickers[grep(pattern = YOUR.NAME, x = dowjones_tickers$clean, ignore.case = T), 'Symbol'] }) %>%
  unlist()
current_dow_jones <- current_dow_jones %>%
  rename(., Name = name)

dowjones_components <- merge(dowjones_components, current_dow_jones, by = c("Name")) %>%
  select(., -name) %>%
  select(Name, ticker, everything()) %>%
  mutate(ticker = ifelse(Name == "Visa", "V", ticker))

#remove unnecessary datasets
rm(current_dow_jones, dowjones_tickers, VEE.TO, VCN.TO, XUU.TO, dowjones)

#get stock data from current dow jones stocks
dowjones_tickers <- c(dowjones_components$ticker)

stocks_dowjones <- BatchGetSymbols(tickers = dowjones_tickers, 
                                   first.date = first_date,
                                   last.date = last_date, 
                                   freq.data = frequency_1,
                                   cache.folder = file.path(tempdir(), 
                                                            'BGS_Cache')) 

DowJones_components_full <- stocks_dowjones$df.tickers %>%
  select(., ref.date, ticker, price.open, price.close, price.high, price.low, price.adjusted) %>%
  rename(date = ref.date, "Opening Price" = price.open, "Closing Price" = price.close, "High Price" = price.high, "Low Price" = price.low,
         "Adjusted Price" = price.adjusted) %>%
  mutate(date = as.Date(date)) %>%
  gather(., `Opening Price`:`Adjusted Price`, key = "Variable", value = "Value") %>%
  merge(., dowjones_components, by = c("ticker")) %>%
  select(., Name, everything()) %>%
  arrange(., ticker, Variable, date) %>%
  mutate(year = "1Y") %>%
  mutate(ytd = ifelse(date >= mdy(paste(1, 1, year(last_date), sep = "/")), "YTD", NA)) %>%
  mutate(month = ifelse(date >= tail(date, 30), "1M", NA)) %>%
  mutate(fivedays = ifelse(date >= tail(date, 5), "5D", NA)) %>%
  gather(., year:fivedays, key = timeframe, value = "timehorizon") %>%
  select(., -timeframe) %>%
  na.omit()

#save data
write_csv(DowJones_components_full,paste0(data_filepath,"DowJones_components_full.csv"),na="")

#### NASDAQ ####
url_nasdaq <- "https://www.advfn.com/nasdaq/nasdaq.asp"
nasdaq_html <- url_nasdaq %>%
  read_html() %>%
  html_nodes(., "table") %>%
  .[7] %>%
  html_table(fill = TRUE)

nasdaq_components <- as.data.frame(nasdaq_html[[1]]) 

names(nasdaq_components) <- nasdaq_components[2,] %>% 
  as.vector() %>% 
  str_to_lower() %>% 
  str_replace_all(.," ","_")

nasdaq_components <- nasdaq_components[-(1:2),] %>%
  select(., company_name, symbol) %>%
  rename(ticker = symbol)

nasdaq_tickers <- c(nasdaq_components$ticker)

nasdaq_data <- BatchGetSymbols(tickers = nasdaq_tickers, 
                                   first.date = first_date,
                                   last.date = last_date, 
                                   freq.data = frequency_1,
                                   cache.folder = file.path(tempdir(), 
                                                            'BGS_Cache'))

nasdaq_data_clean <- nasdaq_data$df.tickers %>%
  mutate(date = as.Date(ref.date)) %>%
  select(date, ticker, price.open, price.high, price.low, price.close, price.adjusted, volume)

nasdaq_data_full <- merge(nasdaq_data_clean, nasdaq_components, by = c("ticker"))

write_csv(nasdaq_data_full,paste0(data_filepath,"nasdaq_data.csv"),na="")

#### CRYPTOCURRENCY ####
#link = https://coinmarketcap.com/


#### S&P 500 DATA ####
# allstocks <- GetSP500Stocks() %>%
#   rename(Ticker = Tickers)
# allstocks_tickers <- allstocks$Ticker
# 
# allstocks_financial <- BatchGetSymbols(tickers = allstocks_tickers, 
#                                        first.date = first_date,
#                                        last.date = last_date, 
#                                        freq.data = frequency_1,
#                                        cache.folder = file.path(tempdir(), 
#                                                                 'BGS_Cache')) 
# 
# all_stocks <- allstocks_financial$df.tickers %>%
#   select(., ref.date, ticker, price.open, price.close, price.high, price.low, price.adjusted) %>%
#   rename(date = ref.date, "Opening Price" = price.open, "Closing Price" = price.close, "High Price" = price.high, "Low Price" = price.low,
#          "Adjusted Price" = price.adjusted, Ticker = ticker) %>%
#   mutate(date = as.Date(date)) %>%
#   merge(., allstocks, by = c("Ticker")) %>%
#   select(., -SEC.filings, -Date.First.Added, -CIK) %>%
#   gather(., `Opening Price`:`Adjusted Price`, key = "Variable", value = "Value")
# 
# write_csv(all_stocks,paste0(data_filepath,"all_stocks.csv"),na="")
