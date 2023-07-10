#SVETA
#PLOTTING THERMAL PERFORMANCE CURVE

#uploaded the done avg actual temps and calculated growth rates to git in raw data
#graphing the temps on x, growth rates on y

#READING IN TABLE
growth_rates_summary_wip <- read_excel("C:/Users/sveta/Documents/B Lab/cross-tolerance/data-raw/growth_rates_summary_wip.xlsx", range = "C1:D7") %>% 
 view()
