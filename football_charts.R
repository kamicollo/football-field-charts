library(tidyverse)
library(data.table)
library(RColorBrewer)

locations = fread("~/coding/real-estate/locations.csv")
customers = fread("~/coding/real-estate/customers.csv")
contracts = fread("~/coding/real-estate/contracts.csv")
terms = fread("~/coding/real-estate/terms.csv")
PL = fread("~/coding/real-estate/monthly_data.csv") %>% 
  mutate(date = as.Date(strptime(paste0(month,"-01"), format="%Y-%m-%d"))) %>%
  mutate(year = year(date), month = month(date))

monthly = PL %>%
  group_by(date) %>% 
  summarize(rent = sum(rent), sqm = sum(sqm) * sum(effective_days) / sum(days_in_month), .groups="drop_last") %>%
  mutate(rent_per_sqm = rent / sqm)
  
yearly = PL %>%
  group_by(year) %>% 
    summarize(rent = sum(rent), sqm = sum(sqm) * sum(effective_days) / sum(days_in_month), .groups="drop_last") %>%
    mutate(rent_per_sqm = rent / sqm)

monthly %>%  ggplot() + geom_line(aes(x = date, y=rent_per_sqm))
yearly %>%  ggplot() + geom_line(aes(x = year, y=rent_per_sqm))  + scale_y_continuous(sec.axis = ~ .*100000) + geom_col(aes(x=year, y=sqm/100000))

future_value = PL %>% filter(date > "2020-12-31") %>% group_by(term_id) %>% summarize(rent = sum(rent), .groups='drop_last') %>%
  inner_join(terms, by='term_id') %>%
  inner_join(contracts, by="contract_id") %>% group_by(contract_id) %>% summarize(future_value = sum(rent), .groups='drop_last')

top_10_contracts = future_value %>% arrange(desc(future_value)) %>% head(10)

for_chart = terms %>%
  inner_join(top_10_contracts, by="contract_id") %>%
  mutate(width = as.numeric(difftime(end, start, units="days"))) %>% mutate(c = start + width/2) %>%
  mutate(extension = as.factor(extension)) %>%
  mutate(contract_id = reorder(as.character(contract_id), future_value, mean))

labels = for_chart %>% group_by(contract_id) %>% 
  summarize(end_date = max(end), value = max(future_value), .groups="drop_last") %>%
  mutate(label = paste0(round(value / 1000000, 1), "M"))

for_chart %>%
ggplot() + geom_tile(aes(x=c, y=contract_id, width=width, height=monthly_rent/max(monthly_rent), fill=extension)) + 
  ylab("") + xlab("Year") + theme_bw() + 
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  geom_label(data = labels, mapping=aes(end_date + 400, contract_id, label=label)) + 
  scale_fill_brewer(palette = "RdYlGn")


