# Description ------------------------------------------------------------------

# For house hunters who do not want to pay "too much" according to price per
# square foot. PPSF varies based on *many* factors, so we reduce real-world
# complexity to PPSF's relationship with beds and baths, for homes in price
# range and region of interest.

# The script pulls five years of Redfin data on SOLD PRICE. Plots are average
# PPSF over time, by beds and baths. Even includes confidence intervals for each
# crossing. The script makes plots to help understand the home buying market and
# the way it has trended. The plots help rationalize a final offer.

# Add more elements to the region_id vector to query and plot more data.
# Currently region_id includes "26781" for Willow Grove and "9641" for
# Jenkintown. To find more region_id, visit redfin.com and enter a new city of
# interest into the search bar. The resulting URL will contain the new
# region_id. Example, https://www.redfin.com/city/9641/PA/Jenkintown contains
# 9641.

# Libraries --------------------------------------------------------------------

library(tidyverse)

# Query Paramneters ------------------------------------------------------------

# "26781" is willow grove 
# "9641" is jenkintown
# ex. https://www.redfin.com/city/9641/PA/Jenkintown
# ex. https://www.redfin.com/city/26781/PA/Willow-Grove
region_id <- c("7735")

# 1 neighborhood
# 2 is zip
# 5 county
# 6 is city
region_type <- 6

# "" = any
# 1 = active
# 130 = pending
# 131 = pending + active
status <- "9"

# 1 house
# 2 townhouse
# 3 condo
# 4 multi-family
# 5 land
# 6 other
property_type <- "1,2,3"

# Price Range
# DEFAULT: (400000 - 540000)
price_base <- 400000
price_ranges <- 20000
price_increments <- 0:6
min_price <- price_base + price_increments*price_ranges
max_price <- price_base + (price_ranges) + price_increments*price_ranges - 1

# Baths Range
num_baths <- 2
max_num_baths <- 6

# Beds Range
num_beds <- 3
max_num_beds <- 6

# Options: 30, 90, 365, 1095 (3yr), 1825 (5yr)
sold_within_days <- "1825"

# redfin_00: original data -----------------------------------------------------

redfin_00 <- readr::read_csv(
  paste0("https://www.redfin.com/stingray/api/gis-csv?al=1",
         "&has_short_term_lease=false",
         "&isRentals=false",
         "&is_furnished=false",
         # "&market=dc",
         "&num_homes=350",
         "&ord=redfin-recommended-asc",
         "&page_number=1",
         "&status=9",
         "&sold_within_days=", sold_within_days,
         "&uipt=", property_type,
         "&region_id=", region_id,
         "&region_type=", region_type,
         "&min_price=", min_price,
         "&max_price=", max_price,
         "&num_baths=", num_baths,
         "&max_num_baths=", max_num_baths,
         "&num_beds=", num_baths,
         "&max_num_beds=", max_num_baths,
         "&v=8")
)

redfin_00

# redfin_01: distinct + clean names --------------------------------------------

redfin_01 <- redfin_00 %>% 
  distinct() %>% 
  janitor::clean_names()

# redfin_02: preprocess the data -----------------------------------------------

redfin_02 <- redfin_01 %>% 
  # Drops rows without a sold date. 
  filter(!is.na(sold_date)) %>% 
  # Drop unwanted columns. 
  select(-days_on_market, -next_open_house_start_time, -next_open_house_end_time) %>% 
  select(-sale_type, -city, -state_or_province, -status, -source, -favorite, -interested) %>% 
  select(-zip_or_postal_code) %>% 
  # Rename column. 
  rename(url = url_see_https_www_redfin_com_buy_a_home_comparative_market_analysis_for_info_on_pricing) %>% 
  # All character-type columns to upper case. 
  mutate(across(where(is.character), str_to_upper)) %>% 
  # Remove rows where property_type is condos/co-op. 
  filter(property_type != "CONDO/CO-OP") %>% 
  # Extract url id from url. 
  mutate(url_id = as.numeric(str_remove(str_extract(url, "\\/[^\\/]+$"), "\\/"))) %>% 
  # Extract and prep sold date components from sold date. 
  mutate(sold_date = lubridate::mdy(sold_date)) %>% 
  mutate(sold_year = lubridate::year(sold_date)) %>% 
  mutate(sold_month = lubridate::month(sold_date)) %>% 
  mutate(sold_quarter = ceiling(sold_month / 3)) %>% 
  mutate(sold_day = lubridate::day(sold_date)) %>% 
  # Extract ID. 
  mutate(mdfr_nbr = as.numeric(str_remove(mls_number, "MDFR"))) %>% 
  # Replace an NA with "UNKNOWN" where column is character-type. 
  mutate(across(where(is.character), ~ifelse(is.na(.x), "UNKNOWN", .x))) %>% 
  # Sold date to numeric. 
  mutate(sold_date = as.numeric(sold_date)) %>% 
  # Does house have HOA. 
  mutate(hoa_month_bool = ifelse(is.na(hoa_month), 0, 1)) %>% 
  mutate(hoa_month = ifelse(is.na(hoa_month), 0, hoa_month)) %>% 
  # Drop unwanted columns. 
  select(-url, -mls_number, -address) %>% 
  # Calculate price per square foot. 
  mutate(ppsf = price / square_feet) %>% 
  # Re-scale literal longitude and latitude into relative longitude and latitude. 
  mutate(longitude = longitude - min(longitude)) %>% 
  mutate(longitude = (longitude / max(longitude)) * 100) %>% 
  mutate(latitude = latitude - min(latitude)) %>% 
  mutate(latitude = (latitude / max(latitude)) * 100) %>% 
  # Drops rows where price or sqft contain NA. 
  filter(!is.na(price)) %>% 
  filter(!is.na(square_feet)) %>%
  # Create a column concatenating beds and baths. 
  mutate(beds_baths = paste0(beds, "_",baths)) %>%
  # Convert to factor-type. 
  mutate(beds = factor(beds)) %>%
  mutate(baths = factor(baths)) 

# plotdata_A -------------------------------------------------------------------

plotdata_A <- redfin_02 %>% 
  # Subset columns. 
  select(ppsf, sold_year, beds, price, square_feet) %>%
  # For distributions. 
  mutate(beds_year = paste0(sold_year, "_", beds)) %>%
  # Summary stats. 
  group_by(sold_year, beds) %>%
  mutate(
    lb = quantile(ppsf, probs = 0.10, na.rm = TRUE), 
    ub = quantile(ppsf, probs = 0.90, na.rm = TRUE), 
    ppsf_avg = sum(price) / sum(square_feet), 
    n = n()
  ) %>%
  ungroup() 

plotdata_A %>% 
  ggplot(aes(x = sold_year)) +
  geom_line(aes(y = ppsf_avg, group = beds, color = beds)) +
  geom_errorbar(aes(y = ppsf, 
                    group = beds_year, 
                    color = beds, 
                    ymin = lb, 
                    ymax = ub), 
                width = 0.15, 
                position = "dodge") +
  geom_label(aes(y = ppsf_avg, 
                 group = beds, 
                 color = beds, 
                 label = n), 
             show.legend = FALSE, 
             size = 1.9, 
             position = position_dodge(width = 0.15)) + 
  labs(title = "Mean with CI over time by Beds") +
  theme_linedraw() +
  ylim(c(100, 400))

# plotdata_B -------------------------------------------------------------------

plotdata_B <- redfin_02 %>% 
  # Subset columns. 
  select(ppsf, sold_year, beds_baths, beds, baths, price, square_feet) %>%
  # For distributions. 
  mutate(bby = paste0(sold_year, "_", beds, "_", baths)) %>%
  # Summary stats. 
  group_by(sold_year, beds, baths) %>%
  mutate(
    lb = quantile(ppsf, probs = 0.10, na.rm = TRUE), 
    ub = quantile(ppsf, probs = 0.90, na.rm = TRUE), 
    ppsf_avg = sum(price) / sum(square_feet), 
    n = n()
  ) %>%
  ungroup() %>%
  # For dropping empty crossings.  
  group_by(beds, baths) %>%
  mutate(nyr = n_distinct(sold_year)) %>%
  ungroup() %>%
  filter(nyr >= (max(nyr) - 1))

plotdata_B %>% 
  ggplot(aes(x = sold_year)) +
  geom_line(aes(y = ppsf_avg, group = baths, color = baths)) +
  geom_errorbar(aes(y = ppsf, 
                    group = baths, 
                    color = baths, 
                    ymin = lb, 
                    ymax = ub), 
                width = 0.15, 
                position = "dodge") +
  geom_label(aes(y = ppsf_avg, 
                 group = baths, 
                 color = baths, 
                 label = n), 
             show.legend = FALSE, 
             size = 1.9, 
             position = position_dodge(width = 0.15)) + 
  labs(title = "Mean with CI over time by Beds and Baths") +
  facet_wrap(vars(factor(beds))) +
  theme_linedraw() +
  ylim(c(100, 400))

# Write csvs -------------------------------------------------------------------

write_csv(redfin_00, "redfin_00.csv")
write_csv(redfin_01, "redfin_01.csv")
write_csv(redfin_02, "redfin_02.csv")
write_csv(plotdata_A, "plotdata_A.csv")
write_csv(plotdata_B, "plotdata_B.csv")


