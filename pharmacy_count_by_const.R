# install.packages("x")
library(readr)
library(dplyr)
library(lubridate)
library (tidyr)

# add column headings
headers <- c("code_1", 
             "pharmacy", 
             "code_2", 
             "code_3",
             "address_1",
             "address_2",
             "address_3",
             "address_4",
             "address_5",
             "post_code",
             "date_opened",
             "date_closed",
             "A=open",
             "1=pharmacy",
             "code_4",
             "code_5",
             "code_6",
             "phone_number",
             "code_7",
             "code_8",
             "code_9",
             "code_10",
             "code_11",
             "code_12")

# clean raw NHS data
pharm <- read_csv("nhs_data/edispensary.csv", col_names = headers) |> 
  filter(`1=pharmacy`== 1) |>
  mutate(
    date_opened = ymd(date_opened), 
    date_closed = ifelse(is.na(date_closed), "2035-12-31", as.character(date_closed)),
    date_closed = ymd(date_closed) 
  )

# select required columns
pc <- read_csv("geography_data/postcode_to_new_constituency_UK.csv") |> 
  select(pcds, `New constituency`)

# join by postcode
pharm_to_const <- left_join(pharm, pc, by = c("post_code" = "pcds"))

# years of interest
years <- ymd(c(20160901, 
               20170901, 
               20180901, 
               20190901, 
               20200901, 
               20210901, 
               20220901, 
               20230901, 
               20240901, 
               20250101))

# counting open pharmacies function
calculate_open_pharmacies <- function(data, year) {
  data |> 
    filter(date_opened <= year & date_closed > year) |>
    group_by(`New constituency`) |>
    summarise(open_pharmacies = n(), .groups = "drop") |>
    mutate(date = as.Date(year))
}

# calling the above function on our dataset for each element (year) in years
results <- lapply(years, function(year) calculate_open_pharmacies(pharm_to_const, year)) |> 
  bind_rows() |> 
  mutate(date = format(as.Date(date), "Number of pharmacies in %B %Y")) |>   
  pivot_wider(
    names_from = date, 
    values_from = open_pharmacies
  ) |> 
  rename(Constituency = `New constituency`)

# export results
if (!dir.exists("output_data")) {
  dir.create("output_data")
}

write.csv(results, "output_data/open_pharmacies_by_constituency.csv", row.names = FALSE)

rows_with_na <- pharm_to_const %>%
  filter(is.na(`New constituency`))


