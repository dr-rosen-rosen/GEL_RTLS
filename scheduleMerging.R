# combining rotation spreadsheets

library(here)
library(tidyverse)

workbooks <- list.files(
  path = "/Users/mrosen44/Johns\ Hopkins/Amanda\ Bertram\ -\ Resident\ Schedules",
  pattern = "*.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

df <- data.frame(
  date = POSIXct(),
  service = character(),
  name = character(),
  pgy = character(),
  site = character()
)

for (workbook in workbooks) {
  print(workbook)
  # pgy <- as.integer(str_match(workbook, "PGY(.*?).xlsx")[2])
  pgy <- str_match(workbook,"[Hopkins|Bayview] - (.*?).xlsx")[2]
  if (grepl('Bayview', workbook, fixed = TRUE)) {
    site <- 'Bayview'
  } else { site <- 'Hopkins'}
  print(pgy)
  print(site)
  sheets <- readxl::excel_sheets(workbook)
  for (sheet in sheets) {
    # print(sheet)
    one_sheet <- readxl::read_excel(
      path = workbook,
      sheet = sheet) %>%
      janitor::clean_names() %>%
      dplyr::select(date, service)
    one_sheet$name <- sheet
    one_sheet$site <- site
    one_sheet$pgy <- pgy
    # print(sheet)
    # print(sum(is.na(one_sheet$site)))
    df <- bind_rows(df, one_sheet)
  }
}
skimr::skim(df)
write_csv(df, 'mergedRotations.csv')
