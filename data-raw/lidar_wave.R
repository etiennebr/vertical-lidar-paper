## code to prepare `wave` data (vertical lidar return distribution)

wave <- readr::read_csv("data-raw/lidar_wave.csv")

usethis::use_data(wave)
