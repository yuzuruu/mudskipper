###########################################################
# Mudskippers' migration 
# 10th. January 2023
# Yuzuru Utsunomiya, Ph. D.
# (Faculty of Economics, Nagaski University)
###########################################################
# 
# NOTE
# We provided data and related codes on Github. 
# (https://github.com/yuzuruu/mudskipper)
# 
# read requisite library
# ---- read.library ----
library(tidyverse)
library(khroma)
library(cmdstanr)
library(summarytools)
library(tableone)
# 
# ---- read.data ----
# read migration data
# Before reading the data, place the data into our working directory.
mudskipper_data <- 
  readxl::read_excel(
    "mudskipper.xlsx",
    sheet = "fig_6",
    col_names = TRUE
    ) %>% 
  # reshape the data tidy
  tidyr::pivot_longer(
    cols = c(land, water, shore),
    names_to = "position",
    values_to = "proportion"
  ) %>% 
  # transform data type from character to factor
  dplyr::mutate(
    across(
      where(is.character),  
      factor
      )
    ) %>% 
  # fill NA into missing values for analysis below
  tidyr::complete(
    Days_after_hatch, 
    nesting(
      group, 
      individual
      ), 
    position
    ) %>% 
  # add a variable for analysis
  # STAN does not accept any character-type data. Instead, we set the position using number.
  dplyr::mutate(
    position_number = factor(
      dplyr::case_when(
        position == "land" ~ "1",
        position == "shore" ~ "2",
        position == "water" ~ "3",
        TRUE ~ "hoge"
        )
      )
    )
# 
# ---- descriptive.statistics ----
# Make a descriptive statistics table
mudskipper_data_summary <- 
  readxl::read_excel(
    "mudskipper.xlsx",
    sheet = "fig_6",
    col_names = TRUE
  ) %>% 
  tidyr::pivot_longer(
    cols = c(land, water, shore),
    names_to = "position",
    values_to = "proportion"
  ) %>% 
  dplyr::mutate(
    group = factor(group),
    individual = factor(individual),
    position = factor(position),
    proportion = as.numeric(proportion)
  ) %>% 
  dplyr::group_by(position, individual) %>% 
  # summarise the proportion
  # NOTE
  # We need to omit the NA and added na.rm = TRUE.
  dplyr::summarise(
    N = n(),
    Min. = min(proportion, na.rm = TRUE),
    Mean = mean(proportion, na.rm = TRUE),
    Median = median(proportion, na.rm = TRUE),
    Max. = max(proportion, na.rm = TRUE),
    SD = sd(proportion, na.rm = TRUE)
  )
# save the table
readr::write_excel_csv(
  mudskipper_data_summary,
  "mudskipper_data_summary.csv"
)
# 
# ---- line.plot ----
mudskipper_line <- 
  mudskipper_data %>% 
  ggplot2::ggplot(
    aes(
      x = Days_after_hatch,
      y = proportion,
      group = interaction(group, individual),
      shape = group,
      colour = individual
    )
  ) +
  geom_line() +
  geom_point() +
  scale_color_okabeito() +
  facet_wrap(~ individual + position) + 
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank()
  )
# save the line plot
ggsave(
  "mudskipper_line_individual.pdf", 
  plot = mudskipper_line, 
  width = 300, 
  height = 300, 
  units = "mm"
  )
# 
# ---- data.for.analysis.land ------
# make a data frame including results in LAND
mudskipper_table_land <- 
  mudskipper_data %>% 
  # select data in LAND
  dplyr::filter(position == "land") %>%
  dplyr::select(-group, -position_number) %>% 
  tidyr::pivot_wider(
    names_from = c(individual, position),
    values_from = proportion
  ) %>% 
  dplyr::select(-Days_after_hatch) %>% 
  data.table::setnames(
    c(
      "070708-1_land",    
      "070708-2_land", 
      "070708-3_land",    
      "070618-1_land",    
      "070618-2_land",   
      "070618-3_land",    
      "070525-1_land",    
      "070525-2_land"    
      )
    ) 
mudskipper_table_t_land <- 
  mudskipper_table_land %>% 
  t(.)
Y_land <- mudskipper_table_t_land
# 
# make data suitable for STAN computation
# 
ypos_land <- 
  Y_land[!is.na(Y_land)]
n_pos_land <- 
  length(ypos_land)  # number on non-NA
indx_pos_land <- 
  which(
    !is.na(Y_land), 
    arr.ind = TRUE
    )  # index on the non-NAs
col_indx_pos_land <- 
  as.vector(indx_pos_land[, "col"])
row_indx_pos_land <- 
  as.vector(indx_pos_land[, "row"])

# ---- data.for.analysis.shore ------
mudskipper_table_shore <- 
  mudskipper_data %>% 
  dplyr::filter(position == "shore") %>%
  dplyr::select(-group, -position_number) %>% 
  tidyr::pivot_wider(
    names_from = c(individual, position),
    values_from = proportion
  ) %>% 
  dplyr::select(-Days_after_hatch) %>% 
  data.table::setnames(
    c(
      "070708-1_shore",
      "070708-2_shore",
      "070708-3_shore",
      "070618-1_shore",
      "070618-2_shore",
      "070618-3_shore",
      "070525-1_shore",
      "070525-2_shore"
    )
  ) 
mudskipper_table_t_shore <- mudskipper_table_shore %>% t(.)
Y_shore <- mudskipper_table_t_shore
# 
# make data suitable for STAN computation
ypos_shore <- 
  Y_shore[!is.na(Y_shore)]
n_pos_shore <- 
  length(ypos_shore)  # number on non-NA
indx_pos_shore <- 
  which(
    !is.na(Y_shore), 
    arr.ind = TRUE
    )  # index on the non-NAs
col_indx_pos_shore <- 
  as.vector(indx_pos_shore[, "col"])
row_indx_pos_shore <- 
  as.vector(indx_pos_shore[, "row"])
# 
# ---- data.for.analysis.water ------
mudskipper_table_water <- 
  mudskipper_data %>% 
  dplyr::filter(position == "water") %>%
  dplyr::select(-group, -position_number) %>% 
  tidyr::pivot_wider(
    names_from = c(individual, position),
    values_from = proportion
  ) %>% 
  dplyr::select(-Days_after_hatch) %>% 
  data.table::setnames(
    c(
      "070708-1_water" ,
      "070708-2_water",
      "070708-3_water",
      "070618-1_water",
      "070618-2_water",
      "070618-3_water",
      "070525-1_water",
      "070525-2_water"
    )
  ) 
mudskipper_table_t_water <- mudskipper_table_water %>% t(.)
Y_water <- mudskipper_table_t_water
# 
# make data suitable for STAN computation
# 
ypos_water <- Y_water[!is.na(Y_water)]
n_pos_water <- length(ypos_water)  # number on non-NA
indx_pos_water <- which(!is.na(Y_water), arr.ind = TRUE)  # index on the non-NAs
col_indx_pos_water <- as.vector(indx_pos_water[, "col"])
row_indx_pos_water <- as.vector(indx_pos_water[, "row"])
# 
# ---- kick.stan ----
# compile stan code
# We use cmdstanr instead of rstan. In terms of computation period,
# the cmdstanr is advantageous.
mod <-
  cmdstanr::cmdstan_model(
    "mudskipper_ssm_01.stan"
    )
# kick the stan code
# WARNING
# This process needs long computation period.
# Beware and comment out when not in use.
fit_land <-
  mod$sample(
    data = list(
      y = ypos_land,
      TT = ncol(Y_land),
      N = nrow(Y_land),
      n_position = n_pos_land,
      col_index_position = col_indx_pos_land,
      row_index_position = row_indx_pos_land
      ),
    seed = 123,
    chains = 4,
    iter_warmup = 200000,
    iter_sampling = 200000,
    parallel_chains = 4,
    refresh = 1000 # print update every 500 iters
)
# save the sampling results
fit_land$save_object(file = "fit_land.rds")
# read the sampling results
fit_land <- readRDS("fit_land.rds")
readr::write_excel_csv(
  fit_land$summary(), 
  "fit_land_summary.csv"
  )
fit_land_summary <- 
  readr::read_csv(
    "fit_land_summary.csv"
    )
# 
# shore
fit_shore <-
  mod$sample(
    data = list(
      y = ypos_shore,
      TT = ncol(Y_shore),
      N = nrow(Y_shore),
      n_position = n_pos_shore,
      col_index_position = col_indx_pos_shore,
      row_index_position = row_indx_pos_shore
    ),
    seed = 123,
    chains = 4,
    iter_warmup = 200000,
    iter_sampling = 200000,
    parallel_chains = 4,
    refresh = 5000 # print update every 500 iters
  )
# save the sampling results
fit_shore$save_object(file = "fit_shore.rds")
# read the sampling results
fit_shore <- readRDS("fit_shore.rds")
readr::write_excel_csv(
  fit_shore$summary(), 
  "fit_shore_summary.csv"
)
fit_shore_summary <- 
  readr::read_csv(
    "fit_shore_summary.csv"
  )
# water
fit_water <-
  mod$sample(
    data = list(
      y = ypos_water,
      TT = ncol(Y_water),
      N = nrow(Y_water),
      n_position = n_pos_water,
      col_index_position = col_indx_pos_water,
      row_index_position = row_indx_pos_water
    ),
    seed = 123,
    chains = 4,
    iter_warmup = 200000,
    iter_sampling = 200000,
    parallel_chains = 4,
    refresh = 5000 # print update every 500 iters
  )
# save the sampling results
fit_water$save_object(file = "fit_water.rds")
# read the sampling results
fit_water <- readRDS("fit_water.rds")
readr::write_excel_csv(
  fit_water$summary(), 
  "fit_water_summary.csv"
)
fit_water_summary <- 
  readr::read_csv(
    "fit_water_summary.csv"
  )
# 
# ----- line.results ------

fit_land_summary <- readr::read_csv("fit_land_summary.csv")
fit_shore_summary <- readr::read_csv("fit_shore_summary.csv")
fit_water_summary <- readr::read_csv("fit_water_summary.csv")



# land
# overlap observed number and estimated values 
# yhat
# yhat denotes estimated value of Y
data_yhat_land <- 
  fit_land_summary %>% 
  # pick up the yhat
  dplyr::filter(
    stringr::str_detect(
      variable, 
      "yhat"
      ) 
  ) %>% 
  # 
  dplyr::mutate(
    # parameter's name
    parameter = str_extract(variable, "(.+)(?=\\[)"),
    # days after hatch
    Days_after_hatch = as.numeric(str_extract(variable, "(?<=[:punct:])(.+)(?=\\,)")),
    # ID by individual (toll tank)
    individual = as.numeric(str_extract(variable, "(?<=,)(.+)(?=\\])"))
    ) %>% 
  # transform the number into group ID
  dplyr::mutate(
    individual = 
      factor(
        dplyr::case_when(
          individual == 1 ~ "070708-1",
          individual == 2 ~ "070708-2",
          individual == 3 ~ "070708-3",
          individual == 4 ~ "070618-1",
          individual == 5 ~ "070618-2",
          individual == 6 ~ "070618-3",
          individual == 7 ~ "070525-1",
          individual == 8 ~ "070525-2",
          TRUE ~ "hoge"
          )
        ),
    # for convenience to indicate x axis
    Days_after_hatch = Days_after_hatch+27
    )

# Y
data_Y_land <- 
  mudskipper_data %>% 
  dplyr::filter(position == "land") %>%
  dplyr::select(-group, -position) 

data_yhat_y_land <- 
  data_yhat_land %>% 
  dplyr::left_join(
    data_Y_land,
    by = c("individual", "Days_after_hatch")
  )

# 作図
line_data_yhat_y_land <- 
  data_yhat_y_land %>% 
  ggplot2::ggplot(
    aes(
      x = Days_after_hatch,
      y = proportion,
      color = individual
    )
  ) +
  # geom_ribbon(
  #   aes(
  #     ymin = q5,
  #     ymax = q95,
  #   ),
  #   fill = "grey",
  #   colour = "transparent"
  # ) +
  geom_line() +
  geom_point(
    aes(
      x = Days_after_hatch,
      y = mean,
      color = individual,
      fill = individual
    ),
    shape = 21 # circle
  ) +
  labs(
    x = "Days after hatch (28 - 43 days)",
    y = "Proportion to be on land (Unit: %)",
    title = "Land",
    subtitle = ""
  ) +
  scale_color_okabeito() +
  scale_fill_okabeito() +
  # facet_wrap(~ individual, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank()
  )

ggsave(
  "line_data_yhat_y_land.pdf",
  plot = line_data_yhat_y_land,
  height = 200,
  width = 200,
  units = "mm"
)



# water
# overlap observed number and estimated values 
# yhat
# yhat denotes estimated value of Y
data_yhat_water <- 
  fit_water_summary %>% 
  # pick up the yhat
  dplyr::filter(
    stringr::str_detect(
      variable, 
      "yhat"
    ) 
  ) %>% 
  # 
  dplyr::mutate(
    # parameter's name
    parameter = str_extract(variable, "(.+)(?=\\[)"),
    # days after hatch
    Days_after_hatch = as.numeric(str_extract(variable, "(?<=[:punct:])(.+)(?=\\,)")),
    # ID by individual (toll tank)
    individual = as.numeric(str_extract(variable, "(?<=,)(.+)(?=\\])"))
  ) %>% 
  # transform the number into group ID
  dplyr::mutate(
    individual = 
      factor(
        dplyr::case_when(
          individual == 1 ~ "070708-1",
          individual == 2 ~ "070708-2",
          individual == 3 ~ "070708-3",
          individual == 4 ~ "070618-1",
          individual == 5 ~ "070618-2",
          individual == 6 ~ "070618-3",
          individual == 7 ~ "070525-1",
          individual == 8 ~ "070525-2",
          TRUE ~ "hoge"
        )
      ),
    # for convenience to indicate x axis
    Days_after_hatch = Days_after_hatch+27
  )

# Y
data_Y_water <- 
  mudskipper_data %>% 
  dplyr::filter(position == "water") %>%
  dplyr::select(-group, -position) 

data_yhat_y_water <- 
  data_yhat_water %>% 
  dplyr::left_join(
    data_Y_water,
    by = c("individual", "Days_after_hatch")
  )

# 作図
line_data_yhat_y_water <- 
  data_yhat_y_water %>% 
  ggplot2::ggplot(
    aes(
      x = Days_after_hatch,
      y = proportion,
      color = individual
    )
  ) +
  # geom_ribbon(
  #   aes(
  #     ymin = q5,
  #     ymax = q95,
  #   ),
  #   fill = "grey",
  #   colour = "transparent"
  # ) +
  geom_line() +
  geom_point(
    aes(
      x = Days_after_hatch,
      y = mean,
      color = individual,
      fill = individual
    ),
    shape = 21 # circle
  ) +
  labs(
    x = "Days after hatch (28 - 43 days)",
    y = "Proportion to be on land (Unit: %)",
    title = "Water",
    subtitle = ""
  ) +
  scale_color_okabeito() +
  scale_fill_okabeito() +
  # facet_wrap(~ individual, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank()
  )

ggsave(
  "line_data_yhat_y_water.pdf",
  plot = line_data_yhat_y_water,
  height = 200,
  width = 200,
  units = "mm"
)




# shore
# overlap observed number and estimated values 
# yhat
# yhat denotes estimated value of Y
data_yhat_shore <- 
  fit_shore_summary %>% 
  # pick up the yhat
  dplyr::filter(
    stringr::str_detect(
      variable, 
      "yhat"
    ) 
  ) %>% 
  # 
  dplyr::mutate(
    # parameter's name
    parameter = str_extract(variable, "(.+)(?=\\[)"),
    # days after hatch
    Days_after_hatch = as.numeric(str_extract(variable, "(?<=[:punct:])(.+)(?=\\,)")),
    # ID by individual (toll tank)
    individual = as.numeric(str_extract(variable, "(?<=,)(.+)(?=\\])"))
  ) %>% 
  # transform the number into group ID
  dplyr::mutate(
    individual = 
      factor(
        dplyr::case_when(
          individual == 1 ~ "070708-1",
          individual == 2 ~ "070708-2",
          individual == 3 ~ "070708-3",
          individual == 4 ~ "070618-1",
          individual == 5 ~ "070618-2",
          individual == 6 ~ "070618-3",
          individual == 7 ~ "070525-1",
          individual == 8 ~ "070525-2",
          TRUE ~ "hoge"
        )
      ),
    # for convenience to indicate x axis
    Days_after_hatch = Days_after_hatch+27
  )

# Y
data_Y_shore <- 
  mudskipper_data %>% 
  dplyr::filter(position == "shore") %>%
  dplyr::select(-group, -position) 

data_yhat_y_shore <- 
  data_yhat_shore %>% 
  dplyr::left_join(
    data_Y_shore,
    by = c("individual", "Days_after_hatch")
  )

# 作図
line_data_yhat_y_shore <- 
  data_yhat_y_shore %>% 
  ggplot2::ggplot(
    aes(
      x = Days_after_hatch,
      y = proportion,
      color = individual
    )
  ) +
  # geom_ribbon(
  #   aes(
  #     ymin = q5,
  #     ymax = q95,
  #   ),
  #   fill = "grey",
  #   colour = "transparent"
  # ) +
  geom_line() +
  geom_point(
    aes(
      x = Days_after_hatch,
      y = mean,
      color = individual,
      fill = individual
    ),
    shape = 21 # circle
  ) +
  labs(
    x = "Days after hatch (28 - 43 days)",
    y = "Proportion to be on land (Unit: %)",
    title = "Shore",
    subtitle = ""
  ) +
  scale_color_okabeito() +
  scale_fill_okabeito() +
  # facet_wrap(~ individual, scales = "free_y") +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank()
  )

ggsave(
  "line_data_yhat_y_shore.pdf",
  plot = line_data_yhat_y_shore,
  height = 200,
  width = 200,
  units = "mm"
)



# END
