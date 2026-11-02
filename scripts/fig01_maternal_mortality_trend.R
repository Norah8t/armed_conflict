# Figure 1: maternal mortality trend, 2000-2017, for countries where it increased
#
# Input:  data/processed/final_data.csv, data/original/countries.txt
# Output: output/fig01_maternal_mortality_trend.png
# Run from the repository root.

library(dplyr)
library(ggplot2)

conflict <- read.csv(file.path("data", "processed", "final_data.csv"))
countries <- read.table(file.path("data", "original", "countries.txt"),
                        header = TRUE, quote = "\"") |>
  rename(iso = ISO)

# Stop at 2017: maternal mortality is missing for every country in 2018 and 2019.
# diff_matmor is each year's value minus the same country's value in 2000.
matmor_diff <- conflict |>
  dplyr::select(iso, year, maternal_mortality) |>
  dplyr::filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(diff_matmor = maternal_mortality - maternal_mortality[1L])

# Keep countries whose 2017 value is above their 2000 value. Countries with no
# maternal mortality data give NA here and are dropped.
matmor_increase <- matmor_diff |>
  dplyr::filter(diff_matmor[year == 2017] > 0) |>
  ungroup() |>
  left_join(countries, by = "iso")

n_with_data <- matmor_diff |>
  dplyr::filter(!is.na(maternal_mortality)) |>
  pull(iso) |>
  n_distinct()
n_increase <- n_distinct(matmor_increase$iso)

# Order the legend by the 2017 value, so it follows the order of the line ends
legend_order <- matmor_increase |>
  dplyr::filter(year == 2017) |>
  arrange(desc(maternal_mortality)) |>
  pull(country_name)

matmor_increase <- matmor_increase |>
  mutate(country_name = factor(country_name, levels = legend_order))

# One colour per country, ordered so neighbouring legend entries stay distinct
# under common colour-vision deficiencies
country_colours <- c("#0097b2", "#eb6834", "#9a6a00", "#e87ba4", "#a33fb5",
                     "#3f8fc4", "#6250d6", "#7f9c00", "#eda100", "#008300",
                     "#2a78d6", "#1baf7a", "#b0307a")
stopifnot(n_increase <= length(country_colours))

fig_trend <- ggplot(matmor_increase,
                    aes(x = year, y = maternal_mortality, colour = country_name)) +
  geom_line(linewidth = 0.7) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2017),
                     minor_breaks = NULL) +
  # Log scale: Haiti is near 500 and Canada near 10, which would flatten most
  # of the lines on a linear axis
  scale_y_log10(breaks = c(10, 20, 50, 100, 200, 500), minor_breaks = NULL) +
  scale_colour_manual(values = country_colours) +
  labs(
    title = sprintf("Maternal mortality rose from 2000 to 2017 in %d of %d countries",
                    n_increase, n_with_data),
    x = "Year",
    y = "Maternal mortality per 100,000 live births (log scale)",
    colour = "Country (highest 2017\nvalue first)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA))

dir.create("output", showWarnings = FALSE)
ggsave(file.path("output", "fig01_maternal_mortality_trend.png"),
       fig_trend, width = 8, height = 5, dpi = 300)
