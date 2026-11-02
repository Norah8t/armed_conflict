# Table 1: country-year characteristics by armed conflict status, 2000-2017
#
# Input:  data/processed/final_data.csv (made by scripts/create_final_data.R)
# Output: output/table01_armed_conflict_characteristics.docx
# Run from the repository root.

library(dplyr)
library(flextable)
library(table1)

# Each row of the data is one country-year, so the table describes country-years
# rather than one baseline row per country. This keeps every country's missing
# years visible. Maternal mortality is not available after 2017.
final_data <- read.csv(file.path("data", "processed", "final_data.csv"))

table_data <- final_data |>
  filter(year <= 2017) |>
  mutate(
    conflict_group = factor(armconf1, levels = c(0, 1),
                            labels = c("No armed conflict", "Armed conflict")),
    across(c(oecd, earthquake, drought),
           ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes")))
  )

label(table_data$maternal_mortality) <- "Maternal mortality (per 100,000 live births)"
label(table_data$infant_mortality)   <- "Infant mortality (per 1,000 live births)"
label(table_data$neonatal_mortality) <- "Neonatal mortality (per 1,000 live births)"
label(table_data$under5_mortality)   <- "Under-5 mortality (per 1,000 live births)"
label(table_data$gdp_1000)           <- "GDP per capita (current US$ 1,000)"
label(table_data$oecd)               <- "OECD member"
label(table_data$pop_dens)           <- "Population density (% living at >1,000 people/km\u00b2)"
label(table_data$urban)              <- "Urban residence (%)"
label(table_data$age_dep)            <- "Age dependency ratio (per 100 working-age people)"
label(table_data$male_edu)           <- "Male education (mean years per capita)"
label(table_data$temp)               <- "Population-weighted mean temperature (\u00b0C)"
label(table_data$rainfall_1000)      <- "Population-weighted rainfall (1,000 mm/year)"
label(table_data$earthquake)         <- "Earthquake in the year"
label(table_data$drought)            <- "Drought in the year"

# Variables with small values need an extra decimal place
two_decimals <- c("gdp_1000", "rainfall_1000")

# Median [Q1, Q3] for continuous variables, because most are skewed; n (%) of
# "Yes" for binary variables. The Missing row is written for every variable, so
# variables with no missing data show 0 rather than dropping the row.
render_cell <- function(x, name, ...) {
  n_missing <- sum(is.na(x))
  missing_cell <- sprintf("%s (%.1f%%)",
                          format(n_missing, big.mark = ",", trim = TRUE),
                          100 * n_missing / length(x))

  if (is.numeric(x)) {
    digits <- if (name %in% two_decimals) 2 else 1
    q <- unname(formatC(quantile(x, c(0.25, 0.50, 0.75), na.rm = TRUE),
                        format = "f", digits = digits, big.mark = ","))
    c("",
      "Median [Q1, Q3]" = sprintf("%s [%s, %s]", q[2], q[1], q[3]),
      "Missing" = missing_cell)
  } else {
    n_yes <- sum(x == "Yes", na.rm = TRUE)
    c("",
      "Yes" = sprintf("%s (%.1f%%)", format(n_yes, big.mark = ",", trim = TRUE),
                      100 * n_yes / sum(!is.na(x))),
      "Missing" = missing_cell)
  }
}

table_1 <- table1(
  ~ maternal_mortality + infant_mortality + neonatal_mortality +
    under5_mortality + gdp_1000 + oecd + pop_dens + urban + age_dep + male_edu +
    temp + rainfall_1000 + earthquake + drought | conflict_group,
  data = table_data,
  overall = c(left = "All country-years"),
  render = render_cell
)

table_note <- sprintf(
  paste0("Unit of analysis is the country-year: %s country-years from %s countries, ",
         "%s-%s. Armed conflict means a conflict with at least 25 battle-related deaths in the ",
         "previous year (one-year lag), so a country can contribute to both ",
         "columns. Missing is n (%% of the column's country-years). Percentages ",
         "for Yes use non-missing country-years."),
  format(nrow(table_data), big.mark = ","), n_distinct(table_data$iso),
  min(table_data$year), max(table_data$year)
)

table_1_word <- t1flex(table_1) |>
  set_caption(as_paragraph(as_chunk(
    "Table 1. Characteristics of country-years by armed conflict status",
    props = fp_text_default(font.family = "Arial", font.size = 10,
                            bold = TRUE, italic = FALSE)
  ))) |>
  add_footer_lines(table_note) |>
  font(fontname = "Arial", part = "all") |>
  fontsize(size = 9, part = "all") |>
  # Tight rows and set column widths keep the whole table on one page and inside
  # the margins. autofit() sets both, so width() and height_all() come after it.
  padding(padding.top = 1, padding.bottom = 1, part = "body") |>
  autofit() |>
  width(j = 1, width = 3.3) |>
  width(j = 2:4, width = 1.3) |>
  height_all(height = 0.19, part = "body")

dir.create("output", showWarnings = FALSE)
save_as_docx(
  table_1_word,
  path = file.path("output", "table01_armed_conflict_characteristics.docx"),
  pr_section = officer::prop_section(
    page_margins = officer::page_mar(top = 0.7, bottom = 0.7,
                                     left = 0.5, right = 0.5,
                                     header = 0.3, footer = 0.3)
  )
)
