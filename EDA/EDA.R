# 02_eda.R
library(tidyverse)

coffee <- read_csv("D:/Stats140/coffee_clean.csv", show_col_types = FALSE)

core <- c("aroma", "flavor", "aftertaste", "acidity", "body",
          "balance", "cupper_points")
coffee <- coffee |>
  mutate(country = recode(country_of_origin,
                          "United States (Hawaii)"       = "USA (Hawaii)",
                          "Tanzania, United Republic Of" = "Tanzania"))

# shared theme + palette
accent <- "#6F4E37"   # coffee brown
fill2  <- "#C8A27C"   # latte
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold"),
                  panel.grid.minor = element_blank()))

dir.create("figures", showWarnings = FALSE)

#1) sample size by country
p1 <- coffee |>
  count(country) |>
  ggplot(aes(x = n, y = fct_reorder(country, n))) +
  geom_col(fill = accent) +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = sprintf("Sample size by country of origin (n = %d)", nrow(coffee)),
       x = "number of coffees", y = NULL)

p1

ggsave("D:/Stats140/figures/01_country_counts.png", p1, width = 8, height = 4.2, dpi = 150)

#2) distribution of total cup points
mu <- mean(coffee$total_cup_points)
p2 <- ggplot(coffee, aes(total_cup_points)) +
  geom_histogram(bins = 30, fill = accent, colour = "white", alpha = 0.9) +
  geom_vline(xintercept = mu, linetype = "dashed") +
  annotate("text", x = mu + 0.4, y = Inf, vjust = 2,
           label = sprintf("mean = %.1f", mu), size = 3.3) +
  labs(title = "Distribution of Total Cup Points",
       x = "Total Cup Points", y = "count")
p2

ggsave("D:/Stats140/figures/02_score_dist.png", p2, width = 7, height = 4, dpi = 150)

#3) correlation heatmap of sensory scores (motivates PCA)
corr <- cor(coffee[core], use = "complete.obs")
corr_long <- as.data.frame(corr) |>
  rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "r") |>
  mutate(var1 = factor(var1, levels = core),
         var2 = factor(var2, levels = rev(core)))

p3 <- ggplot(corr_long, aes(var1, var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  scale_fill_gradient(low = "#F2E8DC", high = accent, limits = c(0, 1)) +
  coord_fixed() +
  labs(title = "Correlations among sensory scores",
       subtitle = "Strong positive correlations motivate PCA / factor analysis",
       x = NULL, y = NULL, fill = "r") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
p3
ggsave("D:/Stats140/figures/03_corr_heatmap.png", p3, width = 6.5, height = 6, dpi = 150)

#4) total cup points by country (motivates MANOVA)
p4 <- coffee |>
  mutate(country = fct_reorder(country, total_cup_points, .fun = median,
                               .desc = TRUE)) |>
  ggplot(aes(country, total_cup_points)) +
  geom_boxplot(fill = fill2, outlier.size = 0.8) +
  labs(title = "Total Cup Points by country",
       subtitle = "Modest differences in the total — the full sensory vector (MANOVA) reveals more",
       x = NULL, y = "Total Cup Points") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
p4
ggsave("D:/Stats140/figures/04_box_by_country.png", p4, width = 9, height = 4.5, dpi = 150)

