# RQ1: Latent structure of cupping scores
# Question: What underlying dimensions explain professional cupping scores?
# Methods : PCA + factor analysis on the 6 core sensory attributes.

library(tidyverse)
library(psych)
library(ggrepel)


# 6 core sensory attributes. We EXCLUDE:
# uniformity / clean_cup / sweetness  (near-constant at 10)
# total_cup_points (it is the sum of the components)
# cupper_points (holistic "overall" score, not an attribute)
core <- c("aroma", "flavor", "aftertaste", "acidity", "body", "balance")
X <- coffee %>%
  dplyr::select(all_of(core)) %>%
  drop_na()

accent <- "#6F4E37"; fill2 <- "#C8A27C"
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold"),
                  panel.grid.minor = element_blank()))

# 1) Suitability checks 
R <- cor(X)
kmo  <- KMO(R)
bart <- cortest.bartlett(R, n = nrow(X))
cat("KMO (overall MSA):", round(kmo$MSA, 3), "\n")
cat(sprintf("Bartlett: chi2 = %.1f, df = %d, p = %.3g\n",
            bart$chisq, bart$df, bart$p.value))

# 2) PCA 
pca <- prcomp(X, scale. = TRUE)

# The sign of each principal component is arbitrary (an eigenvector times -1 is
# still an eigenvector). prcomp may return PC1 with all-negative loadings, which
# would make "higher quality" point left and contradict the axis label. Anchor
# each PC so its largest-magnitude loading is positive -> deterministic & intuitive.
flip <- apply(pca$rotation, 2, function(v) sign(v[which.max(abs(v))]))
pca$rotation <- sweep(pca$rotation, 2, flip, `*`)
pca$x        <- sweep(pca$x,        2, flip, `*`)

ev  <- pca$sdev^2 # eigenvalues
evr <- ev / sum(ev) # proportion of variance

variance_tbl <- tibble(
  PC         = paste0("PC", seq_along(ev)),
  eigenvalue = round(ev, 3),
  prop_var   = round(evr, 3),
  cumulative = round(cumsum(evr), 3)
)
print(variance_tbl)

# loadings (correlation of variables with PCs) = rotation * sdev
loadings <- sweep(pca$rotation, 2, pca$sdev, `*`)
loadings_tbl <- as_tibble(loadings[, 1:2], rownames = "attribute") |>
  rename(PC1 = PC1, PC2 = PC2) |>
  mutate(across(c(PC1, PC2), ~ round(.x, 3)))
cat("\nLoadings (first 2 PCs):\n"); print(loadings_tbl)

# 3) Scree plot 
p_scree <- tibble(PC = seq_along(ev), eigenvalue = ev, prop = evr) |>
  ggplot(aes(PC, eigenvalue)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "gray60") +
  geom_line(colour = accent, linewidth = 1) +
  geom_point(colour = accent, size = 3) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            vjust = -0.9, size = 3.2) +
  annotate("text", x = max(ev), y = 1.12, label = "Kaiser = 1",
           colour = "gray50", size = 3, hjust = 1) +
  scale_x_continuous(breaks = seq_along(ev)) +
  labs(title = "Scree plot — one dominant component",
       x = "principal component", y = "eigenvalue")

p_scree

ggsave(
  filename = "figures/05_pca_scree.png",
  plot = p_scree,
  width = 8,
  height = 5,
  dpi = 300)

# 4) Biplot coloured by Total Cup Points
scores <- as_tibble(pca$x[, 1:2])
# X was built as coffee |> select(core) |> drop_na(), so dropping the same
# rows from coffee reproduces X's row order exactly -> safe to bind.
scores$total <- coffee |> drop_na(all_of(core)) |> pull(total_cup_points)

arrows_df <- as_tibble(loadings[, 1:2], rownames = "attribute")
scl <- 3.2

p_biplot <- ggplot(scores, aes(PC1, PC2)) +
  geom_hline(yintercept = 0, colour = "gray80", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "gray80", linewidth = 0.3) +
  geom_point(aes(colour = total), size = 1.4, alpha = 0.7) +
  scale_colour_gradient(low = "#EAD7B7", high = accent, name = "Total\nCup Points") +
  geom_segment(data = arrows_df,
               aes(x = 0, y = 0, xend = PC1 * scl, yend = PC2 * scl),
               arrow = arrow(length = unit(0.18, "cm")),
               colour = "#3b2417", linewidth = 0.7, inherit.aes = FALSE) +
  ggrepel::geom_text_repel(
    data = arrows_df,
    aes(x = PC1 * scl * 1.12, y = PC2 * scl * 1.12, label = attribute),
    fontface = "bold", size = 3.4, inherit.aes = FALSE,
    segment.color = NA, box.padding = 0.3, max.overlaps = Inf)+
  labs(title = "PCA biplot of sensory profile",
       x = sprintf("PC1 (%.0f%%) — overall quality", evr[1] * 100),
       y = sprintf("PC2 (%.0f%%) — aroma vs body contrast", evr[2] * 100))
p_biplot
              
# 5) Factor analysis (for comparison with PCA) 
# 1-factor and 2-factor ML solutions; 2-factor uses varimax rotation.
fa1 <- fa(X, nfactors = 1, fm = "ml")
fa2 <- fa(X, nfactors = 2, fm = "ml", rotate = "varimax")


cat("Factor analysis: 1 factor");  print(fa1$loadings, cutoff = 0)
cat("Factor analysis: 2 factors (varimax)"); print(fa2$loadings, cutoff = 0)
cat("FA 2-factor variance explained:"); print(round(fa2$Vaccounted, 3))

