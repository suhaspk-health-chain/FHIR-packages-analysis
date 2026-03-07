# ============================================
# Quick Summary Plots for FHIR Resources
# ============================================

# --- Setup ---
source("D:/health-chain-repository/HC Data analysis/FHIR-packages-analysis/scripts/00_setup_packages.R")
source("D:/health-chain-repository/HC Data analysis/FHIR-packages-analysis/scripts/01_config.R")
source("D:/health-chain-repository/HC Data analysis/FHIR-packages-analysis/scripts/05_theme_healthchain.R")

# Load cleaned resource-level dataset
res <- arrow::read_parquet(file.path(OUT_DIR, "processed/fhir_resources.parquet"))

# --- 1️⃣ Distribution of Resources by FHIR Version ---
p1 <- res %>%
  count(version) %>%
  ggplot(aes(x = version, y = n, fill = version)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.3, color = "#0c223f", size = 4) +
  labs(
    title = "FHIR Resources by Version",
    x = "FHIR Version",
    y = "Resource Count"
  ) +
  theme_healthchain()

ggsave(
  file.path(FIG_DIR, "res_by_version.png"),
  p1,
  width = 8,
  height = 5,
  dpi = 150
)

# --- 2️⃣ Top 15 FHIR Resource Types (Overall) ---
top_data <- res %>%
  count(resource_type, sort = TRUE) %>%
  slice_head(n = 15)

p2 <- top_data %>%
  ggplot(aes(x = reorder(resource_type, n), y = n, fill = resource_type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = -0.1,
    color = "#0c223f",
    size = 4
  ) +
  expand_limits(y = max(top_data$n) * 1.15) +
  labs(
    title = "Top 15 FHIR Resource Types (All Versions)",
    x = "Resource Type",
    y = "Count"
  ) +
  theme_healthchain()

ggsave(
  file.path(FIG_DIR, "top_resource_types.png"),
  p2,
  width = 12,
  height = 8,
  dpi = 250
)

# --- 3️⃣ Top Resource Types by Version (Faceted) ---
top_by_version <- res %>%
  count(version, resource_type) %>%
  group_by(version) %>%
  slice_max(order_by = n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(version = factor(version, levels = sort(unique(version))))

p3 <- top_by_version %>%
  ggplot(aes(x = reorder(resource_type, n), y = n, fill = version)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ version, scales = "free_y") +
  geom_text(
    aes(label = scales::comma(n)),
    hjust = -0.1,
    color = "#0c223f",
    size = 3
  ) +
  labs(
    title = "Top Resource Types by Version",
    x = "Resource Type (per version)",
    y = "Count"
  ) +
  theme_healthchain()

ggsave(
  file.path(FIG_DIR, "top_resource_types_by_version.png"),
  p3,
  width = 16,
  height = 12,
  dpi = 200
)

# --- 4️⃣ Summary Log ---
cat("\n✅ Plot generation completed successfully:\n",
    "- figs/res_by_version.png\n",
    "- figs/top_resource_types.png\n",
    "- figs/top_resource_types_by_version.png\n")