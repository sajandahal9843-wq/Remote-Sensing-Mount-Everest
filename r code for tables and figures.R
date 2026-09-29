# ==============================================================================
# Title: Publication-Grade Graph Generation (No Titles/Subtitles for Journal Captions)
# File Path: C:/Users/user/OneDrive/Desktop/everest/everest.csv
# Output: Figure 1 to Figure 4 (Clean 300 DPI PNGs ready for manuscript captions)
# ==============================================================================

# 1. Load Required Libraries
required_pkgs <- c("tidyverse", "ggplot2", "scales", "gridExtra", "corrplot")
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
}
library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)
library(corrplot)

# 2. Load Data
file_path <- "C:/Users/user/OneDrive/Desktop/everest/everest.csv"
if (!file.exists(file_path)) {
  file_path <- "Everest_Elevation_Zoned_Snow_Climate_2000_2025.csv" # Fallback
}
df <- read.csv(file_path)

# 3. Parse GEE Zonal Breakdown String format
parse_zonal_str <- function(str_val) {
  if (is.na(str_val)) return(data.frame(zone_1=NA, zone_2=NA, zone_3=NA, zone_4=NA))
  matches <- gregexpr("zone_id\\s*=\\s*([0-9.]+),\\s*sum\\s*=\\s*([0-9.eE+-]+)", str_val)
  extracted <- regmatches(str_val, matches)[[1]]
  
  z_vals <- setNames(rep(0, 4), 1:4)
  for (item in extracted) {
    id <- as.numeric(sub(".*zone_id\\s*=\\s*([0-9.]+).*", "\\1", item))
    s <- as.numeric(sub(".*sum\\s*=\\s*([0-9.eE+-]+).*", "\\1", item))
    if (id %in% 1:4) z_vals[id] <- s
  }
  return(data.frame(zone_1=z_vals[1], zone_2=z_vals[2], zone_3=z_vals[3], zone_4=z_vals[4]))
}

zonal_df <- map_dfr(df$zonal_breakdown, parse_zonal_str)
full_data <- bind_cols(df %>% select(year, month, date, basin_mean_temp_k, basin_mean_solar_rad, basin_mean_precip), zonal_df)
full_data$temp_c <- full_data$basin_mean_temp_k - 273.15

# ==============================================================================
# FIGURE 1 (Subsection 4.1): Seasonal Climatology Profile (No Title)
# ==============================================================================
monthly_clima_long <- full_data %>%
  group_by(month) %>%
  summarise(
    Zone_1 = mean(zone_1, na.rm = TRUE),
    Zone_2 = mean(zone_2, na.rm = TRUE),
    Zone_3 = mean(zone_3, na.rm = TRUE),
    Zone_4 = mean(zone_4, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("Zone_"), names_to = "Elevation_Zone", values_to = "Mean_Snow_Area")

fig1 <- ggplot(monthly_clima_long, aes(x = month, y = Mean_Snow_Area, color = Elevation_Zone, group = Elevation_Zone)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_bw(base_size = 13) +
  labs(
    x = "Month",
    y = "Mean Snow Cover Extent (km²)",
    color = "Elevation Zone"
  ) +
  theme(axis.title = element_text(face = "bold"))

ggsave("Figure1_Seasonal_Climatology.png", plot = fig1, width = 8, height = 6, dpi = 300)


# ==============================================================================
# FIGURE 2 (Subsection 4.2): Multi-Decadal Zoned Trends (No Title)
# ==============================================================================
annual_data <- full_data %>%
  group_by(year) %>%
  summarise(
    temp_c = mean(temp_c, na.rm = TRUE),
    solar_rad = mean(basin_mean_solar_rad, na.rm = TRUE),
    precip = mean(basin_mean_precip, na.rm = TRUE),
    zone_1 = mean(zone_1, na.rm = TRUE),
    zone_2 = mean(zone_2, na.rm = TRUE),
    zone_3 = mean(zone_3, na.rm = TRUE),
    zone_4 = mean(zone_4, na.rm = TRUE)
  )

annual_long <- annual_data %>%
  select(year, zone_1, zone_2, zone_3, zone_4) %>%
  pivot_longer(cols = starts_with("zone_"), names_to = "zone", values_to = "snow_area") %>%
  mutate(Zone_Label = case_when(
    zone == "zone_1" ~ "Zone 1 (3000–4000m)",
    zone == "zone_2" ~ "Zone 2 (4000–5000m)",
    zone == "zone_3" ~ "Zone 3 (5000–6000m)",
    zone == "zone_4" ~ "Zone 4 (>6000m)"
  ))

fig2 <- ggplot(annual_long, aes(x = year, y = snow_area, color = Zone_Label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, linetype = "dashed", linewidth = 0.8, se = TRUE) +
  facet_wrap(~Zone_Label, scales = "free_y", ncol = 2) +
  theme_bw(base_size = 12) +
  labs(
    x = "Year",
    y = "Annual Mean Snow Cover Area (km²)"
  ) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  scale_color_brewer(palette = "Dark2")

ggsave("Figure2_Elevation_Trends.png", plot = fig2, width = 10, height = 8, dpi = 300)


# ==============================================================================
# FIGURE 3 (Subsection 4.3): Climatic Drivers Multi-Panel Trend Plot (No Title)
# ==============================================================================
p_temp <- ggplot(annual_data, aes(x = year, y = temp_c)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "firebrick", size = 1.5) +
  geom_smooth(method = "lm", linetype = "dashed", color = "black", linewidth = 0.7) +
  theme_bw(base_size = 11) +
  labs(x = "", y = "Temperature (°C)")

p_precip <- ggplot(annual_data, aes(x = year, y = precip)) +
  geom_line(color = "dodgerblue4", linewidth = 1) +
  geom_point(color = "dodgerblue4", size = 1.5) +
  geom_smooth(method = "lm", linetype = "dashed", color = "black", linewidth = 0.7) +
  theme_bw(base_size = 11) +
  labs(x = "", y = "Precipitation (m)")

p_solar <- ggplot(annual_data, aes(x = year, y = solar_rad)) +
  geom_line(color = "darkorange", linewidth = 1) +
  geom_point(color = "darkorange", size = 1.5) +
  geom_smooth(method = "lm", linetype = "dashed", color = "black", linewidth = 0.7) +
  theme_bw(base_size = 11) +
  labs(x = "Year", y = "Radiation (W/m²)")

fig3 <- grid.arrange(p_temp, p_precip, p_solar, ncol = 1)
ggsave("Figure3_Climatic_Drivers_Trends.png", plot = fig3, width = 8, height = 10, dpi = 300)


# ==============================================================================
# FIGURE 4 (Subsection 4.4): Climate-Snow Correlation Heatmap (No Title)
# ==============================================================================
cor_data <- annual_data %>% select(zone_1, zone_2, zone_3, zone_4, temp_c, solar_rad, precip)
colnames(cor_data) <- c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Temp (°C)", "Solar Rad", "Precip")
cor_mat <- cor(cor_data, use = "complete.obs")

png("Figure4_Climate_Snow_Correlation.png", width = 8, height = 8, units = "in", res = 300)
corrplot(cor_mat, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#B2182B", "#EF8A62", "#F7F7F7", "#67A9CF", "#2166AC"))(200))
dev.off()


# ==============================================================================
# Title: Automated Statistical Table Generation for Upper Dudh Koshi Snow Dynamics
# File Path: C:/Users/user/OneDrive/Desktop/everest/everest.csv
# Output: Publication-Ready CSV Tables & Console Outputs
# ==============================================================================

# 1. Load Required Libraries
required_pkgs <- c("tidyverse", "scales")
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies = TRUE)
}
library(tidyverse)

# 2. Load Data (Update path if necessary)
file_path <- "C:/Users/user/OneDrive/Desktop/everest/everest.csv"
# If running in local directory where file was uploaded, fallback to local filename:
if (!file.exists(file_path)) {
  file_path <- "Everest_Elevation_Zoned_Snow_Climate_2000_2025.csv"
}

df <- read.csv(file_path)

# 3. Parse GEE Zonal Breakdown String format into individual numeric columns
parse_zonal_str <- function(str_val) {
  if (is.na(str_val)) return(data.frame(zone_1=NA, zone_2=NA, zone_3=NA, zone_4=NA))
  matches <- gregexpr("zone_id\\s*=\\s*([0-9.]+),\\s*sum\\s*=\\s*([0-9.eE+-]+)", str_val)
  extracted <- regmatches(str_val, matches)[[1]]
  
  z_vals <- setNames(rep(0, 4), 1:4)
  for (item in extracted) {
    id <- as.numeric(sub(".*zone_id\\s*=\\s*([0-9.]+).*", "\\1", item))
    s <- as.numeric(sub(".*sum\\s*=\\s*([0-9.eE+-]+).*", "\\1", item))
    if (id %in% 1:4) z_vals[id] <- s
  }
  return(data.frame(zone_1=z_vals[1], zone_2=z_vals[2], zone_3=z_vals[3], zone_4=z_vals[4]))
}

zonal_df <- map_dfr(df$zonal_breakdown, parse_zonal_str)
full_data <- bind_cols(df %>% select(year, month, date, basin_mean_temp_k, basin_mean_solar_rad, basin_mean_precip), zonal_df)
full_data$temp_c <- full_data$basin_mean_temp_k - 273.15

# ==============================================================================
# TABLE 1: Multi-Decadal Monthly Climatology (Section 4.1)
# ==============================================================================
table1_monthly <- full_data %>%
  group_by(month) %>%
  summarise(
    Zone1_Mean = mean(zone_1, na.rm = TRUE), Zone1_SD = sd(zone_1, na.rm = TRUE),
    Zone2_Mean = mean(zone_2, na.rm = TRUE), Zone2_SD = sd(zone_2, na.rm = TRUE),
    Zone3_Mean = mean(zone_3, na.rm = TRUE), Zone3_SD = sd(zone_3, na.rm = TRUE),
    Zone4_Mean = mean(zone_4, na.rm = TRUE), Zone4_SD = sd(zone_4, na.rm = TRUE),
    Temp_Mean_C = mean(temp_c, na.rm = TRUE)
  )

print("=== TABLE 1: Monthly Climatology ===")
print(table1_monthly)
write.csv(table1_monthly, "Table1_Monthly_Climatology.csv", row.names = FALSE)


# ==============================================================================
# TABLE 2: Elevation-Zoned Linear Trend Statistics (Section 4.2)
# ==============================================================================
annual_data <- full_data %>%
  group_by(year) %>%
  summarise(
    temp_c = mean(temp_c, na.rm = TRUE),
    solar_rad = mean(basin_mean_solar_rad, na.rm = TRUE),
    precip = mean(basin_mean_precip, na.rm = TRUE),
    zone_1 = mean(zone_1, na.rm = TRUE),
    zone_2 = mean(zone_2, na.rm = TRUE),
    zone_3 = mean(zone_3, na.rm = TRUE),
    zone_4 = mean(zone_4, na.rm = TRUE)
  )

calc_trend_stats <- function(vec, yr) {
  model <- lm(vec ~ yr)
  s <- summary(model)
  data.frame(
    Mean = mean(vec, na.rm = TRUE),
    SD = sd(vec, na.rm = TRUE),
    Slope = coef(model)[2],
    P_Value = s$coefficients[2, 4],
    R_Squared = s$r.squared
  )
}

table2_trends <- bind_rows(
  data.frame(Zone = "Zone 1 (3000–4000m)", calc_trend_stats(annual_data$zone_1, annual_data$year)),
  data.frame(Zone = "Zone 2 (4000–5000m)", calc_trend_stats(annual_data$zone_2, annual_data$year)),
  data.frame(Zone = "Zone 3 (5000–6000m)", calc_trend_stats(annual_data$zone_3, annual_data$year)),
  data.frame(Zone = "Zone 4 (>6000m)",     calc_trend_stats(annual_data$zone_4, annual_data$year))
)

print("=== TABLE 2: Elevation Trend Statistics ===")
print(table2_trends)
write.csv(table2_trends, "Table2_Elevation_Trends.csv", row.names = FALSE)


# ==============================================================================
# TABLE 3: Climate-Snow Correlation Matrix (Section 4.4)
# ==============================================================================
cor_matrix <- cor(annual_data %>% select(zone_1, zone_2, zone_3, zone_4, temp_c, solar_rad, precip), use = "complete.obs")

# Extract sub-matrix of climatic drivers vs zones
table3_corr <- cor_matrix[c("temp_c", "solar_rad", "precip"), c("zone_1", "zone_2", "zone_3", "zone_4")]
colnames(table3_corr) <- c("Zone_1", "Zone_2", "Zone_3", "Zone_4")

print("=== TABLE 3: Climate-Snow Pearson Correlation Matrix ===")
print(table3_corr)
write.csv(as.data.frame(table3_corr), "Table3_Correlation_Matrix.csv", row.names = TRUE)