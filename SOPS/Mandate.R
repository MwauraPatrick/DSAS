library(sf)
library(rnaturalearth)
library(ggplot2)

# Get Kenya boundaries
kenya <- ne_countries(country = "Kenya", scale = "medium", returnclass = "sf")

# Plot Kenya silhouette (black shape, white background)
p <- ggplot() +
  geom_sf(data = kenya, fill = "black", color = NA) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# Save as Kenya mask
ggsave("kenya_mask.png", p, width = 6, height = 6, dpi = 300, bg = "white")


# -------------------------------
# 1. DATA
# -------------------------------
keywords <- data.frame(
  word = c(
    "data", "analysis", "biostatistics", "modelling",
    "monitoring", "evaluation", "bioinformatics",
    "predictive models", "machine learning", "algorithms",
    "design", "reporting", "genomic", "proteomic",
    "database management", "data safeguarding", "collection",
    "interpretation", "epidemiology", "disease trends",
    "automation", "capacity building", "mentorship",
    "dissemination", "publications", "workshops",
    "research support", "ensemble modelling", "computational tools"
  ),
  weight = c(
    50, 48, 45, 44,
    40, 38, 37,
    36, 35, 34,
    32, 30, 28, 27,
    26, 25, 24,
    23, 22, 21,
    20, 18, 17,
    15, 14, 13,
    12, 11, 10
  )
)


# -------------------------------
# 2. PACKAGES
# -------------------------------
library(wordcloud2)
library(webshot2)
library(htmlwidgets)

# -------------------------------
# 3. KENYA MASK IMAGE
# -------------------------------
# IMPORTANT: put "kenya_mask.png" in your working directory.
# Black shape, white background.

mask_path <- "kenya_mask.png"


# -------------------------------
# 4. CREATE WORD CLOUD
# -------------------------------
wc <- wordcloud2(
  data = keywords,
  size = 1.45,
  color = "random-dark",
  backgroundColor = "white",
  figPath = mask_path # <-- Kenya shape
)

# Preview in RStudio viewer
wc


# -------------------------------
# 5. EXPORT AS PNG
# -------------------------------

save_png <- function(widget, file, width = 1000, height = 1000) {
  tmpHTML <- tempfile(fileext = ".html")
  saveWidget(widget, tmpHTML, selfcontained = TRUE)
  webshot(tmpHTML, file = file, vwidth = width, vheight = height)
}

save_png(wc, "kenya_wordcloud.png")
