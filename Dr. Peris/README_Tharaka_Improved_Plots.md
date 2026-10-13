# Tharaka 30 m Landsat analysis: improved plots and output paths

## Files

- `Tharaka_Land_Cover_Analysis_Landsat30m_ImprovedPlots.Rmd`: revised source document.

## How to run

1. Save the revised `.Rmd` beside the current project `.Rmd` and reference CSVs, keeping the existing `01_data` folder beside it.
2. Open the revised document in RStudio and Knit it. The setup chunk detects the source document's directory, sets that as the knitr root, and creates `Tharaka_Land_Cover_Results` beside the `.Rmd`.
3. If the 16 seasonal multiband Landsat GeoTIFFs are not already available, use the included Earth Engine export section, download the exports from Google Drive, and place them in `01_data/landsat/qgis_input`. Rerun the QGIS processing section so clipped rasters appear in `01_data/landsat/qgis_clipped`.
4. Rerun Section 38. It checks that inputs are nominally 30 m and creates cluster maps, cluster area/share plots, 100% stacked composition plots, seasonal NDVI signatures, and mapped-coverage QA outputs.
5. Review `Tharaka_Land_Cover_Results/Unsupervised_Image_Learning/Tharaka_Cluster_to_LandCover_Crosswalk.csv` against the maps and seasonal signatures. Fill the cluster labels and rerun Section 38 to generate the named land-cover trend plots.

## Where outputs go

All plot/map graphics are saved underneath `Tharaka_Land_Cover_Results` beside the `.Rmd`:

- `figures/`: pilot-point diagnostics, locator maps, scene diagnostics and optional Random Forest plots.
- `Unsupervised_Image_Learning/`: primary 30 m cluster and image-learned land-cover plots, raster maps, signatures and crosswalk.
- `tables/`: selected output tables and figure inventory.
- `diagnostics/`: mapped-area coverage QA.
- `MODIS_500m_Benchmark/`: optional MODIS comparison, if manually enabled; not the primary trend result.

## Interpretation notes

- The four points are snapshots (2000, 2010, 2020 and 2025); the lines between them are visual guides, not annual observations.
- Pilot-point NDVI and index plots describe the sampled pilot locations, not every pixel in the sub-county.
- Unsupervised clusters are not land-cover names. Review the map and spectral signatures before assigning forest, bushland, grassland, farmland, settlement or other/uncertain labels.
- The new coverage QA compares the amount of valid mapped area in each year. Large differences can reflect clouds or no-data masks, so they should be checked before interpreting raw area changes as actual land-cover conversion.
- The R Markdown source was statically checked for balanced chunks and delimiters, but it was not executed against the user's local Google Earth Engine session or seasonal GeoTIFFs in this environment.
