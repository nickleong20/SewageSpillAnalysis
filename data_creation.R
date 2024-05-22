# Import Datasets
# Update the file paths based on the actual location of your files
sewage_spills <- read_csv("data/UPDATED_sewage_spill_locations(Sheet1).csv")
sewage_spills <- clean_names(sewage_spills)

# Read the shapefile
# Update the file path to the shapefile
watersheds_without_islands <- st_read("data/Watersheds/Watersheds.shp")
islands <- st_read("data/2020_Census_County_Boundaries/2020_Census_County_Boundaries.shp")

# Perform spatial join
watersheds <- st_intersection(watersheds_without_islands, islands)

# Convert sewage spills to spatial data frame
sewage_spills_sf <- st_as_sf(sewage_spills, coords = c("longitude", "latitude"), crs = st_crs(watersheds))

# Reproject datasets to WGS84
sewage_spills_sf <- st_transform(sewage_spills_sf, crs = 4326)
watersheds <- st_transform(watersheds, crs = 4326)

# Perform spatial join to attribute sewage spills to watersheds
sewage_spills_watershed <- st_join(sewage_spills_sf, watersheds)

# Handle NA values by finding the nearest watershed
na_spills <- sewage_spills_watershed[is.na(sewage_spills_watershed$wuname), ]
non_na_spills <- sewage_spills_watershed[!is.na(sewage_spills_watershed$wuname), ]
nearest_watershed <- sf::st_nearest_feature(na_spills, non_na_spills)

# Replace NA values with the nearest watershed name
sewage_spills_watershed$wuname[is.na(sewage_spills_watershed$wuname)] <- non_na_spills$wuname[nearest_watershed]
# Convert sewage_spills_watershed to a non-spatial data frame
sewage_spills_watershed_df <- as.data.frame(sewage_spills_watershed)
sewage_spills_watershed_df <- sewage_spills_watershed_df %>%
  select(-objectid, -wuc, -huc, -swma, -area_sqmi, -area_m2, -name_hwn, -st_areasha, -st_perimet, -objectid.1, -geoid20, -name20, -namelsad20, -aland20, -awater20, -pop20, -st_areasha.1, -st_perimet.1)

sewage_spills_watershed_df <- sewage_spills_watershed_df %>%
  left_join(watersheds, by = c("wuname", "island" = "name20"))


# Convert back to a spatial data frame
sewage_spills_watershed <- st_as_sf(sewage_spills_watershed_df)

st_write(sewage_spills_watershed, "sewage_spills_watershed.shp")
