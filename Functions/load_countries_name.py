import geopandas as gpd
import numpy as np
import os

# Create a custom grid of latitude and longitude points with 0.5-degree resolution
latitude_grid, longitude_grid = np.meshgrid(
    np.arange(-89.75, 90, 0.5),
    np.arange(-179.75, 180, 0.5)
)
grid_df = gpd.GeoDataFrame(geometry=gpd.points_from_xy(longitude_grid.ravel(), latitude_grid.ravel()))
print(grid_df)
grid_df.describe()
# Fetch country boundaries using geopandas (you can use data from Natural Earth, if needed)
# Download country boundaries dataset (if needed)
# gpd.datasets.download('naturalearth_lowres')

# Read country boundaries dataset
countries = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
countries.head()
# Spatial join to find the nearest country for each point in the grid
grid_with_country = gpd.sjoin(grid_df, countries, how="left", op="within")

# Extract the country name for each point and add it to the grid_df
grid_df['country_name'] = grid_with_country['name']

# Print the resulting DataFrame with the custom grid and country names
print(grid_df)

# Save the resulting DataFrame with the custom grid and country names to a CSV file
grid_df.to_csv('custom_grid_with_country_names.csv', index=False)
