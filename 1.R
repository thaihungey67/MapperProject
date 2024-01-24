# Load the ggplot2 library
library(ggplot2)

# Function to generate a random map
generate_random_map <- function(rows, cols) {
  terrain_types <- c("Grassland", "Forest", "Mountain", "Water")
  
  # Create an empty map
  map <- matrix(0, nrow = rows, ncol = cols)
  
  # Fill the map with random terrain types
  for (i in 1:rows) {
    for (j in 1:cols) {
      map[i, j] <- sample(1:length(terrain_types), 1)
    }
  }
  
  return(list(map = map, terrain_types = terrain_types))
}

# Function to generate an image of the map using ggplot2
generate_map_image <- function(map, terrain_types) {
  df <- data.frame(
    row = rep(1:nrow(map), each = ncol(map)),
    col = rep(1:ncol(map), times = nrow(map)),
    terrain = factor(terrain_types[map], levels = terrain_types)
  )
  
  ggplot(df, aes(x = col, y = row, fill = terrain)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_manual(values = c("green", "darkgreen", "gray", "blue")) +
    theme_void() +
    theme(legend.position="none")
}

# Set the size of the map
rows <- 5
cols <- 5

# Generate and plot the map
random_map <- generate_random_map(rows, cols)
map_image <- generate_map_image(random_map$map, random_map$terrain_types)
print(map_image)
