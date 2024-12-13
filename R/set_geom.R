

# Crear un objeto sf de ejemplo
puntos <- data.frame(
  id = 1:3,
  x = c(10, 20, 30),
  y = c(40, 50, 60)
)
sf_obj <- sf::st_as_sf(puntos, coords = c("x", "y"), crs = 4326)
print(sf_obj)

nombre_geometria <- attr(sf_obj, "sf_column")

# Cambiar el nombre del campo de geometría
sf_obj <- sf::st_set_geometry(sf_obj, "geom")
names(sf_obj)[names(sf_obj) == nombre_geometria] <- "geom"
print(sf_obj)
