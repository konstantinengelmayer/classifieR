library(terra)
# Load the test raster stack
test <- rast(system.file("extdata", "rgb_cir_stack_2020.tif", package = "classifieR"))

# Define a smaller extent for cropping (xmin, xmax, ymin, ymax)
small_extent <- ext(790500, 791000, 5276000, 5277000)

# Crop the raster stack to the smaller extent
test <- crop(test, small_extent)

# Define a test for the calculate_indices function
test_that("calculate_indices works correctly", {
  # Calculate indices
  stack <- calculate_indices(
    red = test$rgb_2020_1,
    blue = test$rgb_2020_2,
    nir = test$cir,
    green = test$rgb_2020_3,
    indices = c("NDVI", "EVI", "SAVI", "GCI", "OSAVI", "MSAVI", "TVI", "DVI", "RVI",
                "IPVI", "GNDVI", "VARI", "TGI", "NDRE", "CIgreen", "CIrededge", "ARVI",
                "MTVI", "NDWI", "NDSI", "BSI", "NBRI", "IBI")
  )

  # Check if the result is a SpatRaster
  expect_s4_class(stack, "SpatRaster")

  # Check if the correct number of layers is returned
  expect_equal(nlyr(stack), 23)

  # Check if the layer names match the indices
  expected_names <- c("NDVI", "EVI", "SAVI", "GCI", "OSAVI", "MSAVI", "TVI", "DVI", "RVI",
                      "IPVI", "GNDVI", "VARI", "TGI", "NDRE", "CIgreen", "CIrededge", "ARVI",
                      "MTVI", "NDWI", "NDSI", "BSI", "NBRI", "IBI")
  expect_equal(names(stack), expected_names)

  # Check if an invalid index raises an error
  expect_error(calculate_indices(
    red = test$rgb_2020_1,
    blue = test$rgb_2020_2,
    nir = test$cir,
    green = test$rgb_2020_3,
    indices = c("INVALID_INDEX")
  ), "Invalid indices specified")
})
