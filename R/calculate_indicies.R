#' Calculate Indices Based on RGB and NIR Raster Layers
#'
#' This function calculates various indices based on the provided RGB and NIR raster layers.
#' Indices include NDVI, EVI, SAVI, GCI, and many others.
#'
#' @param red SpatRaster representing the Red channel.
#' @param green SpatRaster representing the Green channel.
#' @param blue SpatRaster representing the Blue channel.
#' @param nir SpatRaster representing the Near-Infrared channel.
#' @param indices Character vector specifying which indices to calculate.
#'                Options include "NDVI", "EVI", "SAVI", "GCI", "OSAVI", "MSAVI", "TVI",
#'                "DVI", "RVI", "IPVI", "GNDVI", "VARI", "TGI", "NDRE", "CIgreen",
#'                "CIrededge", "ARVI", "MTVI", "NDWI", "NDSI", "BSI", "NBRI", "IBI".
#' @param output_dir Character string specifying the directory to save the output indices as .tif files.
#'                   If NULL, the function returns a SpatRaster stack.
#' @return A SpatRaster containing the calculated indices if output_dir is NULL, otherwise NULL.
#' @import terra
#' @export
calculate_indices <- function(red, green, blue, nir, indices, output_dir = NULL) {
  # List of valid indices
  valid_indices <- c("NDVI", "EVI", "SAVI", "GCI", "OSAVI", "MSAVI", "TVI",
                     "DVI", "RVI", "IPVI", "GNDVI", "VARI", "TGI", "NDRE",
                     "CIgreen", "CIrededge", "ARVI", "MTVI", "NDWI", "NDSI",
                     "BSI", "NBRI", "IBI")

  # Check if all specified indices are valid
  if (!all(indices %in% valid_indices)) {
    invalid_indices <- indices[!indices %in% valid_indices]
    stop("Invalid indices specified: ", paste(invalid_indices, collapse = ", "))
  }

  results <- list()
  total_indices <- length(indices)
  count <- 0

  if ("NDVI" %in% indices) {
    message("Calculating NDVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$NDVI <- (nir - red) / (nir + red)
    names(results$NDVI) <- "NDVI"
  }
  if ("EVI" %in% indices) {
    message("Calculating EVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$EVI <- 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)
    names(results$EVI) <- "EVI"
  }
  if ("SAVI" %in% indices) {
    message("Calculating SAVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    L <- 0.5
    results$SAVI <- (nir - red) * (1 + L) / (nir + red + L)
    names(results$SAVI) <- "SAVI"
  }
  if ("GCI" %in% indices) {
    message("Calculating GCI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$GCI <- nir / green - 1
    names(results$GCI) <- "GCI"
  }
  if ("OSAVI" %in% indices) {
    message("Calculating OSAVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$OSAVI <- (nir - red) / (nir + red + 0.16)
    names(results$OSAVI) <- "OSAVI"
  }
  if ("MSAVI" %in% indices) {
    message("Calculating MSAVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$MSAVI <- (2 * nir + 1 - sqrt((2 * nir + 1)^2 - 8 * (nir - red))) / 2
    names(results$MSAVI) <- "MSAVI"
  }
  if ("TVI" %in% indices) {
    message("Calculating TVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$TVI <- sqrt((nir - red) / (nir + red) + 0.5)
    names(results$TVI) <- "TVI"
  }
  if ("DVI" %in% indices) {
    message("Calculating DVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$DVI <- nir - red
    names(results$DVI) <- "DVI"
  }
  if ("RVI" %in% indices) {
    message("Calculating RVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$RVI <- nir / red
    names(results$RVI) <- "RVI"
  }
  if ("IPVI" %in% indices) {
    message("Calculating IPVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$IPVI <- nir / (nir + red)
    names(results$IPVI) <- "IPVI"
  }
  if ("GNDVI" %in% indices) {
    message("Calculating GNDVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$GNDVI <- (nir - green) / (nir + green)
    names(results$GNDVI) <- "GNDVI"
  }
  if ("VARI" %in% indices) {
    message("Calculating VARI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$VARI <- (green - red) / (green + red - blue)
    names(results$VARI) <- "VARI"
  }
  if ("TGI" %in% indices) {
    message("Calculating TGI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$TGI <- -0.5 * ((190 * (red - green)) - (120 * (red - blue)))
    names(results$TGI) <- "TGI"
  }
  if ("NDRE" %in% indices) {
    message("Calculating NDRE... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$NDRE <- (nir - red) / (nir + red)
    names(results$NDRE) <- "NDRE"
  }
  if ("CIgreen" %in% indices) {
    message("Calculating CIgreen... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$CIgreen <- nir / green - 1
    names(results$CIgreen) <- "CIgreen"
  }
  if ("CIrededge" %in% indices) {
    message("Calculating CIrededge... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$CIrededge <- nir / red - 1
    names(results$CIrededge) <- "CIrededge"
  }
  if ("ARVI" %in% indices) {
    message("Calculating ARVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$ARVI <- (nir - (2 * red - blue)) / (nir + (2 * red + blue))
    names(results$ARVI) <- "ARVI"
  }
  if ("MTVI" %in% indices) {
    message("Calculating MTVI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$MTVI <- 1.5 * (1.2 * (nir - green) - 2.5 * (red - green)) / sqrt((2 * nir + 1)^2 - (6 * nir - 5 * sqrt(red)) - 0.5)
    names(results$MTVI) <- "MTVI"
  }
  if ("NDWI" %in% indices) {
    message("Calculating NDWI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$NDWI <- (green - nir) / (green + nir)
    names(results$NDWI) <- "NDWI"
  }
  if ("NDSI" %in% indices) {
    message("Calculating NDSI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$NDSI <- (green - nir) / (green + nir)
    names(results$NDSI) <- "NDSI"
  }
  if ("BSI" %in% indices) {
    message("Calculating BSI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$BSI <- (red + nir - (blue + green)) / (red + nir + (blue + green))
    names(results$BSI) <- "BSI"
  }
  if ("NBRI" %in% indices) {
    message("Calculating NBRI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$NBRI <- (nir - red) / (nir + red)
    names(results$NBRI) <- "NBRI"
  }
  if ("IBI" %in% indices) {
    message("Calculating IBI... ", count <- count + 1, "/", total_indices, " indices calculated.")
    results$IBI <- (2 * green - (nir + blue)) / (nir + blue)
    names(results$IBI) <- "IBI"
  }
  message(count, "/", total_indices, " indices calculated.")

  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    for (name in names(results)) {
      message("Saving ", name, " to file...")
      writeRaster(results[[name]], filename = file.path(output_dir, paste0(name, ".tif")), overwrite = TRUE)
    }
    return(NULL)
  } else {
    # Convert list to SpatRaster
    result_stack <- rast(results)
    return(result_stack)
  }
}


