#' @author Zhengjia Wang
#' @date 2023-02-13
#' @license Apache-2.0
#' 
#' @title Generate FreeSurfer `pial-outer-smoothed` surface from `lh.pial`
#' @description The `pial-outer-smoothed` is originally designed for 
#' gyrification. However, many other calculations also need this surface, 
#' for example, brain-shift correction in electrode localization. 
#' Generating `pial-outer-smoothed` from native FreeSurfer requires Matlab.
#' Here's an alternative implementation in R.
#' 
#' @param surface_path path to a FreeSurfer pial surface
#' @param IJK2RAS voxel 'IJK' (zero-indexed) to 'tkrRAS' or 'RAS' transform;
#' leave it `NULL` if you don't know how to set it
#' @param save_path path to save the pial envelope, or `NULL` if no save 
#' is needed
#' @param verbose whether to verbose the progress; default is true
#' @param preview whether to preview the 3D model; default is false. Please
#' install package `rgl` if enabled
#' 
#' @returns A pial envelope as 3D mesh
#' 
#' @examples 
#' 
#' snippet <- raveio::load_snippet("imaging-pial-outer-smoothed")
#' surface_path <- "~/rave_data/data_dir/demo/DemoSubject/fs/surf/lh.pial.asc"
#' envelope <- snippet(surface_path = surface_path)
#' 
#' if(dipsaus::package_installed("rgl")) {
#'   rgl::shade3d(envelope, col = 2)
#' }
#' 
#' END OF DOC
NULL

# ---- Variable Section --------------------------------------------------------

# surface_path <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV010/rave-imaging/fs/surf/rh.pial'
# IJK2RAS <- NULL
# verbose <- TRUE
# preview <- TRUE
# save_path <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV010/rave-imaging/fs/surf/lh.pial-outer-smoothed'

# ---- Code Section ------------------------------------------------------------
# ensure parameters
force(surface_path)

# Preparations
# This script require `imager` and `Rvcg` packages
if(!dipsaus::package_installed("imager")) { ravemanager:::install_packages("imager") }
if(!dipsaus::package_installed("Rvcg")) { ravemanager:::install_packages("Rvcg") }
debug <- function(..., appendLF = TRUE) {
  if (verbose) {
    message(..., appendLF = appendLF)
  }
}

if(interactive()) {
  rgl_options <- options("rgl.useNULL" = TRUE, "rgl.startQuartz" = FALSE)
  on.exit({ do.call(options, rgl_options) }, add = TRUE)
}

preview3D <- function(..., func = "wire3d") {
  if(interactive() && 
     isTRUE(get0("preview", ifnotfound = FALSE)) && 
     dipsaus::package_installed("rgl")) {
    
    rgl <- asNamespace("rgl")
    if( ...length() == 0 ) {
      widget <- rgl$rglwidget(x = rgl$scene3d(minimal = FALSE))
      print(widget)
      return()
    } else {
      rgl[[func]](...)
    }
    
  }
}
preview2D <- function(expr) {
  if(interactive() && isTRUE(get0("preview", ifnotfound = FALSE))) {
    expr
  }
}
`%?<-%` <- dipsaus::`%?<-%`

# Initialize the parameters
resolution <- 256L
IJK2RAS %?<-% matrix(
  nrow = 4, byrow = TRUE, 
  c(-1, 0, 0, resolution/2, 
    0, 0, 1, -resolution/2, 
    0, -1, 0, resolution/2, 
    0, 0, 0, 1))
verbose %?<-% TRUE
preview %?<-% FALSE
save_path %?<-% NULL
if(length(save_path) > 0) {
  if(length(save_path) > 1) {
    warning("`save_path` length is greater than 1. Using the first path...")
    save_path <- save_path[[1]]
  }
  if(!file.exists(dirname(save_path))) {
    stop("Cannot find directory name: [", dirname(save_path), "]. Please create the directory first.")
  }
}

# Load surface & convert to `mesh3d` object
surface_orig <- freesurferformats::read.fs.surface(surface_path)
surface_orig <- structure(list(
  vb = rbind(t(surface_orig$vertices), 1), 
  it = t(surface_orig$faces)), class = "mesh3d")

# Transform surface to IJK voxel space so can be fitted into a volume
# Remember IJK starts from 0
surface <- surface_orig
surface$vb <- solve(IJK2RAS) %*% surface$vb

vertex_range <- apply(surface$vb, 1, range)
max_fill <- min(c(
  vertex_range[1, 1:3], 
  resolution - vertex_range[2, 1:3],
  20
))

max_fill <- floor(max_fill) - 2
debug("Max grow size: ", max_fill)

# Creating a volume
volume <- array(0L, dim = c(rep(resolution, 3), 1))

# embed the surface in volume space 
surface_index <- round(surface$vb[c(1,2,3), ])
surface_index <- surface_index[1, ] + surface_index[2, ] * resolution + 
  surface_index[3, ] * (resolution^2) + 1
volume[surface_index] <- 1L

# convert to cimg object so imager can handle it
volume <- imager::as.cimg(volume)

# Grow the volume by ~15mm and shrink back (~12mm). 
# This step connects the 
# segmented voxels into a shell that is water-tight
volume_grew <- imager::grow(volume, max_fill)
volume_shrunk <- imager::shrink(volume_grew, max_fill - 3)

# bucket-fill from the corner 
volume_filled <- imager::bucketfill(
  volume_shrunk, x = 1, y = 1, z = 1, color = 1L)

# Fill the voxels within the surface
volume2 <- 1L - (volume_filled - volume_shrunk)

# preview
preview2D({
  oldPar <- graphics::par(c("mfrow", "mar"))
  graphics::par(mfrow = c(2, 2), mar = c(0.1, 0.1, 2.1, 0.1))
  
  frame <- ceiling(resolution / 2)
  volume <- imager::add.color(volume)
  
  plot(volume, frame = frame, axes = FALSE, main = "1. Initial surface embed")
  
  imager::channel(volume, 2) <- volume_grew - imager::channel(volume, 1)
  plot(volume, frame = frame, axes = FALSE, main = "2. Dilate voxels")
  
  imager::channel(volume, 2) <- volume_shrunk - volume_grew
  plot(volume, frame = frame, axes = FALSE, main = "3. Erode voxels")
  
  imager::channel(volume, 2) <- volume2
  plot(volume, frame = frame, axes = FALSE, main = "4. Filled volume")
  
  do.call(graphics::par, oldPar)
  
})

rm(volume, volume_shrunk, volume_filled, volume_grew)

# Generate surface mesh from volume4
volume2 <- as.array(volume2)
dim(volume2) <- dim(volume2)[c(1,2,3)]
mesh <- Rvcg::vcgIsosurface(volume2, threshold = 0.5, IJK2RAS = IJK2RAS)

debug(sprintf("The initial reconstructed surface volume is %.1f mm^3", suppressWarnings(Rvcg::vcgVolume(mesh))))

mesh_remesh <- Rvcg::vcgUniformRemesh(
  mesh, voxelSize = 1, multiSample = FALSE,
  mergeClost = TRUE, silent = !verbose)
envelope_smoothed <- Rvcg::vcgSmooth(
  mesh_remesh,
  "surfPreserveLaplace",
  lambda = 10,
  delta = 20
)

debug(sprintf("The re-meshed+smoothed surface volume is %.1f mm^3", Rvcg::vcgVolume(envelope_smoothed)))

if(preview) {
  debug("Generating 3D preview (if rgl package is available)...")
}
preview3D(surface_orig, col = 2, func = "shade3d")
preview3D(envelope_smoothed, col = 3)
preview3D()

# save surface
if(length(save_path)) {
  raveio::backup_file(save_path, remove = TRUE, quiet = !verbose)
  face_index <- t(envelope_smoothed$it)
  face_index_start <- min(face_index)
  face_index <- face_index - (face_index_start - 1L)
  freesurferformats::write.fs.surface(
    filepath = save_path,
    vertex_coords = t(envelope_smoothed$vb[c(1, 2, 3), ]),
    faces = face_index,
    format = "bin"
  )
  invisible(envelope_smoothed)
} else {
  envelope_smoothed
}

