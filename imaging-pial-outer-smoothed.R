#' @author Zhengjia Wang (copyright reserved)
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
#' surface_path <- '~/rave_data/raw_dir/DemoSubject/rave-imaging/fs/surf/lh.pial'
#' envelope <- snippet(surface_path = surface_path)
#' 
#' if(dipsaus::package_installed("rgl")) {
#'   rgl::shade3d(envelope, col = 2)
#' }
#' 
#' END OF DOC
NULL

# ---- Variable Section --------------------------------------------------------

# surface_path <- '~/Dropbox (PENN Neurotrauma)/RAVE/Samples/raw/PAV010/rave-imaging/fs/surf/lh.pial'
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
preview3D <- function(..., func = "wire3d") {
  if(isTRUE(get0("preview", ifnotfound = FALSE)) && dipsaus::package_installed("rgl")) {
    options("rgl.startQuartz" = FALSE)
    rgl <- asNamespace("rgl")
    rgl[[func]](...)
  }
}
preview2D <- function(expr) {
  if(isTRUE(get0("preview", ifnotfound = FALSE))) {
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

# Creating a volume
volume <- array(0L, dim = c(rep(resolution, 3), 1))

# embed the surface in volume space 
surface_index <- round(surface$vb[c(1,2,3), ])
surface_index <- surface_index[1, ] + surface_index[2, ] * resolution + 
  surface_index[3, ] * (resolution^2) + 1
volume[surface_index] <- 1L

# convert to cimg object so imager can handle it
volume <- imager::as.cimg(volume)

# Grow the volume by 15 voxels and shrink back. This step connects the 
# segmented voxels into a shell that is water-tight
volume_filled <- imager::fill(volume, 15)

# bucket-fill from the corner 
volume_filled2 <- imager::bucketfill(volume_filled, x = 1, y = 1, z = 1, color = 1L)

# Fill the voxels within the surface
volume2 <- 1L - (volume_filled2 - volume_filled)

# Clean, remove "isolated" voxels by shrink first and grow
volume3 <- imager::clean(volume2, 3)
# image((imager::grow(volume3, 3)[127,,] - volume3[128,,]))

# Further row the image by 3 voxels
volume4 <- imager::grow(volume3, 3)

# preview
preview2D({
  oldPar <- graphics::par(c("mfrow", "mar"))
  graphics::par(mfrow = c(2, 3), mar = c(0.1, 0.1, 2.1, 0.1))
  
  frame <- ceiling(resolution / 2)
  volume <- imager::add.color(volume)
  
  plot(volume, frame = frame, axes = FALSE, main = "1. Initial surface embed")
  
  imager::channel(volume, 2) <- volume_filled
  plot(volume, frame = frame, axes = FALSE, main = "2. Grow 15 voxels+shrink back")
  
  imager::channel(volume, 2) <- volume_filled2
  plot(volume, frame = frame, axes = FALSE, main = "3. Bucket-fill outside")
  
  imager::channel(volume, 2) <- volume2
  plot(volume, frame = frame, axes = FALSE, main = "4. 1-XOR(2, 3)")
  
  imager::channel(volume, 2) <- volume3
  plot(volume, frame = frame, axes = FALSE, main = "5. Remove thin edges")
  
  imager::channel(volume, 1) <- volume4 - volume3
  plot(volume, frame = frame, axes = FALSE, main = "6. Expand 5 by 3 voxels")
  
  do.call(graphics::par, oldPar)
  
})

rm(volume, volume2, volume3, volume_filled, volume_filled2)

# Generate surface mesh from volume4
volume4 <- as.array(volume4)
dim(volume4) <- dim(volume4)[c(1,2,3)]
mesh <- Rvcg::vcgIsosurface(volume4, threshold = 0.5, IJK2RAS = IJK2RAS)

debug(sprintf("The initial reconstructed surface volume is %.1f mm^3", Rvcg::vcgVolume(mesh)))

mesh_remesh <- Rvcg::vcgUniformRemesh(mesh, voxelSize = 1, multiSample = FALSE,
                                      mergeClost = TRUE, silent = !verbose)
envelope_smoothed <- Rvcg::vcgSmooth(
  mesh_remesh,
  "surfPreserveLaplace",
  lambda = 10,
  delta = 20
)

debug(sprintf("The re-meshed+smoothed surface volume is %.1f mm^3", Rvcg::vcgVolume(envelope_smoothed)))

preview3D(surface_orig, col = 2, func = "shade3d")
preview3D(envelope_smoothed, col = 3)

# save surface
if(length(save_path)) {
  raveio::backup_file(save_path, remove = TRUE, quiet = !verbose)
  face_index <- t(envelope_smoothed$it)
  face_index_start <- min(face_index)
  face_index <- face_index - (face_index_start - 1L)
  freesurferformats::write.fs.surface(
    filepath = save_path,
    vertex_coords = t(envelope_smoothed$vb[c(1, 2, 3 )]),
    faces = face_index,
    format = "bin"
  )
  invisible(envelope_smoothed)
} else {
  envelope_smoothed
}

