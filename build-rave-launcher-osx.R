#' @title Build `rave-2.0.app` on OSX
#' @description Build startup application for RAVE. Must run in Mac OSX
#' @returns Nothing
#' @examples
#' 
#' # Make sure brew and node.js have been installed to your mac
#' # If you haven't, please open a new bash terminal and run:
#' #
#' # /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
#' # brew install node
#' 
#' build <- raveio::load_snippet("build-rave-launcher-osx", local = FALSE)
#' build()
#' 
#' END OF DOC
NULL

# Make sure brew exists
if(dipsaus::get_os() != "darwin") {
  stop("This script only works on Mac OSX")
}
# make sure brew exists
brew <- raveio::cmd_homebrew(error_on_missing = TRUE)
if(Sys.which("npm") == "") {
  system2(brew, "install node")
}
if(Sys.which("npm") == "") {
  npm <- file.path(dirname(brew), "npm", fsep = "/")
} else {
  npm <- Sys.which("npm")
}
npm <- normalizePath(npm, winslash = "/")

build_path <- normalizePath(file.path(tempdir(), "rave-electron-app-builder"), winslash = "/", mustWork = FALSE)

if(file.exists(build_path)) { unlink(build_path, recursive = TRUE, force = TRUE) }

dir.create(build_path, showWarnings = FALSE, recursive = TRUE)

utils::download.file("https://github.com/dipterix/rave-electron-app/archive/refs/heads/main.zip", destfile = file.path(build_path, "main.zip"))

utils::unzip(file.path(build_path, "main.zip"), exdir = build_path)

src_path <- file.path(build_path, "rave-electron-app-main")

cwd <- getwd()
on.exit({
  setwd(cwd)
  unlink(build_path, recursive = TRUE, force = TRUE)
}, add = TRUE)

setwd(src_path)

system2(npm, "install")
system2(npm, "run make")

# check if the platform is ARM

# /private/var/folders/bs/n0q8wqv931g89ppshhgp2m2m0000gn/T/RtmplBTv57/rave-electron-app-builder/rave-electron-app-main/out/make
fs <- list.files("./out", pattern = "^rave-2\\.0\\.app$", recursive = TRUE, all.files = TRUE, ignore.case = TRUE, full.names = TRUE, include.dirs = TRUE)
fs <- fs[[1]]

tryCatch({
  dir.create("/Applications/RAVE", showWarnings = FALSE, recursive = TRUE)
  file.copy(fs, "/Applications/RAVE", overwrite = TRUE, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
  system2("open", "/Applications/RAVE")
}, error = function(e) {
  path <- dirname(normalizePath(fs, winslash = "/"))
  system2("open", shQuote(path))
})

invisible()
