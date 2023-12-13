# RAVE-iEEG Code Snippet

## What's this

This repository contains code snippets to perform `iEEG` (intracranial electroencephalography) analysis with [RAVE](https://rave.wiki/) framework code. RAVE provides powerful graphical user interface (GUI). However, there are more you can do without GUI. This repository is designed for RAVE users who want to do something that hasn't been integrated into RAVE's GUI

## How to use this repository

Most scripts in this repository can be directly sourced into `R`. [[This example](dummy-snippet.R)] provides documentation on how to run the snippet.

### Method 1:

You can download the snippets, un-comment the code that declares the global variables, then run the script just like any other R script.

### Method 2

Use built-in function

## Build your own `rave-gists`

1. Download this repository in zip file
2. Open file `rave-gists.Rproj` using RStudio
3. Run the following command to restore the developing environment

```r
# similar to conda/venv in R
install.packages("renv")

# Download and install packages to current project
renv::restore()

# Ensure python for RAVE (optional but recommended)
rpymat:::ensure_rpymat()
```

An isolated RAVE will be downloaded and installed on your project folder.
