## RAVE-iEEG Code Snippet

### What's this

This repository contains code snippets to perform `iEEG` (intracranial electroencephalography) analysis with [RAVE](https://rave.wiki/) framework code. RAVE provides powerful graphical user interface (GUI). However, there are more you can do without GUI. This repository is designed for RAVE users who want to do something that hasn't been integrated into RAVE's GUI

### How to use this repository

Most scripts in this repository can be directly sourced into `R`. [[This example](dummy-snippet.R)] provides documentation on how to run the snippet.

### Topics

#### Format conversion

* `fst` to `HDF5` [[link](fileformat-convert-fst-to-hdf5.R)] [topic: fileformat-convert-fst-to-hdf5]

#### Electrode localization, coordinates

* Convert electrode coordinates: from `tkrRAS` or `scannerRAS` to MNI305 or MNI152 [[link](coordinate-compute-MNI-from-tkrRAS-or-scannerRAS.R)] [topic: coordinate-compute-MNI-from-tkrRAS-or-scannerRAS] 

