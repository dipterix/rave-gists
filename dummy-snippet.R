#' @author Zhengjia Wang
#' @date Feb 08, 2023
#' @license Apache-2.0
#' 
#' @title This file is used to demonstrate how rave-snippet works
#' 
#' A RAVE code snippet contains three sections: 
#'   * documentation section, 
#'   * variable section, 
#'   * code section.
#' 
#' This documentation is located at `documentation section`, starting with two 
#' characters `#'`. The section ends with `#' END OF DOC`. 
#' 
#' The documentation section describes what the code snippets do, and what are
#' the inputs and outputs, and possibly with examples.
#' 
#' The variable section starts right after `#' END OF DOC`. They are usually 
#' commented for a reason (see 'How to use snippets' below). You can download 
#' and un-comment these lines and run snippets as standalone scripts.
#' 
#' The last section is code section, usually you don't have to change the magic
#' there unless you know what you are doing.
#' 
#' @section How to use rave-snippets:
#' 
#' Scenario 1: You can download the script to your local disk, from there
#' un-comment the *variable section*, set variable as required, and run the
#' whole script using `ctrl/command+shift+enter` (or `command+enter` to run 
#' line by line)
#' 
#' Scenario 2: You can use `rave` built-in functions to load the snippet:
#' 
#' 
