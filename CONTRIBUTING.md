# Contributing
1. [Understanding the basic function structure](/CONTRIBUTING.md#intro)
2. [Using Issues and new branches](/CONTRIBUTING.md#issues)
3. [Versioning Process](/CONTRIBUTING.md#versioning)

# Intro
## Package Strucure
Within a package structure, functions live within the `R/` directory. Each function also a has corresponding `*.md` file that resides in the `man/` directory. The `*.md` files are the help files that appear when `?myfunction` is called or when previewed in RStudio.

```
├── R
│   ├── make_FR711.R
│   ├── make_FR712.R
├── man
│   ├── make_FR711.md
│   ├── make_FR712.md

```

## Roxygen skeleton
The `*.R` file that contains special *roxygen* header text denoted by `#'` that is used to generate the `*.md` file. The *roxygen skeleton* is easily added to the `*.R` file from RStudio when the cursor is inside the function body and then accessing the *magic wand* drop down menu or using *ctrl+alt+shift+r* .

``` r
#' hello creesys
#'
#' @return str "hello creesys"
#' @export
#' @description
#' This function only returns 'hello creesys' but
#' is here as a simple example of how to write functions.
#'
#' @examples
#' hello_creesys()

hello_creesys <- function(){
  "hello creesys"
}

```

## Generating the *.md file
The `*.md` files can be generated by calling `devtools::document`. When contributing functions to `creesysr` however these files will be generated through a [Github action](.github) workflow when the `*.R` file is pushed to the repository. The Github Action recognizes that a new function or changes to an existing function has occurred and will run the `devtools::document` command in a background job and will create or update the `*.md` file and then commit it to the working branch.  

## General function structure
The package analysis workflow will (*generally?*) require functions that accept a dataframe that resembles the Fishnet (FN2) naming conventions and creates a new FN2 table and as such functions should generally be named based on the new table that is generated.

``` r
# load raw data
## FN2 zip files can be read using gfsR::import_creel_series
creel_file <- "Data/SC07_WPR/DATA.ZIP"
wpgcreel <- import_creel_series(creel_file)

# Create table dataframe from list item
FN111 <- wpgcreel$FN111

# Step 1
FR711 <- make_FR711(FN111)

# Step 2
FR712 <- make_FR712(FR711)
```

# Issues
Github has a section for Issues. Using Issues is a good way to keep track of the development process and have conversations about how to solve an issue. New functions should start off by creating an Issue describing the intended role the function will fill. Additionally, a new branch should be created specific to the function. Keeping new functions on a unique branch helps ensure that working versions of the package remain working until sufficient testing and code review has occurred. 

# Versioning
The repo will always contain two working branches: *main* and *development*. Additional branches will be created and (often) deleted as required for new functions or bug fixes. The *main* branch will be reserved for code that is part of a specific release of the package that is deemed stable enough for use by others (i.e v 0.1). The *development* branch should generally be ahead of the *main* branch as features are added to the branch for testing prior to an official release. New branches should generally be created from *development*. The *development* version will increment as functions (v 0.0.1) or important patches (0.0.0.9001) are added through pull requests. Maintaining versioning helps with navigating the version history should there be a need to reset a branch. Also, users are able to re-build the package locally on their machine and having the version information routinely changed advises user of which version is currently loaded.  