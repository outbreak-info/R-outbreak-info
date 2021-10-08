# Steps to update the package down build

# Semi-manual process!
# NOTE: you'll need to supervise this process, because every example that 
# contains a call to a genomics function will require GISAID authentication.


# 1. Knit the README.Rmd file
# Creates README.md
knitr::knit("README.Rmd")

# 2. Build the site
# Note: some of the functions in the examples take a fair bit of time to run.
pkgdown::build_site()

# 3. Push to GitHub; will automatically update GitHub pages.