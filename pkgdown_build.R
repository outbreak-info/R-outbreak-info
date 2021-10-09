# Steps to update the package down build

# Semi-manual process!
# NOTE: you'll need to supervise this process, because every example that
# contains a call to a genomics function will require GISAID authentication.

update_package_website = function(auth_token, commit_message = "update R outbreakinfo package website"){
  # Set the authentication token
  Sys.setenv(OUTBREAK_INFO_TOKEN = auth_token)

  # 1. Knit the README.Rmd file
  # Creates README.md
  knitr::knit("README.Rmd")

  # 2. Build the site
  # Note: some of the functions in the examples take a fair bit of time to run.
  pkgdown::build_site()

  # 3. Push to GitHub; will automatically update GitHub pages.
  gert::git_commit_all(commit_message)
  gert::git_push()
}
