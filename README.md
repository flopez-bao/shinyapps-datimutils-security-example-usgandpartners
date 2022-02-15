The following example app was built to demonstrate the use of functionality in datimutils for securing data access specifically for data meant to be accessible to USG Folks and Partners. In order to run this application do the following:

1. Clone the repo.
2. Open as a project.
3. If you don’t already have `renv` installed make sure to install it and then run `renv::activate()` — you may be prompted to follow up with `renv::restore()`.
4. You will also be asked to install all the packages listed in the `renv.lock` file.
5. Edit your `.Rprofile` in your root directory where you can point to datim.
6. Reload the project so that the environment variables are available to the app.
7. Ensure your shiny app is set to run externally in a web browser as opposed to the Rstudio IDE.
8. You can now run the app.

Front end directions once app is launched from Rstudio.

* In order to log in with OAuth. Note if you have already logged into this instance, today, steps 3 and 4 may be skipped based on your session activity
  1. Click "Log in with DATIM"
  2. You will be redirected to the datim instance url coded in the app
  3. Agree to the usage terms
  4. Sign in with your credentials
  5. Click "Authorize"
  6. You will then be redirect to the Shiny app

* In order to log in traditionally
  1. Enter your credentials on the shiny app
  2. Click "Log in!"
