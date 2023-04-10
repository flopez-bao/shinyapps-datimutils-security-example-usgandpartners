# README

The following example app was built to demonstrate the functionality in datimutils for restricting data access depending on whether the user is a USG or Partner client. Developers can use this application as a template when needing to restrict data streams based on user membership.

## Installation
- clone this repo
- make sure you have `renv` installed, if not run `install.packages('renv')`
- run `renv::activate()`
- open the newly created `.Rprofile` in your root directory and set up your env variables,
reference `example_Rprofile.R` to see what your `.Rprofile` should look like.
- open .gitignore and add `.Rprofile` to the list
- restart your r session
- run `renv::restore()` to restore your environment -- this essentially makes sure your environment
reflects what is in the `renv.lock file` in this repo
- you should now be able to run the application and/or the scripts

## Usage

Reference the `server` and `ui` files for a basic app example with code for
logging in and out with oauth and then restricting app access and/or data stream access
based on the user type. 

## Important

In order to run the application you must set up environment variables. 
Reference `example_Rprofile.R` to see what your `.Rprofile` should look like.