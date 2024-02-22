# ous-shiny-demo

This is an example shiny dashboard used for monitoring Ocean Use Survey results from [SeaSketch](https://www.seasketch.org/). The data contained in this repo were entirely fabricated for demonstration purposes.

## Use as a framework

This app is designed to be both a [functional example](https://seasketch.shinyapps.io/ous-shiny-demo/) and a template that can be adapted to monitor other, real SeaSketch surveys. The goal was to generalize as much of the code as possible, allowing users to make minimal changes to apply it to their unique survey. That being said, surveys in SeaSketch can vary widely, and the amount of work required to adapt it to a given survey will also vary.

## Adaptation workflow

These are the basic steps required to adapt this framework to your survey

-   Clone the repo
-   Replace `data/responses.csv` and `data/shapes.json` with the respective survey exports from your SeaSketch project
    -   Note that the spatial data from SeaSketch will defualt to a different name - change the file name to `shapes.json`
-   Replace `data/eez.fgb` with a flatgeobuf containing the EEZ or other study area polygon applicable to your project
-   Add `data/survey_targets.csv` with your own targets CSV containing the columns "sector" and "target"
-   Edit `R/project_variables.R` - this script contains all project-specific variables:
    -   `project` is the prefix added to all downloads from the app
    -   `app_title` will be diplayed in the upper left hand corner of the app ui
    -   `seasketch_url` should be the link to your seasketch project page and is accessible via the "Responses Updated:" box on the "Overview" tab
    -   `sectors` is a character vector of ocean use sectors in your project
    -   `region` is the export id of your region question
    -   `region_list` is a character vector of the regions respondents can choose
    -   `age_groups` is a character vector of the age groups respondents can choose from
    -   `genders` is a character vector of the genders respondents can choose from
    -   `json_columns` is a dataframe containing the export ids of any group response questions (which export as JSON-formatted strings), and their respective answer option objects (`age_groups` and `genders` in this example) - this excludes `region` because it is parsed differently from the other group response questions
    -   `columns_to_remove` is a character vector of any columns you don't need in your cleaned data
    -   `shape_attributes_to_keep` is a character vector of shape attributes you want to keep in your cleaned spatial data
-   Run `R/data_init.R` - this will process the data in `responses.csv` and `shapes.json`, writing five `*.RDS` files to `data/temp`:
    -   `respondent_info.RDS` contains aggregated response data, with one row per survey response (similar to how it's exported from SeaSketch)
    -   `responses.RDS` contains response data with one row per sector response
    -   `shapes.RDS` contains the spatial data joined with other response data in the form of an `SF` dataframe
    -   `data_date.RDS` contains the date that `responses.csv` was created
    -   `temp_data_date.RDS` contains the date that the files in `data/temp` were created (i.e. when `R/data_init.R` was run)
-   Secure app
    -   If your app doesn't require authentication, skip to the last bullet in this section - otherwise, follow the steps below
    -   Create user database
        -   Source `auth/create_user_db.R`
        -   Run `create_user_db({initial_user}, {password}, {passphrase})` with the arguments:
            -   `initial_user`: your desired username to access the app
            -   `password`: your desired password
            -   `passphrase` your desired passphrase to decrypt the database
        -   This will create `auth/users.sqlite`
        -   This app implementation stores the passphrase in a (gitignored) plaintext file `auth/passphrase.txt` which is read in `global.R` - consider whether this is secure enough for your needs
        -   Once the app is up and running you can use initial credentials created with `create_user_db()` to login and any additional users can be added from the [shinymanager](https://github.com/datastorm-open/shinymanager) admin interface accessible via the plus icon in the lower righthand corner of the app
    -   Add `R/secure_option.R` which should assign `TRUE` to the variable `secure` - this is intended to be an easy gitignored way to develop locally with `secure` set to `FALSE`. Just make sure it's set to `TRUE` wherever it's deployed. If you don't need your app secured, just set `secure` to `FALSE` and call it a day

## Updating data

Currently, SeaSketch doesn't have a developer API for requesting survey data. Updates will need to be done by manually adding the latest `responses.csv` and `shapes.json` files exported from SeaSketch to the shiny app repo.
