library(tidyverse)
library(NHSRdatasets)
library(NHSRplotthedots)

# example of comment for a function

# classic dataframe with awful date format


data <- data.frame(
  month_name = c('Jan', 'Feb', 'Jun', 'Apr', 'May', 'Sep', 'Aug'),
  year = c(2022,2022,2022,2023,2023,2023,2023)
)

data

date_convert <- function (data){#' @name date_convert
  #' @title Creates a date object representing the end of the month for each row
  #'
  #' @description This function takes a data frame containing 'year' and 'month_name' 
  #' columns and creates a new column named 'period'. The 'period' column 
  #' represents the last day of the month for each row. It achieves this by:
  #' 
  #' 1. Combining the 'year' and 'month_name' columns into a string with format "YYYY-MM-DD".
  #' 2. Converting the string to a date object.
  #' 3. Using "ceiling_date" function to set the date to the last day of the month.
  #' 4. Subtracting one day to get the end of the month.
  #'
  #' @param data A data frame containing 'year' and 'month_name' columns.
  #'
  #' @return A data frame with an additional column named 'period' containing 
  #' the date object representing the end of the month for each row.
  #'
  #' @examples
  #' # Example data
  #' data <- data.frame(year = c(2023, 2023, 2024), month_name = c("January", "February", "March"))
  #'
  #' # Apply the function
  #' data_with_period <- date_convert(data)
  #'
  #' # Print the resulting data frame
  #' print(data_with_period)
  
}

date_convert(data)


# another example - just commenting line by line
dat <- ae_attendances


dat <- dat %>%
  filter(org_code %in% c('RF4', 'RQM'),
         period > '2017-10-01',
         type %in% c('1'))

plot_f <- function(org) {
  dat <- dat |> filter(org_code == org)

  p <- ptd_spc(dat,
    date_field = period,
    value_field = attendances
  )

  p <- ptd_create_ggplot(p,
    icons_position = "none",
    point_size = 2,
    fixed_y_axis_multiple = FALSE,
    x_axis_date_format = "%b-%y"
  ) +
    labs(
      y = "Number of attendances",
      x = ""
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  p
}

snippet
# Filter data based on specific criteria
dat <- dat %>%
  # Filter rows where 'org_code' is in ('RF4', 'RQM')
  filter(org_code %in% c('RF4', 'RQM')) %>%
  # Filter rows where 'period' is greater than '2017-10-01'
  filter(period > '2017-10-01') %>%
  # Filter rows where 'type' is in ('1')
  filter(type %in% c('1'))

# Define a function to create a plot
plot_f <- function(org) {
  # Filter data for the specific organization
  dat_filtered <- dat %>%
    filter(org_code == org)
  
  # Create plot using ptd_spc function
  p <- ptd_spc(dat_filtered,
               # Set 'period' as the date field
               date_field = period,
               # Set 'attendances' as the value field
               value_field = attendances
  )
  
  # Modify the plot using ptd_create_ggplot function
  p <- ptd_create_ggplot(p,
                         # No icons on the plot
                         icons_position = "none",
                         # Set point size to 2
                         point_size = 2,
                         # Don't fix y-axis multiple
                         fixed_y_axis_multiple = FALSE,
                         # Set x-axis date format
                         x_axis_date_format = "%b-%y"
  ) +
    # Set axis labels
    labs(
      y = "Number of attendances",
      x = ""
    ) +
    # Apply minimal theme
    theme_minimal() +
    # Remove legend
    theme(legend.position = "none")
  
  # Return the modified plot
  return(p)
}
Use code with caution.

plot_f('RF4')
