generate_sociology_data <- function() {

  # Random number of rows and columns
  n <- sample(1000:2000, 1)
  p <- sample(15:23, 1)

  # Approximately half numeric and half categorical variables
  n_numeric <- floor(p / 2)
  n_categorical <- p - n_numeric

  # Generate all possible variables
  variables <- list(

    # Categorical variables
    gender = factor(
      sample(
        c("Male", "Female", "Other"),
        n,
        replace = TRUE
      )
    ),

    relationship_status = factor(
      sample(
        c("Single", "Married", "Divorced", "Widowed"),
        n,
        replace = TRUE
      )
    ),

    education = factor(
      sample(
        c("Primary", "Secondary", "Bachelor", "Master", "PhD"),
        n,
        replace = TRUE
      ),
      levels = c(
        "Primary",
        "Secondary",
        "Bachelor",
        "Master",
        "PhD"
      ),
      ordered = TRUE
    ),

    employment_status = factor(
      sample(
        c("Employed", "Unemployed", "Student", "Retired"),
        n,
        replace = TRUE
      )
    ),

    social_class = factor(
      sample(
        c("Working", "Middle", "Upper-middle", "Upper"),
        n,
        replace = TRUE
      )
    ),

    political_interest = factor(
      sample(
        c("Very low", "Low", "Medium", "High", "Very high"),
        n,
        replace = TRUE
      ),
      levels = c(
        "Very low",
        "Low",
        "Medium",
        "High",
        "Very high"
      ),
      ordered = TRUE
    ),

    religion = factor(
      sample(
        c("None", "Christian", "Muslim", "Hindu", "Other"),
        n,
        replace = TRUE
      )
    ),

    ethnicity = factor(
      sample(
        c("Group A", "Group B", "Group C", "Group D", "Other"),
        n,
        replace = TRUE
      )
    ),

    region = factor(
      sample(
        c("North", "South", "East", "West", "Central"),
        n,
        replace = TRUE
      )
    ),

    housing = factor(
      sample(
        c("Own", "Rent", "Family", "Other"),
        n,
        replace = TRUE
      )
    ),

    occupation = factor(
      charlatan::ch_job(n)
    ),

    name = charlatan::ch_name(n),

    # Numeric variables
    age = charlatan::ch_integer(
      n = n,
      min = 18,
      max = 85
    ),

    income = round(
      exp(
        charlatan::ch_norm(
          n = n,
          mean = log(35000),
          sd = 0.55
        )
      ),
      0
    ),

    hours_worked = round(
      pmax(
        0,
        charlatan::ch_norm(
          n = n,
          mean = 35,
          sd = 10
        )
      ),
      1
    ),

    life_satisfaction = charlatan::ch_integer(
      n = n,
      min = 0,
      max = 10
    ),

    political_scale = charlatan::ch_integer(
      n = n,
      min = 0,
      max = 10
    ),

    trust_institutions = charlatan::ch_integer(
      n = n,
      min = 0,
      max = 10
    ),

    social_media_hours = round(
      charlatan::ch_unif(
        n = n,
        min = 0,
        max = 8
      ),
      1
    ),

    household_size = charlatan::ch_integer(
      n = n,
      min = 1,
      max = 8
    ),

    children = charlatan::ch_integer(
      n = n,
      min = 0,
      max = 5
    ),

    commute_minutes = charlatan::ch_integer(
      n = n,
      min = 0,
      max = 120
    ),

    neighbourhood_safety = charlatan::ch_integer(
      n = n,
      min = 0,
      max = 10
    ),

    volunteering_hours = round(
      charlatan::ch_unif(
        n = n,
        min = 0,
        max = 20
      ),
      1
    )
  )

  # Names of variables by type
  categorical_names <- c(
    "gender",
    "relationship_status",
    "education",
    "employment_status",
    "social_class",
    "political_interest",
    "religion",
    "ethnicity",
    "region",
    "housing",
    "occupation",
    "name"
  )

  numeric_names <- c(
    "age",
    "income",
    "hours_worked",
    "life_satisfaction",
    "political_scale",
    "trust_institutions",
    "social_media_hours",
    "household_size",
    "children",
    "commute_minutes",
    "neighbourhood_safety",
    "volunteering_hours"
  )

  # Randomly select approximately half of the variables
  selected_categorical <- sample(
    categorical_names,
    n_categorical
  )

  selected_numeric <- sample(
    numeric_names,
    n_numeric
  )

  selected <- sample(
    c(
      selected_categorical,
      selected_numeric
    )
  )

  # Create data frame
  data <- as.data.frame(
    variables[selected]
  )

  # Add 3-10% missing values to EVERY column
  # This preserves factor and ordered-factor classes
  for (j in seq_along(data)) {

    missing_prop <- runif(
      1,
      min = 0.03,
      max = 0.10
    )

    n_missing <- round(
      n * missing_prop
    )

    missing_positions <- sample(
      seq_len(n),
      size = n_missing,
      replace = FALSE
    )

    data[[j]][missing_positions] <- NA
  }

  # Return the generated dataset
  return(data)
}
