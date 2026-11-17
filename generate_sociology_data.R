generate_sociology_data <- function() {
  
  n <- sample(1000:2000, 1)
  p <- sample(15:23, 1)
  
  n_numeric <- floor(p / 2)
  n_categorical <- p - n_numeric
  
  variables <- list(
    
    gender = factor(sample(
      c("Woman", "Man", "Non-binary", "Other",
        "Prefer not to say"),
      n, replace = TRUE,
      prob = c(.48, .45, .03, .01, .03)
    )),
    
    relationship_status = factor(sample(
      c("Single", "Married", "Cohabiting",
        "Divorced", "Widowed"),
      n, replace = TRUE
    )),
    
    education = factor(
      sample(
        c("No qualification", "Secondary", "College",
          "Undergraduate", "Postgraduate"),
        n, replace = TRUE
      ),
      levels = c(
        "No qualification",
        "Secondary",
        "College",
        "Undergraduate",
        "Postgraduate"
      ),
      ordered = TRUE
    ),
    
    employment_status = factor(sample(
      c("Employed", "Unemployed", "Student",
        "Retired", "Not in labour force"),
      n, replace = TRUE
    )),
    
    social_class = factor(sample(
      c("Working class", "Lower middle class",
        "Middle class", "Upper middle class"),
      n, replace = TRUE
    )),
    
    political_interest = factor(
      sample(
        c("Not at all", "Slightly", "Moderately",
          "Very", "Extremely"),
        n, replace = TRUE
      ),
      levels = c(
        "Not at all",
        "Slightly",
        "Moderately",
        "Very",
        "Extremely"
      ),
      ordered = TRUE
    ),
    
    religion = factor(sample(
      c("Christian", "Muslim", "Hindu", "Jewish",
        "Other religion", "No religion"),
      n, replace = TRUE
    )),
    
    ethnicity = factor(sample(
      c("White", "Black", "Asian", "Mixed", "Other"),
      n, replace = TRUE
    )),
    
    region = factor(sample(
      c("North", "Midlands", "London",
        "South East", "South West", "Wales",
        "Scotland", "Northern Ireland"),
      n, replace = TRUE
    )),
    
    housing = factor(sample(
      c("Own outright", "Mortgage", "Private rent",
        "Social rent", "Living with family"),
      n, replace = TRUE
    )),
    
    occupation = factor(charlatan::ch_job(n)),
    
    name = charlatan::ch_name(n),
    
    age = charlatan::ch_integer(
      n = n,
      min = 18,
      max = 85
    ),
    
    income = round(
      exp(charlatan::ch_norm(
        n = n,
        mean = log(35000),
        sd = 0.55
      )),
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
  
  selected_categorical <- sample(
    categorical_names,
    n_categorical
  )
  
  selected_numeric <- sample(
    numeric_names,
    n_numeric
  )
  
  selected <- sample(
    c(selected_categorical, selected_numeric)
  )
  
  data <- as.data.frame(variables[selected])
  
for (j in seq_along(data)) {
  
  # Choose missingness percentage for this column
  missing_prop <- runif(1, 0.03, 0.10)
  
  # Number of missing observations
  n_missing <- round(n * missing_prop)
  
  # Use charlatan to introduce missing values
  data[[j]] <- charlatan::ch_missing(
    data[[j]],
    n = n_missing
  )
}
  
  data
}
