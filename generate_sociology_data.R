```r

generate_sociology_data <- function() {
  
  # ------------------------------------------------------------
  # 1. Randomly choose number of rows and columns
  # ------------------------------------------------------------
  
  n <- sample(1000:2000, 1)
  p <- sample(15:23, 1)
  
  # ------------------------------------------------------------
  # 2. Approximately half categorical, half numeric
  # ------------------------------------------------------------
  
  n_numeric <- floor(p / 2)
  n_categorical <- p - n_numeric
  
  # ------------------------------------------------------------
  # 3. Generate possible variables
  # ------------------------------------------------------------
  
  variables <- list(
    
    # ---------- CATEGORICAL ----------
    
    gender = factor(sample(
      c("Woman", "Man", "Non-binary", "Other",
        "Prefer not to say"),
      n, replace = TRUE,
      prob = c(.48, .45, .03, .01, .03)
    )),
    
    relationship_status = factor(sample(
      c("Single", "Married", "Cohabiting",
        "Divorced", "Widowed"),
      n, replace = TRUE,
      prob = c(.35, .25, .20, .12, .08)
    )),
    
    education = factor(
      sample(
        c("No qualification", "Secondary", "College",
          "Undergraduate", "Postgraduate"),
        n, replace = TRUE,
        prob = c(.05, .25, .20, .30, .20)
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
      n, replace = TRUE,
      prob = c(.55, .08, .15, .12, .10)
    )),
    
    social_class = factor(sample(
      c("Working class", "Lower middle class",
        "Middle class", "Upper middle class"),
      n, replace = TRUE,
      prob = c(.25, .25, .35, .15)
    )),
    
    political_interest = factor(
      sample(
        c("Not at all", "Slightly", "Moderately",
          "Very", "Extremely"),
        n, replace = TRUE,
        prob = c(.12, .18, .30, .28, .12)
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
      n, replace = TRUE,
      prob = c(.35, .08, .04, .01, .07, .45)
    )),
    
    ethnicity = factor(sample(
      c("White", "Black", "Asian", "Mixed", "Other"),
      n, replace = TRUE,
      prob = c(.65, .10, .12, .08, .05)
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
      n, replace = TRUE,
      prob = c(.20, .30, .25, .15, .10)
    )),
    
    occupation = factor(ch_job(n)),
    
    name = ch_name(n),
    
    
    # ---------- NUMERIC ----------
    
    age = ch_integer(
      n = n,
      min = 18,
      max = 85
    ),
    
    income = round(
      exp(ch_norm(
        n = n,
        mean = log(35000),
        sd = 0.55
      )),
      0
    ),
    
    hours_worked = round(
      pmax(
        0,
        ch_norm(
          n = n,
          mean = 35,
          sd = 10
        )
      ),
      1
    ),
    
    life_satisfaction = ch_integer(
      n = n,
      min = 0,
      max = 10
    ),
    
    political_scale = ch_integer(
      n = n,
      min = 0,
      max = 10
    ),
    
    trust_institutions = ch_integer(
      n = n,
      min = 0,
      max = 10
    ),
    
    social_media_hours = round(
      ch_unif(
        n = n,
        min = 0,
        max = 8
      ),
      1
    ),
    
    household_size = ch_integer(
      n = n,
      min = 1,
      max = 8
    ),
    
    children = ch_integer(
      n = n,
      min = 0,
      max = 5
    ),
    
    commute_minutes = ch_integer(
      n = n,
      min = 0,
      max = 120
    ),
    
    neighbourhood_safety = ch_integer(
      n = n,
      min = 0,
      max = 10
    ),
    
    volunteering_hours = round(
      ch_unif(
        n = n,
        min = 0,
        max = 20
      ),
      1
    )
  )
  
  
  # ------------------------------------------------------------
  # 4. Select variables
  # ------------------------------------------------------------
  
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
  
  
  # ------------------------------------------------------------
  # 5. Add missing values
  # ------------------------------------------------------------
  
  # Each column gets its own missing-value percentage,
  # randomly chosen between 3% and 10%.
  
  for (j in seq_along(data)) {
    
    missing_prop <- runif(
      1,
      min = 0.03,
      max = 0.10
    )
    
    n_missing <- round(n * missing_prop)
    
    missing_rows <- sample(
      seq_len(n),
      size = n_missing,
      replace = FALSE
    )
    
    data[missing_rows, j] <- NA
  }
  
  
  # ------------------------------------------------------------
  # 6. Return dataset
  # ------------------------------------------------------------
  
  return(data)
}
```
