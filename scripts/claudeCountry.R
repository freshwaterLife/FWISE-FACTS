fwise_countries_wins_2026 <- c( 
  # as of september 2026
  # win defined as outcome == "Successful" &
  # reason %in% c("Ecosystem/habitat restoration", "Native/endangered species", "Unknown"),
  # exact definition may be different spelling in column and factors in final cleaned data set
  "Argentina", "Australia", "Austria", "Bermuda", "Canada", "Ecuador", "Finland", "Italy", "Japan",
  "Lithuania", "Mexico", "New Zealand", "Norway", "Palau", "Poland", "South Africa", "Spain", "Sweden",
  "United Arab Emirates", "United Kingdom", "United States of America"
)

dupe_country_check <- function(df, col = country) {
  
  df %>% 
    count({{ col }}, name = "n_rows") %>% 
    filter(n_rows > 1) %>% 
    arrange(desc(n_rows))
  
}

dupe_country_species_check <- function(df) {
  
  df %>% 
    semi_join(
      df %>% 
        count(country, scientificName) %>% 
        filter(n > 1),
      by = c("country", "scientificName")
    ) %>% 
    arrange(country, scientificName) %>% 
    select(country, scientificName, OrigDB, pathwayMainCat, firstRecord, establishmentMeans)
  
}

fix_country <- function(df, col = country, ref = NULL) {
  
  lookup <- c(
    # gfid (Natural Earth short names)
    "Serbia"                 = "Republic of Serbia",
    "Dominican Rep."         = "Dominican Republic",
    "Bosnia and Herz."       = "Bosnia and Herzegovina",
    "Tanzania"               = "United Republic of Tanzania",
    "Dem. Rep. Congo"        = "Democratic Republic of the Congo",
    "Congo"                  = "Republic of the Congo",
    "Central African Rep."   = "Central African Republic",
    "Micronesia"             = "Federated States of Micronesia",
    "Bahamas"                = "The Bahamas",
    "Antigua and Barb."      = "Antigua and Barbuda",
    "St. Vin. and Gren."     = "Saint Vincent and the Grenadines",
    "Cook Is."               = "Cook Islands",
    "Marshall Is."           = "Marshall Islands",
    "Cayman Is."             = "Cayman Islands",
    "Timor-Leste"            = "East Timor",
    "Hong Kong"              = "Hong Kong S.A.R.",
    "Côte d'Ivoire"          = "Ivory Coast",
    "Côte d’Ivoire"          = "Ivory Coast",
    "North & Baltic Seas"    = NA_character_,
    # shoal
    "Macedonia"               = "North Macedonia",
    "Bonaire"                 = NA_character_,
    "Sint Eustatius and Saba" = NA_character_
  )
  
  nm  <- rlang::as_name(rlang::enquo(col))
  org <- paste0(nm, "_original")
  
  out <- df %>% 
    mutate(
      !!org := as.character({{ col }}),
      .clean = str_squish(str_remove_all(.data[[org]], '"')),
      {{ col }} := if_else(.clean %in% names(lookup), unname(lookup[.clean]), .clean)
    ) %>% 
    select(-.clean) %>% 
    relocate(all_of(org), .after = {{ col }})
  
  if (!is.null(ref)) {
    
    bad <- out %>% 
      pull({{ col }}) %>% 
      na.omit() %>% 
      setdiff(ref)
    
    if (length(bad)) warning("unmatched: ", paste(bad, collapse = ", "))
    
  }
  
  out
  
}

add_continent <- function(df, col = country, into = continent) {
  
  regions <- list(
    latin_america = c(
      "Mexico", "Guatemala", "Belize", "El Salvador", "Honduras", "Nicaragua", "Costa Rica", "Panama",
      "Colombia", "Venezuela", "Guyana", "Suriname", "Ecuador", "Peru", "Bolivia", "Brazil", "Paraguay", "Chile",
      "Argentina", "Uruguay", "Cuba", "Haiti", "Dominican Republic", "Jamaica", "The Bahamas", "Puerto Rico",
      "Trinidad and Tobago", "Barbados", "Grenada", "Saint Lucia", "Saint Kitts and Nevis",
      "Saint Vincent and the Grenadines", "Antigua and Barbuda", "Dominica", "Anguilla", "Aruba",
      "British Virgin Islands", "Cayman Islands", "Curaçao", "Montserrat", "Saint Barthelemy", "Saint Martin",
      "Sint Maarten", "Turks and Caicos Islands"
    ),
    north_america = c(
      "United States of America", "Canada", "Bermuda", "Greenland", "Saint Pierre and Miquelon"
    ),
    europe = c(
      "Switzerland", "Greece", "Croatia", "Albania", "Italy", "North Macedonia", "United Kingdom",
      "Bosnia and Herzegovina", "France", "Russia", "Germany", "Austria", "Spain", "Bulgaria", "Portugal",
      "Romania", "Ireland", "Ukraine", "Montenegro", "Moldova", "Republic of Serbia", "Iceland", "Belgium",
      "Netherlands", "Norway", "Denmark", "Sweden", "Slovakia", "Czechia", "Poland", "Belarus", "Latvia",
      "Hungary", "Finland", "Slovenia", "Lithuania", "Estonia", "Luxembourg", "Malta", "Faroe Islands",
      "Guernsey", "Isle of Man", "Jersey", "Monaco", "Liechtenstein"
    ),
    north_africa = c(
      "Morocco", "Algeria", "Tunisia", "Libya", "Egypt"
    ),
    sub_saharan_africa = c(
      "United Republic of Tanzania", "Uganda", "Madagascar", "South Africa", "Kenya", "Cameroon", "Zambia",
      "Democratic Republic of the Congo", "eSwatini", "Namibia", "Mozambique", "Ghana", "Mauritius", "Zimbabwe",
      "Rwanda", "Malawi", "Burundi", "Mauritania", "Ethiopia", "Republic of the Congo", "Ivory Coast", "Nigeria",
      "Central African Republic", "Angola", "Gabon", "Eritrea", "Lesotho", "Botswana", "Cabo Verde", "Togo",
      "Comoros", "Liberia", "Seychelles", "São Tomé and Principe", "Benin", "Burkina Faso", "Guinea", "Senegal",
      "Sierra Leone", "Sudan"
    ),
    west_central_asia = c(
      "Turkey", "Cyprus", "Georgia", "Armenia", "Azerbaijan", "Syria", "Lebanon", "Israel", "Jordan", "Iraq",
      "Iran", "Saudi Arabia", "Kuwait", "Bahrain", "United Arab Emirates", "Oman", "Kazakhstan", "Uzbekistan",
      "Turkmenistan", "Kyrgyzstan", "Tajikistan", "Afghanistan", "Pakistan"
    ),
    east_asia = c(
      "China", "Japan", "South Korea", "North Korea", "Taiwan", "Hong Kong S.A.R.", "Mongolia", "Indonesia",
      "Philippines", "Malaysia", "Singapore", "Brunei", "Thailand", "Vietnam", "Cambodia", "Laos", "Myanmar",
      "East Timor", "Papua New Guinea", "India", "Bangladesh", "Sri Lanka", "Nepal", "Bhutan", "Maldives"
    ),
    oceania = c(
      "Australia", "New Zealand", "Federated States of Micronesia", "Guam", "New Caledonia", "Fiji", "Samoa",
      "Palau", "Vanuatu", "Cook Islands", "Marshall Islands", "Kiribati", "Solomon Islands", "American Samoa",
      "Nauru", "Niue", "Tonga", "Tuvalu"
    )
  )
  
  lookup <- setNames(rep(names(regions), lengths(regions)), unlist(regions, use.names = FALSE))
  
  out <- df %>% 
    mutate({{ into }} := unname(lookup[as.character({{ col }})])) %>% 
    relocate({{ into }}, .after = {{ col }})
  
  bad <- out %>% 
    filter(is.na({{ into }}), !is.na({{ col }})) %>% 
    pull({{ col }}) %>% 
    unique()
  
  if (length(bad)) warning("no continent for: ", paste(bad, collapse = ", "))
  
  out
  
}