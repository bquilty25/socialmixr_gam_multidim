#' Generate a contact matrix from diary survey data
#'
#' Samples a contact survey
#'
#' @param survey a [survey()] object
#' @param countries limit to one or more countries; if not given, will use all countries in the survey; these can be given as country names or 2-letter (ISO Alpha-2) country codes
#' @param survey.pop survey population -- either a data frame with columns 'lower.age.limit' and 'population', or a character vector giving the name(s) of a country or countries from the list that can be obtained via `wpp_countries`; if not given, will use the country populations from the chosen countries, or all countries in the survey if `countries` is not given
#' @param age.limits lower limits of the age groups over which to construct the matrix
#' @param filter any filters to apply to the data, given as list of the form (column=filter_value) - only contacts that have 'filter_value' in 'column' will be considered. If multiple filters are given, they are all applied independently and in the sequence given.
#' @param counts whether to return counts (instead of means)
#' @param symmetric whether to make matrix symmetric, such that \eqn{c_{ij}N_i = c_{ji}N_j}.
#' @param split whether to split the contact matrix into the mean number of contacts, in each age group (split further into the product of the mean number of contacts across the whole population (`mean.contacts`), a normalisation constant (`normalisation`) and age-specific variation in contacts (`contacts`)), multiplied with an assortativity matrix (`assortativity`) and a population multiplier (`demograpy`). For more detail on this, see the "Getting Started" vignette.
#' @param sample.participants whether to sample participants randomly (with replacement); done multiple times this can be used to assess uncertainty in the generated contact matrices. See the "Bootstrapping" section in the vignette for how to do this..
#' @param estimated.participant.age if set to "mean" (default), people whose ages are given as a range (in columns named "..._est_min" and "..._est_max") but not exactly (in a column named "..._exact") will have their age set to the mid-point of the range; if set to "sample", the age will be sampled from the range; if set to "missing", age ranges will be treated as missing
#' @param estimated.contact.age if set to "mean" (default), contacts whose ages are given as a range (in columns named "..._est_min" and "..._est_max") but not exactly (in a column named "..._exact") will have their age set to the mid-point of the range; if set to "sample", the age will be sampled from the range; if set to "missing", age ranges will be treated as missing
#' @param missing.participant.age if set to "remove" (default), participants without age information are removed; if set to "keep", participants with missing age are kept and treated as a separate age group
#' @param missing.contact.age if set to "remove" (default), participants that have contacts without age information are removed; if set to "sample", contacts without age information are sampled from all the contacts of participants of the same age group; if set to "keep", contacts with missing age are kept and treated as a separate age group; if set to "ignore", contact with missing age are ignored in the contact analysis
#' @param weights column names(s) of the participant data of the [survey()] object with user-specified weights (default = empty vector)
#' @param weigh.dayofweek whether to weigh social contacts data by the day of the week (weight (5/7 / N_week / N) for weekdays and (2/7 / N_weekend / N) for weekends)
#' @param weigh.age whether to weigh social contacts data by the age of the participants (vs. the populations' age distribution)
#' @param weight.threshold threshold value for the standardized weights before running an additional standardisation (default 'NA' = no cutoff)
#' @param symmetric.norm.threshold threshold value for the normalization weights when `symmetric = TRUE` before showing a warning that that large differences in the size of the sub-populations are likely to result in artefacts when making the matrix symmetric (default 2).
#' @param sample.all.age.groups what to do if sampling participants (with `sample.participants = TRUE`) fails to sample participants from one or more age groups; if FALSE (default), corresponding rows will be set to NA, if TRUE the sample will be discarded and a new one taken instead
#' @param return.part.weights boolean to return the participant weights
#' @param return.demography boolean to explicitly return demography data that corresponds to the survey data (default 'NA' = if demography data is requested by other function parameters)
#' @param per.capita whether to return a matrix with contact rates per capita (default is FALSE and not possible if 'counts=TRUE' or 'split=TRUE')
#' @param ... further arguments to pass to [get_survey()], [check()] and [pop_age()] (especially column names)
#' @return a contact matrix, and the underlying demography of the surveyed population
#' @importFrom stats xtabs runif median
#' @importFrom utils data globalVariables
#' @importFrom data.table copy
#' @importFrom countrycode countrycode
#' @import data.table
#' @export
#' @autoglobal
#' @examples
#' data(polymod)
#' contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 1, 5, 15))
#' @author Sebastian Funk
contact_matrix <- function(survey, countries = NULL, survey.pop, age.limits, filter, counts = FALSE, symmetric = FALSE, split = FALSE, sample.participants = FALSE, estimated.participant.age = c("mean", "sample", "missing"), estimated.contact.age = c("mean", "sample", "missing"), missing.participant.age = c("remove", "keep"), missing.contact.age = c("remove", "sample", "keep", "ignore"), weights = NULL, weigh.dayofweek = FALSE, weigh.age = FALSE, weight.threshold = NA, symmetric.norm.threshold = 2, sample.all.age.groups = FALSE, return.part.weights = FALSE, return.demography = NA, per.capita = FALSE, ...) {
  surveys <- c("participants", "contacts")

  dot.args <- list(...)
  unknown.args <- setdiff(names(dot.args), union(names(formals(check.contact_survey)), names(formals(pop_age))))
  if (length(unknown.args) > 0) {
    stop("Unknown argument(s): ", paste(unknown.args, sep = ", "), ".")
  }

  ## record if 'missing.participant.age' and 'missing.contact.age' are set, for later
  missing.participant.age.set <- !missing(missing.participant.age)
  missing.contact.age.set <- !missing(missing.contact.age)

  ## read arguments
  estimated.participant.age <- match.arg(estimated.participant.age)
  estimated.contact.age <- match.arg(estimated.contact.age)
  missing.participant.age <- match.arg(missing.participant.age)
  missing.contact.age <- match.arg(missing.contact.age)

  survey <- copy(survey)

  if (!inherits(survey, "contact_survey")) {
    stop(
      "`survey` must be a survey object (created using `survey()` ",
      "or `get_survey()`)"
    )
  }

  if (!missing(age.limits)) {
    age.limits <- as.integer(age.limits)
    if (anyNA(age.limits) || any(diff(age.limits) <= 0)) {
      stop("'age.limits' must be an increasing integer vector of lower age limits.")
    }
  }

  ## check if specific countries are requested (if a survey contains data from multiple countries)
  if (length(countries) > 0 && "country" %in% colnames(survey$participants)) {
    if (all(nchar(countries) == 2)) {
      corrected_countries <- suppressWarnings(
        countrycode(countries, "iso2c", "country.name")
      )
    } else {
      corrected_countries <- suppressWarnings(
        countrycode(countries, "country.name", "country.name")
      )
    }
    present_countries <- unique(as.character(survey$participants$country))
    missing_countries <- countries[which(is.na(corrected_countries))]
    if (length(missing_countries) > 0) {
      stop("Survey data not found for ", paste(missing_countries, sep = ", "), ".")
    }
    countries <- corrected_countries
    survey$participants <- survey$participants[country %in% countries]
    if (nrow(survey$participants) == 0) {
      stop("No participants left after selecting countries.")
    }
  }

  if ("part_age_exact" %in% colnames(survey$participants)) {
    survey$participants[
      ,
      part_age := as.integer(part_age_exact)
    ]
  } else if (!("part_age" %in% colnames(survey$participants))) {
    survey$participants[, part_age := NA_integer_]
  }

  ## sample estimated participant ages
  if ("part_age_est_min" %in% colnames(survey$participants) &&
    "part_age_est_max" %in% colnames(survey$participants)) {
    if (estimated.participant.age == "mean") {
      survey$participants[
        is.na(part_age_exact) &
          !is.na(part_age_est_min) & !is.na(part_age_est_max),
        part_age := as.integer(rowMeans(.SD)),
        .SDcols = c("part_age_est_min", "part_age_est_max")
      ]
    } else if (estimated.participant.age == "sample") {
      survey$participants[
        is.na(part_age) &
          !is.na(part_age_est_min) & !is.na(part_age_est_max) &
          part_age_est_min <= part_age_est_max,
        part_age := as.integer(runif(.N, part_age_est_min, part_age_est_max))
      ]
    }
    # note: do nothing when "missing" is specified
  }

  if ("part_age_est_max" %in% colnames(survey$participants)) {
    max.age <- max(
      c(
        survey$participants[, part_age_exact],
        survey$participants[, part_age_est_max]
      ),
      na.rm = TRUE
    ) + 1
  } else {
    max.age <- max(survey$participants[, part_age], na.rm = TRUE) + 1
  }

  if (missing(age.limits)) {
    all.ages <- unique(as.integer(survey$participants[, part_age]))
    all.ages <- all.ages[!is.na(all.ages)]
    all.ages <- sort(all.ages)
    age.limits <- union(0, all.ages)
  }

  if (missing.participant.age == "remove" &&
    nrow(survey$participants[
      is.na(part_age) | part_age < min(age.limits)
    ]) > 0) {
    if (!missing.participant.age.set) {
      message(
        "Removing participants without age information. ",
        "To change this behaviour, set the 'missing.participant.age' option"
      )
    }
    survey$participants <- survey$participants[
      !is.na(part_age) & part_age >= min(age.limits)
    ]
  }

  ## set contact age if it's not in the data
  if ("cnt_age_exact" %in% colnames(survey$contacts)) {
    survey$contacts[, cnt_age := as.integer(cnt_age_exact)]
  } else {
    survey$contacts[, cnt_age := NA_integer_]
  }

  ## convert factors to integers
  for (age_column in
    c("cnt_age", "cnt_age_est_min", "cnt_age_est_max", "cnt_age_exact")) {
    if (age_column %in% colnames(survey$contacts) &&
      is.factor(survey$contacts[[age_column]])) {
      survey$contacts[, paste(age_column) :=
        as.integer(levels(get(age_column)))[get(age_column)]]
    }
  }

  ## sample estimated contact ages
  if ("cnt_age_est_min" %in% colnames(survey$contacts) &&
    "cnt_age_est_max" %in% colnames(survey$contacts)) {
    if (estimated.contact.age == "mean") {
      survey$contacts[
        is.na(cnt_age) & !is.na(cnt_age_est_min) & !is.na(cnt_age_est_max),
        cnt_age := as.integer(rowMeans(.SD)),
        .SDcols = c("cnt_age_est_min", "cnt_age_est_max")
      ]
    } else if (estimated.contact.age == "sample") {
      survey$contacts[
        is.na(cnt_age) & !is.na(cnt_age_est_min) & !is.na(cnt_age_est_max) &
          cnt_age_est_min <= cnt_age_est_max,
        cnt_age := as.integer(runif(.N, cnt_age_est_min, cnt_age_est_max))
      ]
    }
    # note: do nothing when "missing" is specified
  }

  # remove contact ages below the age limit, before dealing with missing contact ages
  survey$contacts <- survey$contacts[is.na(cnt_age) |
    cnt_age >= min(age.limits), ]

  if (missing.contact.age == "remove" &&
    nrow(survey$contacts[is.na(cnt_age)]) > 0) {
    if (!missing.contact.age.set) {
      message(
        "Removing participants that have contacts without age information. ",
        "To change this behaviour, set the 'missing.contact.age' option"
      )
    }
    missing.age.id <- survey$contacts[
      is.na(cnt_age), part_id
    ]
    survey$participants <- survey$participants[!(part_id %in% missing.age.id)]
  }

  if (missing.contact.age == "ignore" &&
    nrow(survey$contacts[is.na(cnt_age)]) > 0) {
    if (!missing.contact.age.set) {
      message(
        "Ignore contacts without age information. ",
        "To change this behaviour, set the 'missing.contact.age' option"
      )
    }
    survey$contacts <- survey$contacts[!is.na(cnt_age), ]
  }

  ## check if any filters have been requested
  if (!missing(filter)) {
    missing_columns <- list()
    for (table in surveys) {
      if (nrow(survey[[table]]) > 0) {
        missing_columns <-
          c(missing_columns, list(setdiff(names(filter), colnames(survey[[table]]))))
        ## filter contact data
        for (column in names(filter)) {
          if (column %in% colnames(survey[[table]])) {
            survey[[table]] <- survey[[table]][get(column) == filter[[column]]]
          }
        }
      }
    }
    missing_all <- do.call(intersect, missing_columns)
    if (length(missing_all) > 0) {
      warning("filter column(s) ", toString(missing_all), " not found")
    }
  }

  # adjust age.group.brakes to the lower and upper ages in the survey
  survey$participants[, lower.age.limit := reduce_agegroups(
    part_age, age.limits[age.limits < max.age]
  )]
  part.age.group.breaks <- c(age.limits[age.limits < max.age], max.age)
  part.age.group.present <- age.limits[age.limits < max.age]
  survey$participants[, age.group :=
    cut(survey$participants[, part_age],
      breaks = part.age.group.breaks,
      right = FALSE
    )]
  age.groups <- survey$participants[, levels(age.group)]
  age.groups[length(age.groups)] <-
    sub("\\[([0-9]+),.*$", "\\1+", age.groups[length(age.groups)])
  survey$participants[, age.group :=
    factor(age.group, levels = levels(age.group), labels = age.groups)]

  ## add upper age limits
  lower.upper.age.limits <- data.table(
    lower.age.limit = part.age.group.present,
    upper.age.limit = part.age.group.breaks[-1]
  )
  survey$participants <-
    merge(survey$participants, lower.upper.age.limits, by = "lower.age.limit", all.x = TRUE)

  ## if split, symmetric or age weights are requested, get demographic data (survey population)
  need.survey.pop <- split || symmetric || weigh.age ||
    (!is.na(return.demography) && return.demography) || per.capita
  if (need.survey.pop) {
    ## check if survey population is either not given or given as a vector of countries
    if (missing(survey.pop) || is.character(survey.pop)) {
      survey.representative <- FALSE
      if (!missing(survey.pop)) {
        ## survey population is given as vector of countries
        survey.countries <- survey.pop
      } else if (!missing(countries)) {
        ## survey population not given but countries requested from
        ## survey - get population data from those countries
        survey.countries <- countries
      } else {
        ## neither survey population nor country names given - try to
        ## guess country or countries surveyed from participant data
        if ("country" %in% colnames(survey$participants)) {
          survey.countries <- unique(survey$participants[, country])
        } else {
          warning(
            "No 'survey.pop' or 'countries' given, and no '", "country",
            "' column found in the data. ",
            "I don't know which population this is from. ",
            "Assuming the survey is representative"
          )
          survey.representative <- TRUE
        }
      }

      if (!survey.representative) {
        ## get population data for countries from 'wpp' package
        country.pop <- data.table(wpp_age(survey.countries))

        # !! warning: spelling can differ between wpp_age and wpp_countries (e.g. Viet Nam vs Vietnam)
        # fix: rename countries using the same approach as in clean(survey,...)
        country.pop$country <- suppressWarnings(countrycode(country.pop$country, "country.name", "country.name"))

        ## check if survey data are from a specific year - in that case
        ## use demographic data from that year, otherwise latest
        if ("year" %in% colnames(survey$participants)) {
          survey.year <- survey$participants[, median(year, na.rm = TRUE)]
        } else {
          survey.year <- country.pop[, max(year, na.rm = TRUE)]
          warning(
            "No information on year found in the data. Will use ",
            survey.year, " population data."
          )
        }

        ## check if any survey countries are not in wpp
        missing.countries <- setdiff(survey.countries, unique(country.pop$country))
        if (length(missing.countries) > 0) {
          stop(
            "Could not find population data for ",
            toString(missing.countries), ". ",
            " Use wpp_countries() to get a list of country names."
          )
        }

        ## get demographic data closest to survey year
        country.pop.year <- unique(country.pop[, year])
        survey.year <-
          min(country.pop.year[which.min(abs(survey.year - country.pop.year))])
        survey.pop <-
          country.pop[year == survey.year][, list(population = sum(population)),
            by = "lower.age.limit"
          ]
      }
      if (survey.representative) {
        survey.pop <-
          survey$participants[
            ,
            lower.age.limit := reduce_agegroups(part_age, age.limits)
          ]
        survey.pop <- survey.pop[, list(population = .N), by = lower.age.limit]
        survey.pop <- survey.pop[!is.na(lower.age.limit)]
        if ("year" %in% colnames(survey$participants)) {
          survey.year <- survey$participants[, median(year, na.rm = TRUE)]
        }
      }
    } else {
      # if survey.pop is a data frame with columns 'lower.age.limit' and 'population'
      survey.pop <- data.table(survey.pop)
      # make sure the maximum survey.pop age exceeds the participant age group breaks
      if (max(survey.pop$lower.age.limit) < max(part.age.group.present)) {
        survey.pop <- rbind(
          survey.pop,
          list(max(part.age.group.present + 1), 0)
        )
      }

      # add dummy survey.year
      survey.year <- NA_integer_
    }

    # add upper.age.limit after sorting the survey.pop ages (and add maximum age > given ages)
    survey.pop <- survey.pop[order(lower.age.limit), ]
    # if any lower age limits are missing remove them
    survey.pop <- survey.pop[!is.na(population)]
    survey.pop$upper.age.limit <- unlist(c(
      survey.pop[-1, "lower.age.limit"],
      1 + max(survey.pop$lower.age.limit, part.age.group.present)
    ))

    if (weigh.age) {
      ## keep reference of survey.pop
      survey.pop.full <-
        data.table(pop_age(
          survey.pop,
          seq(
            min(survey.pop$lower.age.limit),
            max(survey.pop$upper.age.limit)
          ), ...
        ))
    }

    ## adjust age groups by interpolating, in case they don't match between
    ## demographic and survey data
    survey.pop.max <- max(survey.pop$upper.age.limit)
    survey.pop <- data.table(pop_age(survey.pop, part.age.group.present, ...))

    ## set upper age limits
    survey.pop[, upper.age.limit := c(part.age.group.present[-1], survey.pop.max)]
  }

  ## weights
  survey$participants[, weight := 1]

  ## assign weights to participants to account for weekend/weekday variation
  if (weigh.dayofweek) {
    found.dayofweek <- FALSE
    if ("dayofweek" %in% colnames(survey$participants)) {
      ## Add column sum_weight: Number of entries on weekdays / weekends
      survey$participants[, sum_weight := nrow(.SD),
        by = (dayofweek %in% 1:5),
      ]

      ## The sum of the weights on weekdays is 5
      survey$participants[dayofweek %in% 1:5, weight := 5 / sum_weight]
      ## The sum of the weights on weekend is 2
      survey$participants[!(dayofweek %in% 1:5), weight := 2 / sum_weight]

      survey$participants[, sum_weight := NULL]
      found.dayofweek <- TRUE

      # add boolean for "weekday"
      survey$participants[, is.weekday := dayofweek %in% 1:5]
    }
    if (!found.dayofweek) {
      warning(
        "'weigh.dayofweek' is TRUE, but no 'dayofweek' column in the data. ",
        "Will ignore."
      )
    }
  }

  ## assign weights to participants, to account for age variation
  if (weigh.age) {
    # get number and proportion of participants by age
    survey$participants[, age.count := .N, by = part_age]
    survey$participants[, age.proportion := age.count / .N]

    # get reference population by age (absolute and proportional)
    part.age.all <- range(unique(survey$participants[, part_age]))
    survey.pop.detail <- data.table(pop_age(survey.pop.full, seq(part.age.all[1], part.age.all[2] + 1)))
    names(survey.pop.detail) <- c("part_age", "population.count")
    survey.pop.detail[, population.proportion := population.count / sum(population.count)]

    # merge reference and survey population data
    survey$participants <- merge(survey$participants, survey.pop.detail, by = "part_age")

    # calculate age-specific weights
    survey$participants[, weight.age := population.proportion / age.proportion]

    # merge 'weight.age' into 'weight'
    survey$participants[, weight := weight * weight.age]

    ## Remove the additional columns
    survey$participants[, age.count := NULL]
    survey$participants[, age.proportion := NULL]
    survey$participants[, population.count := NULL]
    survey$participants[, population.proportion := NULL]
    survey$participants[, weight.age := NULL]
  }

  ## option to weigh the contact data with user-defined participant weights
  if (length(weights) > 0) {
    for (i in seq_along(weights)) {
      if (weights[i] %in% colnames(survey$participants)) {
        ## Compute the overall weight
        survey$participants[, weight := weight * get(weights[i])]
      }
    }
  }

  # post-stratification weight standardisation: by age.group
  survey$participants[, weight := weight / sum(weight) * .N,
    by = age.group
  ]

  # option to truncate overall participant weights (if not NULL or NA)
  if (!is.null(weight.threshold) && !is.na(weight.threshold)) {
    survey$participants[weight > weight.threshold, weight := weight.threshold]
    # re-normalise
    survey$participants[, weight := weight / sum(weight) * .N,
      by = age.group
    ]
  }

  ## merge participants and contacts into a single data table
  setkey(survey$participants, part_id)
  participant_ids <- unique(survey$participants$part_id)

  survey$contacts <-
    merge(survey$contacts, survey$participants,
      by = "part_id", all = FALSE,
      allow.cartesian = TRUE, suffixes = c(".cont", ".part")
    )

  setkey(survey$contacts, part_id)

  ## sample contacts
  if (missing.contact.age == "sample" &&
    nrow(survey$contacts[is.na(cnt_age)]) > 0) {
    for (this.age.group in
      unique(survey$contacts[is.na(cnt_age), age.group])) {
      ## first, deal with missing age
      if (nrow(survey$contacts[!is.na(cnt_age) &
        age.group == this.age.group]) > 0) {
        ## some contacts in the age group have an age, sample from these
        survey$contacts[
          is.na(cnt_age) &
            age.group == this.age.group,
          cnt_age :=
            sample(
              survey$contacts[
                !is.na(cnt_age) &
                  age.group == this.age.group,
                cnt_age
              ],
              size = .N,
              replace = TRUE
            )
        ]
      } else if (nrow(survey$contacts[!is.na(cnt_age), ]) > 0) {
        ## no contacts in the age group have an age, sample uniformly between limits
        min.contact.age <-
          survey$contacts[, min(cnt_age, na.rm = TRUE)]
        max.contact.age <-
          survey$contacts[, max(cnt_age, na.rm = TRUE)]
        survey$contacts[
          is.na(cnt_age) &
            age.group == this.age.group,
          cnt_age :=
            as.integer(floor(runif(.N,
              min = min.contact.age,
              max = max.contact.age + 1
            )))
        ]
      }
    }
    survey$contacts <- survey$contacts[!is.na(cnt_age), ] # make sure the final set does not contain NA's anymore
  }

  ## set contact age groups
  max.contact.age <- survey$contacts[, max(cnt_age, na.rm = TRUE) + 1]

  contact.age.group.breaks <- part.age.group.breaks
  if (max.contact.age > max(contact.age.group.breaks)) {
    contact.age.group.breaks[length(contact.age.group.breaks)] <- max.contact.age
  }
  survey$contacts[, contact.age.group :=
    cut(cnt_age,
      breaks = contact.age.group.breaks,
      labels = age.groups,
      right = FALSE
    )]

  ret <- list()
  if (sample.participants) {
    good.sample <- FALSE
    while (!good.sample) {
      ## take a sample from the participants
      part.sample <- sample(participant_ids, replace = TRUE)
      part.age.limits <-
        unique(survey$participants[
          part_id %in% part.sample,
          lower.age.limit
        ])
      good.sample <- !sample.all.age.groups ||
        (length(setdiff(age.limits, part.age.limits)) == 0)

      sample.table <-
        data.table(id = part.sample, weight = 1)
      sample.table <-
        sample.table[, list(bootstrap.weight = sum(weight)), by = id]
      setnames(sample.table, "id", "part_id")
      setkey(sample.table, part_id)

      sampled.contacts <- merge(survey$contacts, sample.table)
      sampled.contacts[, sampled.weight := weight * bootstrap.weight]

      sampled.participants <-
        merge(survey$participants, sample.table)
      sampled.participants[, sampled.weight := weight * bootstrap.weight]
    }
  } else {
    ## just use all participants
    sampled.contacts <- survey$contacts
    sampled.contacts[, sampled.weight := weight]
    sampled.participants <- survey$participants
    sampled.participants[, sampled.weight := weight]
  }

  ## calculate weighted contact matrix
  weighted.matrix <-
    xtabs(
      data = sampled.contacts,
      formula = sampled.weight ~ age.group + contact.age.group,
      addNA = TRUE
    )

  dims <- dim(weighted.matrix)
  dim.names <- dimnames(weighted.matrix)

  weighted.matrix <- array(
    weighted.matrix,
    dim = dims,
    dimnames = dim.names
  )

  if (!counts) { ## normalise to give mean number of contacts
    ## calculate normalisation vector
    norm.vector <- c(xtabs(
      data = sampled.participants,
      formula = sampled.weight ~ age.group, addNA = TRUE
    ))

    ## normalise contact matrix
    weighted.matrix <- weighted.matrix / norm.vector

    ## set non-existent data to NA
    weighted.matrix[is.nan(weighted.matrix)] <- NA_real_
  }


  ## construct a warning in case there are NAs
  na.headers <- anyNA(dimnames(weighted.matrix), recursive = TRUE)
  na.content <- anyNA(weighted.matrix)
  na.present <- na.headers || na.content

  if (na.present) {
    warning.suggestion <- "  Consider "
    if (na.headers) {
      warning.suggestion <- paste0(warning.suggestion, "setting ")
      suggested.options <- NULL
      if (anyNA(rownames(weighted.matrix))) {
        suggested.options <- c(suggested.options, "'missing.participant.age'")
      }
      if (anyNA(colnames(weighted.matrix))) {
        suggested.options <- c(suggested.options, "'missing.contact.age'")
      }

      warning.suggestion <-
        paste0(warning.suggestion, paste(suggested.options, collapse = " and "))
      if (na.content) {
        warning.suggestion <- paste0(warning.suggestion, ", and ")
      } else {
        warning.suggestion <- paste0(warning.suggestion, ".")
      }
    }
    if (na.content) {
      warning.suggestion <- paste0(warning.suggestion, "adjusting the age limits.")
    }
  }

  if (symmetric && prod(dim(as.matrix(weighted.matrix))) > 1) {
    if (counts) {
      warning(
        "'symmetric=TRUE' does not make sense with 'counts=TRUE'; ",
        "will not make matrix symmetric."
      )
    } else if (na.present) {
      warning(
        "'symmetric=TRUE' does not work with missing data; ",
        "will not make matrix symmetric\n",
        warning.suggestion
      )
    } else {
      ## set c_{ij} N_i and c_{ji} N_j (which should both be equal) to
      ## 0.5 * their sum; then c_{ij} is that sum / N_i
      normalised.weighted.matrix <- survey.pop$population * weighted.matrix
      normalised.weighted.matrix <- 0.5 / survey.pop$population *
        (normalised.weighted.matrix + t(normalised.weighted.matrix))
      # show warning if normalisation factors exceed the symmetric.norm.threshold
      normalisation_fctr <- c(normalised.weighted.matrix / weighted.matrix, weighted.matrix / normalised.weighted.matrix)
      normalisation_fctr <- normalisation_fctr[!is.infinite(normalisation_fctr) & !is.na(normalisation_fctr)]
      if (any(normalisation_fctr > symmetric.norm.threshold)) {
        warning("Large differences in the size of the sub-populations with the current age breaks are likely to result in artefacts after making the matrix symmetric. Please reconsider the age breaks to obtain more equally sized sub-populations. Normalization factors: [", paste(round(range(normalisation_fctr, na.rm = TRUE), digits = 1), collapse = ";"), "]")
      }
      # update weighted.matrix
      weighted.matrix <- normalised.weighted.matrix
    }
  }

  if (split) {
    if (counts) {
      warning(
        "'split=TRUE' does not make sense with 'counts=TRUE'; ",
        "will not split the contact matrix."
      )
    } else if (na.present) {
      warning(
        "'split=TRUE' does not work with missing data; ",
        "will not split contact.matrix.\n",
        warning.suggestion
      )
      ret[["mean.contacts"]] <- NA
      ret[["normalisation"]] <- NA
      ret[["contacts"]] <- rep(NA, nrow(weighted.matrix))
    } else {
      ## get rid of name but preserve row and column names
      weighted.matrix <- unname(weighted.matrix)

      nb.contacts <- rowSums(weighted.matrix)
      mean.contacts <- sum(survey.pop$population * nb.contacts) /
        sum(survey.pop$population)
      spectrum.matrix <- weighted.matrix
      spectrum.matrix[is.na(spectrum.matrix)] <- 0
      spectrum <- as.numeric(eigen(spectrum.matrix, only.values = TRUE)$values[1])
      ret[["mean.contacts"]] <- mean.contacts
      ret[["normalisation"]] <- spectrum / mean.contacts

      age.proportions <- survey.pop$population / sum(survey.pop$population)
      weighted.matrix <-
        diag(1 / nb.contacts) %*% weighted.matrix %*% diag(1 / age.proportions)
      nb.contacts <- nb.contacts / spectrum
      ret[["contacts"]] <- nb.contacts
    }
  }
  # make sure the dim.names are retained after symmetric or split procedure
  dimnames(weighted.matrix) <- dim.names

  ret[["matrix"]] <- weighted.matrix

  # option to add matrix per capita, i.e. the contact rate of age i with one individual of age j in the population.
  if (per.capita) {
    if (counts) {
      warning(
        "'per.capita=TRUE' does not make sense with 'counts=TRUE'; ",
        "will not return the contact matrix per capita."
      )
    } else if (split) {
      warning(
        "'per.capita=TRUE' does not make sense with 'split=TRUE'; ",
        "will not return the contact matrix per capita."
      )
    } else {
      survey.pop$population
      weighted.matrix.per.capita <- weighted.matrix / matrix(rep(survey.pop$population, nrow(survey.pop)), ncol = nrow(survey.pop), byrow = TRUE)
      weighted.matrix.per.capita
      ret[["matrix.per.capita"]] <- weighted.matrix.per.capita
    }
  }

  if (exists("survey.year")) {
    survey.pop[, year := survey.year]
    survey.pop <-
      merge(
        survey.pop,
        unique(survey$participants[, list(lower.age.limit, age.group)])
      )
    survey.pop <- survey.pop[, list(age.group, population,
      proportion = population / sum(population), year
    )]
  }

  ## get number of participants in each age group
  if (anyNA(survey$participants$age.group)) {
    useNA <- "always"
  } else {
    useNA <- "no"
  }

  part.pop <- data.table(table(survey$participants[, age.group], useNA = useNA))
  setnames(part.pop, c("age.group", "participants"))
  part.pop[, proportion := participants / sum(participants)]

  if (!is.null(ret)) {
    if (need.survey.pop && (is.na(return.demography) || return.demography)) {
      # change survey.pop$age.group factors into characters (cfr. part.pop)
      survey.pop[, age.group := as.character(age.group)]
      ret[["demography"]] <- survey.pop[]
    }
    ret[["participants"]] <- part.pop[]
  }

  # option to return participant weights
  if (return.part.weights) {
    # default
    part.weights <- survey$participants[, .N, by = list(age.group, weight)]
    part.weights <- part.weights[order(age.group, weight), ]

    # add age and/or dayofweek info
    if (weigh.age && weigh.dayofweek) {
      part.weights <- survey$participants[, .N, by = list(age.group, participant.age = part_age, is.weekday, weight)]
    } else if (weigh.age) {
      part.weights <- survey$participants[, .N, by = list(age.group, participant.age = part_age, weight)]
    } else if (weigh.dayofweek) {
      part.weights <- survey$participants[, .N, by = list(age.group, is.weekday, weight)]
    }

    # order (from left to right)
    part.weights <- part.weights[order(part.weights), ] # nolint

    # set name of last column
    names(part.weights)[ncol(part.weights)] <- "participants"

    # add proportion and add to ret
    part.weights[, proportion := participants / sum(participants)]
    ret[["participants.weights"]] <- part.weights[]
  }

  return(ret)
}

#' Generate a contact matrix using GAMs
#'
#' Fits a Generalized Additive Model (GAM) to survey data to model contact
#' patterns based on specified dimensions (e.g., age, gender) using tensor
#' product splines and interaction terms, then predicts contact rates over a
#' defined grid.
#'
#' @param survey A `contact_survey` object.
#' @param countries Optional vector of country names or codes to filter data.
#' @param dimensions A character vector naming the columns in the survey data
#'   to be used as dimensions in the GAM and the resulting matrix (e.g.,
#'   `c("part_age", "cnt_age", "part_gender", "cnt_gender")`). Assumes these columns exist
#'   and come in pairs following the "part_" and "cnt_" naming convention.
#' @param dim_breaks A named list where each name corresponds to a dimension in
#'   `dimensions`. For numeric dimensions (like age), the value should be a numeric
#'   vector defining the breaks for prediction grid bins (e.g., `seq(0, 80, 5)`).
#'   For factor/character dimensions (like gender), the value should be a vector
#'   of the levels (e.g., `c("F", "M")`).
#' @param gam_formula Optional. A `formula` object for the GAM model. If provided,
#'   it overrides the default automatic formula generation. Use this for custom model
#'   structures, like including interactions between numeric and categorical
#'   dimensions (e.g., age-gender interaction using `by=` in smooth terms:
#'   `s(part_age, by = part_gender)`).
#'   If `NULL` (default), the formula is built automatically including tensor
#'   products for numeric pairs (`te(...)`), interactions for categorical pairs
#'   (`interaction(...)`), and smooths for numeric dimensions varying by
#'   categorical dimensions (`s(..., by = ...)`).
#' @param family The error distribution and link function to be used in the
#'  `mgcv::gam` model (e.g., `nb()` for Negative Binomial, `poisson()`).
#' @param k_tensor Numeric vector `c(k1, k2)` specifying the basis dimensions `k` for
#'   the tensor product smooth `te()` between numeric pairs (e.g., age-age).
#'   Defaults to `c(8, 8)`.
#' @param k_by_dims Numeric value specifying the basis dimension `k` for smooth terms
#'   interacting with categorical variables (`s(..., by = ...)`). Defaults to 6.
#' @param bs_numeric Character string specifying the basis type (e.g., "ps", "cr")
#'   for all numeric smooth terms (`te` and `s`). Defaults to "ps".
#' @param filter Optional list to filter survey data before modeling.
#' @param age_limits Optional vector specifying the minimum and maximum age to consider.
#'   Participants or contacts outside these limits will be excluded before modeling.
#' @param use_bam Logical indicating whether to use `bam()` for fitting (default TRUE).
#' @param ... Additional arguments passed to `mgcv::gam`.
#'
#' @return A list containing:
#'   - `matrix`: The predicted contact **rate** matrix (as a multidimensional array).
#'   - `gam_fit`: The fitted `gam` object from `mgcv`.
#'   - `prediction_grid`: The data frame used for prediction (representing stratum midpoints/levels).
#'   - `dimensions`: The names of the dimensions.
#'   - `dim_breaks`: The breaks used for each dimension.
#'   - `gam_formula`: The `formula` object that was automatically generated (or NA if `gam_formula` was provided).
#'   - `fitting_data`: The aggregated data frame used to fit the GAM model (includes N contacts and N_participants).
#'
#' @importFrom mgcv gam predict.gam bam predict.bam s te
#' @importFrom stats formula reshape aggregate family poisson gaussian nb as.formula
#' @importFrom graphics image
#' @importFrom utils data globalVariables
#' @importFrom data.table copy merge.data.table `:=` setnames setkey data.table .N .SD is.data.table rbindlist as.data.table setkeyv CJ frank
#' @importFrom countrycode countrycode
#' @export
#' @examples
#' \dontrun{
#' library(socialmixr)
#' library(mgcv)
#' data(polymod)
#'
#' # Define dimensions and breaks for age (5-year bands) and gender
#' dimensions_demo <- c("part_age", "cnt_age", "part_gender", "cnt_gender")
#' dim_breaks_demo <- list(
#'   part_age = seq(0, 75, 5),
#'   cnt_age = seq(0, 75, 5),
#'   part_gender = c("F", "M"),
#'   cnt_gender = c("F", "M")
#' )
#'
#' # Run the GAM-based contact matrix estimation (default: includes age-gender interactions)
#' gam_matrix_result_auto <- gam_contact_matrix(
#'   survey = polymod,
#'   countries = "United Kingdom",
#'   # gam_formula = NULL, # Let formula be generated automatically
#'   dimensions = dimensions_demo,
#'   dim_breaks = dim_breaks_demo,
#'   family = nb(),
#'   age_limits = c(0, 75)
#'   # Optional controls for smooths:
#'   # k_tensor = c(8, 8), # Tensor smooth complexity
#'   # k_by = 6,           # Complexity for smooths varying by category
#'   # bs_numeric = "ps"   # Basis type
#' )
#'
#' # Example of providing a simpler custom formula (e.g., separate age/gender effects)
#' # gam_formula_simple <- N ~ te(part_age, cnt_age, k=c(10,10), bs="ps") +
#' #                         s(part_age, k=10, bs="ps") + s(cnt_age, k=10, bs="ps") +
#' #                         interaction(part_gender, cnt_gender)
#' # gam_matrix_result_simple <- gam_contact_matrix(
#' #   survey = polymod, countries = "United Kingdom",
#' #   gam_formula = gam_formula_simple,
#' #   dimensions = dimensions_demo, dim_breaks = dim_breaks_demo,
#' #   family = nb(), age_limits = c(0, 75)
#' # )
#'
#' # Check the automatically generated formula
#' print(gam_matrix_result_auto$gam_formula)
#'
#' # Explore the results (similar to previous example)
#' print(dim(gam_matrix_result_auto$matrix))
#' image(
#'    x = dim_breaks_demo$part_age, y = dim_breaks_demo$cnt_age,
#'    z = gam_matrix_result_auto$matrix[, , "F", "F"],
#'    xlab = "Participant Age", ylab = "Contact Age",
#'    main = "Predicted Contacts: F participants vs F contacts (UK, Polymod - Default Auto Formula)"
#' )
#' }
gam_contact_matrix <- function(survey,
                               countries = NULL,
                               dimensions,
                               dim_breaks,
                               gam_formula = NULL,
                               family = nb(),
                               k_tensor = c(16, 16),
                               k_by_dims = list(), # Named list: suffix=k_value (e.g., list(ethnicity=6))
                               k_by_default = 6, # Default k if suffix not in k_by_dims
                               bs_numeric = "ps",
                               filter = NULL,
                               age_limits = NULL,
                               use_bam = TRUE,
                               ...) {

  # Check if mgcv is installed
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Package 'mgcv' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # --- Argument checks ---
  if (!inherits(survey, "contact_survey")) {
    stop("`survey` must be a survey object.")
  }
   if (!is.character(dimensions) || length(dimensions) < 2 || length(dimensions) %% 2 != 0) {
     stop("`dimensions` must be an even-length character vector with at least two dimension names (expected in part_/cnt_ pairs).")
  }
  if (!is.list(dim_breaks) || !all(names(dim_breaks) %in% dimensions)) {
    stop("`dim_breaks` must be a named list with names corresponding to `dimensions`.")
  }
   if (!all(dimensions %in% names(dim_breaks))) {
    stop("Each dimension in `dimensions` must have corresponding breaks in `dim_breaks`.")
  }
  # Check pairing
  part_dims <- dimensions[startsWith(dimensions, "part_")]
  cnt_dims <- dimensions[startsWith(dimensions, "cnt_")]
   if (length(part_dims) != length(cnt_dims) || length(part_dims) != length(dimensions) / 2) {
       stop("`dimensions` must contain an equal number of 'part_' and 'cnt_' prefixed names.")
   }
  dim_suffixes <- sub("^part_", "", part_dims)
  if (!all(sub("^cnt_", "", cnt_dims) == dim_suffixes)) {
     stop("`dimensions` must form pairs based on suffixes (e.g., 'part_age'/'cnt_age', 'part_gender'/'cnt_gender').")
  }


  # --- 1. Data Preparation ---
  survey_data <- copy(survey) # Avoid modifying the original object

  # Identify numeric and categorical dimensions based on breaks
  dim_types <- sapply(dimensions, function(d) {
      if (is.numeric(dim_breaks[[d]])) "numeric" else "factor"
  })

  # Helper function to create binned factor columns
  create_binned_column <- function(data, dim_name, breaks, prefix) {
      col_name <- paste0(prefix, dim_name)
      if (!col_name %in% names(data)) {
           # Try to find source columns (exact, mean of range)
            exact_col <- paste0(col_name, "_exact")
            est_min_col <- paste0(col_name, "_est_min")
            est_max_col <- paste0(col_name, "_est_max")

            if (exact_col %in% names(data)) {
                data[, (col_name) := data[[exact_col]]]
            } else if (est_min_col %in% names(data) && est_max_col %in% names(data)) {
                 data[, (col_name) := as.integer(rowMeans(.SD, na.rm = TRUE)), .SDcols = c(est_min_col, est_max_col)]
                 message(paste("Note: Created temporary raw", col_name, "using mean of", est_min_col, "and", est_max_col))
            } else {
                stop(paste("Cannot find or create required raw dimension column:", col_name))
            }
             # Ensure correct type if age-like
             if (grepl("age", dim_name, ignore.case = TRUE)) {
                 data[, (col_name) := as.integer(get(col_name))]
             }
      }

      binned_col_name <- paste0(col_name, "_binned")
      raw_values <- data[[col_name]]

      if (is.numeric(breaks)) {
         if(length(breaks) < 2) stop(paste("Numeric dimension", col_name, "needs at least 2 breaks."))
         # Format intervals like contact_matrix
         lower <- breaks[-length(breaks)]
         upper <- breaks[-1]
         labels <- paste0("[", format(lower, trim=TRUE), ",", format(upper, trim=TRUE), ")")
         data[, (binned_col_name) := cut(raw_values, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)]
         return(labels) # Return levels
      } else {
         # Use levels directly for factors/characters
         levels_provided <- as.character(breaks)
         data[, (binned_col_name) := factor(raw_values, levels = levels_provided)]
          return(levels_provided) # Return levels
      }
  }

  # Apply binning/factoring and store levels
  all_levels <- list()
  for (dim_name in dimensions) {
      breaks <- dim_breaks[[dim_name]]
      prefix <- if (startsWith(dim_name, "part_")) "part_" else "cnt_"
      table_name <- if (startsWith(dim_name, "part_")) "participants" else "contacts"
      # Ensure the base column exists before binning
      raw_col_name <- dim_name # The original name like 'part_age'

       # Bin the data
      created_levels <- create_binned_column(survey_data[[table_name]], sub(prefix, "", dim_name), breaks, prefix)
      all_levels[[paste0(dim_name, "_binned")]] <- created_levels
  }
  binned_dimensions <- paste0(dimensions, "_binned")
  binned_part_dims <- binned_dimensions[startsWith(binned_dimensions, "part_")]
  binned_cnt_dims <- binned_dimensions[startsWith(binned_dimensions, "cnt_")]


  # Filter by country if specified (on original columns)
  if (!is.null(countries) && "country" %in% names(survey_data$participants)) {
      # Use countrycode logic similar to contact_matrix if needed
      if (all(nchar(countries) == 2)) {
       corrected_countries <- suppressWarnings(
         countrycode::countrycode(countries, "iso2c", "country.name")
       )
     } else {
       corrected_countries <- suppressWarnings(
         countrycode::countrycode(countries, "country.name", "country.name")
       )
     }
      missing_countries <- countries[is.na(corrected_countries)]
      if (length(missing_countries) > 0) {
        stop("Survey data not found for ", paste(missing_countries, collapse = ", "), ".")
      }
      countries <- corrected_countries
      survey_data$participants <- survey_data$participants[country %in% countries]
      if (nrow(survey_data$participants) == 0) {
        stop("No participants left after selecting countries.")
      }
      # Filter contacts based on remaining participants
      survey_data$contacts <- survey_data$contacts[part_id %in% survey_data$participants$part_id]
  }

  # Apply filters if provided (on original or binned columns if filter refers to bins)
  if (!is.null(filter)) {
      for (col in names(filter)) {
        target_col <- col # Assume original column name
        if (paste0(col, "_binned") %in% binned_dimensions) {
            target_col <- paste0(col, "_binned") # Use binned if filter refers to it
             message(paste("Applying filter on binned column:", target_col))
        }
        if (target_col %in% names(survey_data$participants)) {
          survey_data$participants <- survey_data$participants[get(target_col) == filter[[col]]]
        }
         if (target_col %in% names(survey_data$contacts)) {
           survey_data$contacts <- survey_data$contacts[get(target_col) == filter[[col]]]
         }
      }
      # Ensure consistency after filtering
       part_ids_remain_filter <- unique(survey_data$participants$part_id)
       survey_data$contacts <- survey_data$contacts[part_id %in% part_ids_remain_filter]
       contact_ids_remain_filter <- unique(survey_data$contacts$part_id)
       survey_data$participants <- survey_data$participants[part_id %in% contact_ids_remain_filter]
       if (nrow(survey_data$participants) == 0 || nrow(survey_data$contacts) == 0) {
        stop("No data left after applying filters.")
      }
  }


   # Handle age limits (apply to *binned* age columns now)
   if (!is.null(age_limits)) {
       if (!is.numeric(age_limits) || length(age_limits) != 2 || age_limits[1] >= age_limits[2]) {
           stop("`age_limits` must be a numeric vector of length 2 specifying min and max age.")
       }
       min_age <- age_limits[1]
       max_age <- age_limits[2]

        # Find corresponding binned levels to keep (more robust than filtering raw values now)
        get_levels_within_range <- function(binned_levels, min_val, max_val) {
            pattern <- "\\[([0-9\\.]+),([0-9\\.]+)\\)"
            lower_bounds <- as.numeric(sub(pattern, "\\1", binned_levels))
            upper_bounds <- as.numeric(sub(pattern, "\\2", binned_levels))
             # Keep levels that *overlap* with the desired range
             # Level starts before max AND level ends after min
             keep <- lower_bounds < max_val & upper_bounds > min_val
            return(binned_levels[keep])
        }

       # Filter participants based on binned age
       part_age_binned_col <- grep("^part_.*age.*_binned$", binned_dimensions, value = TRUE)
       if (length(part_age_binned_col) == 1) {
           levels_to_keep <- get_levels_within_range(all_levels[[part_age_binned_col]], min_age, max_age)
           survey_data$participants <- survey_data$participants[get(part_age_binned_col) %in% levels_to_keep]
           message(paste("Filtering participants on age levels:", paste(levels_to_keep, collapse=", ")))
       } else if ("part_age" %in% dimensions) { # Check original dim name
           warning("age_limits provided but 'part_age_binned' column not found or ambiguous.")
       }

       # Filter contacts based on binned age
       cnt_age_binned_col <- grep("^cnt_.*age.*_binned$", binned_dimensions, value = TRUE)
       if (length(cnt_age_binned_col) == 1) {
            levels_to_keep <- get_levels_within_range(all_levels[[cnt_age_binned_col]], min_age, max_age)
            survey_data$contacts <- survey_data$contacts[get(cnt_age_binned_col) %in% levels_to_keep]
            message(paste("Filtering contacts on age levels:", paste(levels_to_keep, collapse=", ")))
       } else if ("cnt_age" %in% dimensions) { # Check original dim name
            warning("age_limits provided but 'cnt_age_binned' column not found or ambiguous.")
       }

       # Ensure consistency after age filtering
       part_ids_remain_age <- unique(survey_data$participants$part_id)
       survey_data$contacts <- survey_data$contacts[part_id %in% part_ids_remain_age]
       contact_ids_remain_age <- unique(survey_data$contacts$part_id)
       survey_data$participants <- survey_data$participants[part_id %in% contact_ids_remain_age]

        if (nrow(survey_data$participants) == 0 || nrow(survey_data$contacts) == 0) {
           stop("No data left after applying age limits.")
       }
   }


  # Merge participant *binned* dimensions onto contacts data
  part_dims_to_merge_binned <- intersect(binned_part_dims, names(survey_data$participants))
  if (length(part_dims_to_merge_binned) > 0) {
       if (!"part_id" %in% names(survey_data$participants) || !"part_id" %in% names(survey_data$contacts)) {
           stop("Missing 'part_id' column, required for merging participant data.")
       }
      merge_cols <- unique(c("part_id", part_dims_to_merge_binned))
      # Select only needed columns from contacts too
      contact_cols_needed <- unique(c("part_id", binned_cnt_dims))
      gam_data_full <- merge(
          survey_data$contacts[, ..contact_cols_needed],
          survey_data$participants[, ..merge_cols],
          by = "part_id",
          all.x = TRUE # Keep all contacts, even if participant was filtered somehow (shouldn't happen now)
      )
  } else {
      # Should not happen if there are participant dimensions
      gam_data_full <- survey_data$contacts[, ..binned_cnt_dims] # Select only binned contact dims
      warning("No participant dimensions found to merge.")
  }

  # --- Remove rows with NA in any *binned* dimension ---
  rows_before_na <- nrow(gam_data_full)
  gam_data_full <- na.omit(gam_data_full, cols = binned_dimensions)
  rows_after_na <- nrow(gam_data_full)
  if (rows_after_na < rows_before_na) {
      message(paste("Removed", rows_before_na - rows_after_na, "contact rows with NA in binned dimension columns."))
  }


  # --- 2. Aggregate Data & Include Zeros ---

  # Create the complete grid of all possible strata based on levels
  # Use CJ for data.table cross join
  all_strata <- do.call(CJ, all_levels)
  setnames(all_strata, names(all_levels)) # Ensure names match binned_dimensions

  # Aggregate OBSERVED contacts by binned dimensions
  if (nrow(gam_data_full) > 0) {
      observed_counts <- gam_data_full[, .N, by = c(binned_dimensions)]
  } else {
      # Handle case with zero contacts after filtering
      observed_counts <- data.table()
      # Add columns specified in binned_dimensions if empty
      for (col in binned_dimensions) set(observed_counts, j = col, value = factor(levels = all_levels[[col]] ))
      observed_counts[, N := integer(0)] # Add N column
  }


  # Aggregate PARTICIPANTS by binned *participant* dimensions
  # Need to ensure participants table has the binned columns correctly
  if (nrow(survey_data$participants) > 0) {
      # Check if all required binned participant columns exist
      missing_binned_part <- setdiff(binned_part_dims, names(survey_data$participants))
      if(length(missing_binned_part) > 0) {
          stop("Internal Error: Binned participant columns missing before aggregation: ", paste(missing_binned_part, collapse=", "))
      }
      participant_counts <- survey_data$participants[, .(N_participants = .N), by = c(binned_part_dims)]
  } else {
       participant_counts <- data.table()
       for (col in binned_part_dims) set(participant_counts, j = col, value = factor(levels = all_levels[[col]] ))
       participant_counts[, N_participants := integer(0)] # Add N_participants column
  }


  # Combine: Start with all strata, add observed counts (N), add participant counts (N_participants)
  gam_data_for_fitting <- merge(all_strata, observed_counts, by = binned_dimensions, all.x = TRUE)
  gam_data_for_fitting[is.na(N), N := 0L] # Replace NA counts with 0

  if (length(binned_part_dims) > 0) {
      gam_data_for_fitting <- merge(gam_data_for_fitting, participant_counts, by = binned_part_dims, all.x = TRUE)
      gam_data_for_fitting[is.na(N_participants), N_participants := 0L] # Strata with 0 participants
  } else {
      # If no participant dimensions, offset is based on total participants? Or should it error?
      # Let's assume N_participants is the total number of participants if no part dims specified
       total_participants <- nrow(survey_data$participants)
       if (total_participants == 0) stop("No participants found, cannot calculate offset.")
       gam_data_for_fitting[, N_participants := total_participants]
       warning("No participant dimensions specified; using total number of participants for offset.")
  }

  # Filter out strata with zero participants (cannot contribute contacts)
  n_rows_before_filter <- nrow(gam_data_for_fitting)
  gam_data_for_fitting <- gam_data_for_fitting[N_participants > 0]
  n_rows_after_filter <- nrow(gam_data_for_fitting)
  message(paste("Removed", n_rows_before_filter - n_rows_after_filter, "strata with zero participants before fitting."))

  if (nrow(gam_data_for_fitting) == 0) {
      stop("No data remaining for GAM fitting after removing strata with zero participants.")
  }


  # --- Create Explicit Interaction Factors using *Binned* Categorical Dimensions ---
  # Find categorical suffixes based on the original dimensions but use binned columns
  categorical_suffixes <- c()
  for (sfx in dim_suffixes) {
      # Check if *both* part_ and cnt_ dimensions for this suffix are factors
      # (This assumes pairs must be of the same type for interaction, which is reasonable)
      part_dim_name <- paste0("part_", sfx)
      cnt_dim_name <- paste0("cnt_", sfx)
      if (part_dim_name %in% names(dim_types) && cnt_dim_name %in% names(dim_types) &&
          dim_types[[part_dim_name]] == "factor" && dim_types[[cnt_dim_name]] == "factor") {
          categorical_suffixes <- c(categorical_suffixes, sfx)
      }
  }

  interaction_col_names <- c()
  if (length(categorical_suffixes) > 0) {
      message(paste("Identified categorical suffixes for interaction:", paste(categorical_suffixes, collapse=", ")))
      for(sfx in categorical_suffixes) {
          part_col_binned <- paste0("part_", sfx, "_binned")
          cnt_col_binned <- paste0("cnt_", sfx, "_binned")
          interaction_col <- paste0(sfx, "_interaction") # Keep original interaction name style
          interaction_col_names <- c(interaction_col_names, interaction_col)

          # Check if binned columns exist before attempting interaction
          if (!(part_col_binned %in% names(gam_data_for_fitting)) || !(cnt_col_binned %in% names(gam_data_for_fitting))) {
              stop(paste("Internal Error: Cannot create interaction factor '", interaction_col,
                         "'. Missing binned columns:", part_col_binned, "or", cnt_col_binned))
          }

          message(paste("Creating explicit interaction factor:", interaction_col, "from binned columns"))
          gam_data_for_fitting[, (interaction_col) := interaction(get(part_col_binned), get(cnt_col_binned), drop = FALSE, sep = ":")]
      }
  } else {
      message("No categorical dimension pairs found for interaction.")
  }


  # --- Build or Use GAM Formula (using binned dimensions and offset) ---
  if (is.null(gam_formula)) {
    message("Building GAM formula automatically (using binned dimensions and offset)...")
    formula_terms <- c()

    # Check argument validity
    if(!is.numeric(k_tensor) || length(k_tensor) != 2) stop("`k_tensor` must be a numeric vector of length 2.")
    # Corrected check for k_by_dims: Ensure it's a list
    if(!is.list(k_by_dims)) stop("`k_by_dims` must be a named list.")
    # Further checks on names vs suffixes can happen later if needed.

    # Use original dimension names and types for logic, but binned names in formula terms
    numeric_suffixes <- c()
    for (sfx in dim_suffixes) {
        part_dim_name <- paste0("part_", sfx)
        cnt_dim_name <- paste0("cnt_", sfx)
        if (part_dim_name %in% names(dim_types) && cnt_dim_name %in% names(dim_types) &&
            dim_types[[part_dim_name]] == "numeric" && dim_types[[cnt_dim_name]] == "numeric") {
            numeric_suffixes <- c(numeric_suffixes, sfx)
        }
    }
    if (length(numeric_suffixes) > 0) {
        message(paste("Identified numeric suffixes for smooths/tensor:", paste(numeric_suffixes, collapse=", ")))
    } else {
         message("No numeric dimension pairs found for smooths/tensor.")
    }

    # Convert numeric binned columns back to numeric midpoints for GAM smooths
    # Create midpoint columns in the fitting data
    midpoint_cols <- c()
    for (num_suffix in numeric_suffixes) {
        for (prefix in c("part_", "cnt_")) {
            dim_name <- paste0(prefix, num_suffix)
            binned_col <- paste0(dim_name, "_binned")
            midpoint_col <- paste0(dim_name, "_midpoint")
            midpoint_cols <- c(midpoint_cols, midpoint_col)

            breaks <- dim_breaks[[dim_name]]
            lower <- breaks[-length(breaks)]
            upper <- breaks[-1]
            midpoints <- lower + diff(breaks) / 2
            levels_original <- all_levels[[binned_col]] # Levels generated earlier

             # Create mapping from factor level to midpoint
             if (length(levels_original) != length(midpoints)) {
                 stop(paste("Internal Error: Mismatch between levels and midpoints for", binned_col))
             }
             level_to_midpoint_map <- setNames(midpoints, levels_original)

            # Apply mapping
             gam_data_for_fitting[, (midpoint_col) := level_to_midpoint_map[as.character(get(binned_col))]]

             # Check for NAs introduced by mapping (shouldn't happen if levels match)
              if (anyNA(gam_data_for_fitting[[midpoint_col]])) {
                  warning(paste("NAs introduced when creating midpoints for", midpoint_col, "- check factor levels."))
              }
        }
    }


    # Handle numeric pairs (Tensor product using midpoint columns)
    for (num_suffix in numeric_suffixes) {
        part_var_mid <- paste0("part_", num_suffix, "_midpoint")
        cnt_var_mid <- paste0("cnt_", num_suffix, "_midpoint")
        formula_terms <- c(formula_terms,
                           paste0("te(", part_var_mid, ", ", cnt_var_mid,
                                  ", k = c(", k_tensor[1], ", ", k_tensor[2], "), bs = \"", bs_numeric, "\")"))
    }

    # Handle categorical pairs (using explicit interaction columns)
    if (length(interaction_col_names) > 0) {
         formula_terms <- c(formula_terms, paste(interaction_col_names, collapse = " + "))
    }

    # --- Add smooths for numeric midpoint variables --- 
    # If categorical interactions exist, allow age smooths to vary by category
    # Otherwise, just add simple age smooths.
    if (length(numeric_suffixes) > 0) {
        for (num_suffix in numeric_suffixes) { # Typically just 'age'
            part_num_var_mid <- paste0("part_", num_suffix, "_midpoint")
            cnt_num_var_mid <- paste0("cnt_", num_suffix, "_midpoint")
            
            if (length(categorical_suffixes) > 0) {
                # Add smooths varying by the *individual* part/contact category
                # Assumes a categorical variable exists for each suffix in categorical_suffixes
                for (cat_suffix in categorical_suffixes) {
                   # Determine k for this category-specific smooth
                   k_val_by <- k_by_default # Start with default
                   if (cat_suffix %in% names(k_by_dims)) {
                       k_val_by <- k_by_dims[[cat_suffix]]
                       if (!is.numeric(k_val_by) || length(k_val_by) != 1 || k_val_by < 3) {
                           warning(paste("Invalid k value specified in k_by_dims for suffix:", cat_suffix, "- using default k=", k_by_default))
                           k_val_by <- k_by_default
                       }
                   } else {
                        message(paste("Suffix:", cat_suffix, "not found in k_by_dims. Using default k=", k_by_default))
                   }

                   part_cat_var_binned <- paste0("part_", cat_suffix, "_binned")
                   cnt_cat_var_binned <- paste0("cnt_", cat_suffix, "_binned")

                   # Check if these factor columns exist before adding terms
                   if (part_cat_var_binned %in% names(gam_data_for_fitting)) {
                       formula_terms <- c(formula_terms,
                                         paste0("s(", part_num_var_mid, ", by = ", part_cat_var_binned, ", k = ", k_val_by, ", bs = \"", bs_numeric, "\")"))
                   } else {
                        warning(paste("Categorical column", part_cat_var_binned, "not found for 'by' term in participant smooth."))
                   }
                   if (cnt_cat_var_binned %in% names(gam_data_for_fitting)) {
                       formula_terms <- c(formula_terms,
                                         paste0("s(", cnt_num_var_mid, ", by = ", cnt_cat_var_binned, ", k = ", k_val_by, ", bs = \"", bs_numeric, "\")"))
                   } else {
                        warning(paste("Categorical column", cnt_cat_var_binned, "not found for 'by' term in contact smooth."))
                   }
                } # End for cat_suffix loop
            } else {
                # No categorical variables, just add simple smooths
                formula_terms <- c(formula_terms,
                                  paste0("s(", part_num_var_mid, ", k = ", k_tensor[1], ", bs = \"", bs_numeric, "\")"))
                formula_terms <- c(formula_terms,
                                  paste0("s(", cnt_num_var_mid, ", k = ", k_tensor[2], ", bs = \"", bs_numeric, "\")"))
            }
        }
    }

    # Assemble final formula string including the offset
    if (length(formula_terms) == 0) {
        # If only offset, GAM might complain. Need at least one predictor or intercept.
        # Add intercept if no terms? Or rely on mgcv default? Let's add explicitly.
        # formula_str <- "N ~ 1 + offset(log(N_participants))"
        stop("Could not generate any smoother/interaction terms for the GAM formula based on dimensions.")
    } else {
        formula_str <- paste("N ~", paste(formula_terms, collapse = " + "), "+ offset(log(N_participants))")
    }

    gam_formula_generated <- as.formula(formula_str)
    message("Generated GAM formula: ", formula_str)
    formula_to_use <- gam_formula_generated
  } else {
    # --- Use Provided GAM Formula ---
     if (!inherits(gam_formula, "formula")) {
        stop("`gam_formula` must be a formula object or NULL.")
     }
     message("Using provided GAM formula. Ensure it includes '+ offset(log(N_participants))' for correct rate modeling.")
     # We need to add the midpoint columns to the data even if formula is provided
     # (assuming the provided formula might use them)
      midpoint_cols <- c()
      numeric_suffixes_g <- dim_suffixes[sapply(dimensions, function(d) dim_types[[d]] == "numeric")]
      for (num_suffix in numeric_suffixes_g) {
            for (prefix in c("part_", "cnt_")) {
                dim_name <- paste0(prefix, num_suffix)
                binned_col <- paste0(dim_name, "_binned")
                midpoint_col <- paste0(dim_name, "_midpoint")
                midpoint_cols <- c(midpoint_cols, midpoint_col)
                if (!midpoint_col %in% names(gam_data_for_fitting)) { # Check if already created
                    breaks <- dim_breaks[[dim_name]]
                    lower <- breaks[-length(breaks)]
                    upper <- breaks[-1]
                    midpoints <- lower + diff(breaks) / 2
                    levels_original <- all_levels[[binned_col]]
                    if (length(levels_original) != length(midpoints)) stop(paste("Mismatch levels/midpoints for", binned_col))
                    level_to_midpoint_map <- setNames(midpoints, levels_original)
                    gam_data_for_fitting[, (midpoint_col) := level_to_midpoint_map[as.character(get(binned_col))]]
                }
            }
      }
     formula_to_use <- gam_formula
     gam_formula_generated <- NA # Mark as user-provided
  }


  # --- 3. Fit GAM/BAM ---
  # Ensure data is data.frame for mgcv (though it often handles data.table)
  gam_data_for_fitting_df <- as.data.frame(gam_data_for_fitting)

  # --- Define potential 'by' variables --- 
  formula_vars <- all.vars(formula_to_use) # Get all variables used in the formula
  potential_by_vars <- formula_vars[grepl("_binned$", formula_vars)] # Find _binned vars in formula

  # --- Ensure 'by' variables are factors --- 
  for(by_var in potential_by_vars) {
      if (by_var %in% names(gam_data_for_fitting_df)) {
          if (!is.factor(gam_data_for_fitting_df[[by_var]])) {
              message(paste("Converting potential 'by' variable", by_var, "to factor."))
              gam_data_for_fitting_df[[by_var]] <- as.factor(gam_data_for_fitting_df[[by_var]])
          }
      } # No need to warn if not found, formula creation should handle that
  }
  # --- End Factor Conversion --- 

  if (use_bam) {
      message("Fitting model using bam()...")
      gam_fit <- mgcv::bam(
          formula = formula_to_use,
          data = gam_data_for_fitting_df,
          family = family,
          method = "fREML",
          discrete = TRUE,
          trace = TRUE,
          ...
      )
      message("BAM fitting complete.")
  } else {
      message("Fitting model using gam()...")
      gam_fit <- mgcv::gam(
          formula = formula_to_use,
          data = gam_data_for_fitting_df,
          family = family,
          method = "fREML", # Use fREML for consistency
          ...
      )
      message("GAM fitting complete.")
  }

  # --- 4. Create Prediction Grid ---
  # Needs to represent the *centers* of the bins for numeric dimensions
  # Needs factor levels for categorical dimensions
  prediction_grid_list <- lapply(dimensions, function(dim_name) {
      breaks <- dim_breaks[[dim_name]]
      if (is.numeric(breaks)) {
          # Use midpoints
           if(length(breaks) < 2) stop(paste("Numeric dimension", dim_name, "needs at least 2 breaks."))
           return(breaks[-length(breaks)] + diff(breaks) / 2)
      } else {
          # Use levels directly
          levels_provided <- as.character(breaks)
          # Ensure factor levels match those potentially used in the model (via interactions)
          # Find corresponding binned column name
           binned_col_name_pred <- names(all_levels)[endsWith(names(all_levels), paste0(dim_name, "_binned"))]
           if (length(binned_col_name_pred) == 1) {
               model_levels <- all_levels[[binned_col_name_pred]] # Use levels derived earlier
               # Check consistency
                if (!all(levels_provided %in% model_levels)) {
                    warning(paste("Some prediction levels for", dim_name, "not in model factor levels:",
                                  paste(setdiff(levels_provided, model_levels), collapse=", ")))
                }
                return(factor(levels_provided, levels=model_levels))
           } else {
                warning(paste("Could not find unique binned column for factor", dim_name, "using provided levels directly."))
                return(factor(levels_provided))
           }
       }
  })
  # Name the list elements with the *midpoint* or *original factor* names expected by the formula
  names(prediction_grid_list) <- sapply(dimensions, function(dim_name) {
      if (dim_types[[dim_name]] == "numeric") paste0(dim_name, "_midpoint") else dim_name
  })


  # Expand grid
  prediction_grid <- expand.grid(prediction_grid_list, KEEP.OUT.ATTRS = FALSE)
  prediction_grid$N_participants <- 1 # Set N_participants=1 for rate prediction
  setDT(prediction_grid)

  # --- Add required 'by' factor columns to prediction grid --- 
  # The fitted model formula now uses *_binned columns for 'by=' smooths.
  # We need to add these to the prediction grid, ensuring levels match fitting data.
  if (length(categorical_suffixes) > 0) {
      formula_vars <- all.vars(formula_to_use) 
      potential_by_vars <- formula_vars[grepl("_binned$", formula_vars)]
      
      for (cat_suffix in categorical_suffixes) {
          part_cat_col_binned <- paste0("part_", cat_suffix, "_binned")
          cnt_cat_col_binned <- paste0("cnt_", cat_suffix, "_binned")
          part_cat_col_orig <- paste0("part_", cat_suffix)
          cnt_cat_col_orig <- paste0("cnt_", cat_suffix)
          
          # Add participant factor if used in formula
          if (part_cat_col_binned %in% potential_by_vars) {
              if (part_cat_col_orig %in% names(prediction_grid)) {
                  if (part_cat_col_binned %in% names(gam_data_for_fitting_df)) {
                     fit_levels <- levels(gam_data_for_fitting_df[[part_cat_col_binned]])
                     # Use data.table syntax to add column
                     prediction_grid[, (part_cat_col_binned) := factor(.SD[[part_cat_col_orig]], levels = fit_levels)]
                     message(paste("Added factor column:", part_cat_col_binned, "to prediction grid."))
                  } else {
                     warning(paste("Column", part_cat_col_binned, "used in formula but not found in fitting data frame! Cannot set levels for prediction grid."))
                  }
              } else {
                  warning(paste("Original column", part_cat_col_orig, "not found in prediction grid to create", part_cat_col_binned))
              }
          }
          # Add contact factor if used in formula
          if (cnt_cat_col_binned %in% potential_by_vars) {
              if (cnt_cat_col_orig %in% names(prediction_grid)) {
                 if (cnt_cat_col_binned %in% names(gam_data_for_fitting_df)) {
                     fit_levels <- levels(gam_data_for_fitting_df[[cnt_cat_col_binned]])
                     # Use data.table syntax to add column
                     prediction_grid[, (cnt_cat_col_binned) := factor(.SD[[cnt_cat_col_orig]], levels = fit_levels)]
                     message(paste("Added factor column:", cnt_cat_col_binned, "to prediction grid."))
                 } else {
                      warning(paste("Column", cnt_cat_col_binned, "used in formula but not found in fitting data frame! Cannot set levels for prediction grid."))
                 }
              } else {
                  warning(paste("Original column", cnt_cat_col_orig, "not found in prediction grid to create", cnt_cat_col_binned))
              }
          }
      }
  }
  # --- End adding 'by' columns ---

  # --- Create Explicit Interaction Factors in Prediction Grid --- 
  # This needs to be done AFTER adding the 'by' columns if they share base names
  # Ensure levels match those potentially created during fitting
  if (length(categorical_suffixes) > 0) {
      for(sfx in categorical_suffixes) {
          part_col_orig <- paste0("part_", sfx)
          cnt_col_orig <- paste0("cnt_", sfx)
          interaction_col <- paste0(sfx, "_interaction") 
          
          # Ensure base columns exist and are factors (may already be factors)
          if (part_col_orig %in% names(prediction_grid) && !is.factor(prediction_grid[[part_col_orig]])) {
               prediction_grid[, (part_col_orig) := as.factor(get(part_col_orig))]
          }
          if (cnt_col_orig %in% names(prediction_grid) && !is.factor(prediction_grid[[cnt_col_orig]])) {
              prediction_grid[, (cnt_col_orig) := as.factor(get(cnt_col_orig))]
          }
          
          # Create the interaction factor, ensuring all levels from the fitting data interaction are present
          if (part_col_orig %in% names(prediction_grid) && cnt_col_orig %in% names(prediction_grid)) {
              if (interaction_col %in% names(gam_data_for_fitting_df)) { # Check if it exists in fitting data
                  fit_interaction_levels <- levels(gam_data_for_fitting_df[[interaction_col]])
                  
                  # Create interaction in prediction grid
                  prediction_grid[, temp_interaction := interaction(get(part_col_orig), get(cnt_col_orig), drop = FALSE, sep = ":")]
                  
                  # Ensure factor levels match the fitting interaction levels
                  current_levels <- levels(prediction_grid$temp_interaction)
                  all_expected_levels <- union(current_levels, fit_interaction_levels)
                  prediction_grid[, (interaction_col) := factor(temp_interaction, levels = all_expected_levels)]
                  prediction_grid[, temp_interaction := NULL] # Remove temporary column
                  
                  message(paste("Added interaction factor column:", interaction_col, "to prediction grid."))
              } else {
                  warning(paste("Interaction column", interaction_col, "not found in fitting data. Cannot guarantee matching levels for prediction."))
                  # Create interaction anyway, but levels might mismatch
                  prediction_grid[, (interaction_col) := interaction(get(part_col_orig), get(cnt_col_orig), drop = FALSE, sep = ":")]
              }
           } else {
                warning("Could not create interaction factor '", interaction_col, "' in prediction grid: base columns missing.")
           }
       }
   }
  # --- End Creating Interaction Factors ---

  # Convert final prediction grid to data frame for return value consistency
  prediction_grid_df <- as.data.frame(prediction_grid)

  # --- 5. Predict on Grid --- 
  # Initialize predicted_values to NULL
  predicted_values <- NULL
  predict_error <- NULL

  message("Predicting contact rates on grid...")
  tryCatch({
      predict_func <- if (inherits(gam_fit, "bam")) mgcv::predict.bam else mgcv::predict.gam
      # Debugging: Print newdata just before predict
      message("First few rows of newdata for prediction:")
      print(head(prediction_grid))
      message("Names of newdata for prediction:")
      print(names(prediction_grid))

      predicted_values <- predict_func(
          gam_fit,
          newdata = prediction_grid,
          type = "response",
          se.fit = FALSE
      )
      message("Prediction function call completed.")
  }, error = function(e) {
     # Error handling
     message("!!! Error occurred during prediction function call !!!")
     predict_error <<- e # Store error message globally within the function scope
     message(e$message)
  })

  # --- Check if prediction failed ---
  if (is.null(predicted_values)) {
      error_msg <- "Prediction step failed."
      if (!is.null(predict_error)) {
          error_msg <- paste(error_msg, "Reason:", predict_error$message)
      }
      stop(error_msg, call. = FALSE)
  } else {
      message(paste("Prediction successful. Length of predicted_values:", length(predicted_values)))
  }

  # Check for negative predictions
  if (any(predicted_values < 0)) {
      warning("Negative predicted values detected. This is not expected for Poisson or Negative Binomial models. Please check your model and data.")
  }

  # --- 6. Reshape into Matrix/Array ---
   # Dimensions based on the number of levels/bins defined in *dim_breaks*
   array_dims <- sapply(dimensions, function(dim_name) {
       breaks <- dim_breaks[[dim_name]]
       if (is.numeric(breaks)) {
           return(length(breaks) - 1) # Number of intervals
       } else {
           return(length(breaks)) # Number of levels
       }
   })
  names(array_dims) <- dimensions # Keep original dimension names

  # Create dimnames for the array using the *binned factor levels* from all_levels
  array_dimnames <- lapply(dimensions, function(dim_name) {
       binned_col_name <- paste0(dim_name, "_binned")
       if (binned_col_name %in% names(all_levels)) {
           return(all_levels[[binned_col_name]])
       } else {
           # Fallback if something went wrong (shouldn't happen)
           warning(paste("Could not find binned levels for dimension", dim_name, "using raw breaks for dimnames."))
           breaks <- dim_breaks[[dim_name]]
           if (is.numeric(breaks)) {
                lower <- breaks[-length(breaks)]
                upper <- breaks[-1]
                return(paste0("[", format(lower, trim=TRUE), ",", format(upper, trim=TRUE), ")"))
           } else {
               return(as.character(breaks))
           }
       }
  })
  names(array_dimnames) <- dimensions


  # Reshape the predicted values into the array
  if(length(predicted_values) != prod(array_dims)) {
      stop(paste("Internal Error: Number of predicted values (", length(predicted_values),
                 ") does not match the expected array size (", prod(array_dims), "). Check prediction grid generation."))
  }
  contact_array <- array(
      data = predicted_values,
      dim = array_dims,
      dimnames = array_dimnames
  )

  # --- 7. Return Results ---
  return(
    list(
      matrix = contact_array, # This is now contact *rate*
      prediction_grid = prediction_grid_df, # Grid used for prediction
      dimensions = dimensions,
      dim_breaks = dim_breaks,
      gam_formula = gam_formula_generated, # The one automatically generated (or NA)
      gam_fit = gam_fit,
      fitting_data = gam_data_for_fitting # Data used for fitting (aggregated)
    )
  )
}

# Helper function (if not already present) - reduce_agegroups
# Ensure this exists or is defined if needed by other parts of contact_matrix.R
# reduce_agegroups <- function(ages, limits) { ... }

