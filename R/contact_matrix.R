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

  # --- 7. Return Results ---
  # Assign gam_fit to a temp variable to avoid potential naming conflicts
  final_gam_fit <- gam_fit

  return(
    list(
      matrix = weighted.matrix,
      gam_fit = final_gam_fit, # Use the temp variable
      prediction_grid = prediction_grid,
      dimensions = dimensions,
      dim_breaks = dim_breaks
    )
  )
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
#' @param k_by Numeric value specifying the basis dimension `k` for smooth terms
#'   interacting with categorical variables (`s(..., by = ...)`). Defaults to 6.
#' @param bs_numeric Character string specifying the basis type (e.g., "ps", "cr")
#'   for all numeric smooth terms (`te` and `s`). Defaults to "ps".
#' @param filter Optional list to filter survey data before modeling.
#' @param age_limits Optional vector specifying the minimum and maximum age to consider.
#'   Participants or contacts outside these limits will be excluded before modeling.
#' @param weigh.dayofweek Logical; if TRUE, weigh contacts by day of the week.
#' @param weigh.age Logical; if TRUE, weigh contacts by participant age distribution
#'   compared to the population distribution provided in `survey.pop`. Requires
#'   `part_age` in `dimensions`.
#' @param survey.pop Population data, required if `weigh.age = TRUE`. Can be a
#'   data frame with `lower.age.limit` and `population` columns, or country name(s).
#' @param user_weights Character vector of column names in `survey$participants`
#'   containing custom weights to apply.
#' @param weight.threshold Numeric threshold to truncate calculated weights before
#'   standardization. Defaults to `NA` (no truncation).
#' @param ... Additional arguments passed to `mgcv::gam`.
#'
#' @return A list containing:
#'   - `matrix`: The predicted contact matrix (as a multidimensional array, representing predicted sum of weights).
#'   - `gam_fit`: The fitted `gam` object from `mgcv`.
#'   - `prediction_grid`: The data frame used for prediction.
#'   - `dimensions`: The names of the dimensions.
#'   - `dim_breaks`: The breaks used for each dimension.
#'   - `gam_formula_str`: The formula string used for the GAM fit.
#'
#' @importFrom mgcv gam predict.gam
#' @importFrom stats formula reshape aggregate family poisson gaussian nb as.formula
#' @importFrom graphics image
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
#' print(gam_matrix_result_auto$gam_formula_str)
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
                               k_tensor = c(8, 8),
                               k_by = 6,
                               bs_numeric = "ps",
                               filter = NULL,
                               age_limits = NULL,
                               weigh.dayofweek = FALSE,
                               weigh.age = FALSE,
                               survey.pop = NULL,
                               user_weights = NULL,
                               weight.threshold = NA,
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

  # --- Additional Argument Checks for Weighting ---
  if (weigh.age) {
      if (is.null(survey.pop)) {
          stop("`survey.pop` must be provided when `weigh.age = TRUE`.")
      }
      if (!"part_age" %in% dimensions) {
          stop("`part_age` must be included in `dimensions` when `weigh.age = TRUE`.")
      }
      # Check if pop_age function is available if needed
      if (is.character(survey.pop) && !exists("wpp_age")) {
            warning("`survey.pop` provided as country names, but `wpp_age` function (e.g., from wppExplorer) not found. Age weighting by country name might fail.")
      }
       if (is.data.frame(survey.pop) && !exists("pop_age")) {
            warning("`pop_age` function (from socialmixr) not found. Age weighting interpolation might fail.")
       }
  }
  if (!is.null(user_weights) && !is.character(user_weights)) {
      stop("`user_weights` must be a character vector of column names or NULL.")
  }


  # --- 1. Data Preparation ---
  survey_data <- copy(survey) # Avoid modifying the original object

  # Ensure essential columns exist (part_id, etc.)
  if (!"part_id" %in% names(survey_data$participants) || !"part_id" %in% names(survey_data$contacts)) {
      stop("Both participants and contacts tables must contain 'part_id'.")
  }

  # Ensure required dimension columns exist and handle estimation (e.g., age from exact/range)
  for (suffix in dim_suffixes) {
      part_dim_name <- paste0("part_", suffix)
      cnt_dim_name <- paste0("cnt_", suffix)

      # Participant dimension handling (prioritise exact, then mean of range)
      if (!(part_dim_name %in% names(survey_data$participants))) {
          exact_col <- paste0(part_dim_name, "_exact")
          est_min_col <- paste0(part_dim_name, "_est_min")
          est_max_col <- paste0(part_dim_name, "_est_max")

          if (exact_col %in% names(survey_data$participants)) {
              survey_data$participants[, (part_dim_name) := get(exact_col)]
              message(paste("Using", exact_col, "for", part_dim_name))
          } else if (est_min_col %in% names(survey_data$participants) && est_max_col %in% names(survey_data$participants)) {
               survey_data$participants[, (part_dim_name) := rowMeans(.SD, na.rm = TRUE), .SDcols = c(est_min_col, est_max_col)]
               message(paste("Note: Created", part_dim_name, "using mean of", est_min_col, "and", est_max_col))
          } else {
              stop(paste("Cannot find or create required participant dimension:", part_dim_name))
          }
           # Ensure correct type
           if (part_dim_name == "part_age") { survey_data$participants[, (part_dim_name) := as.integer(get(part_dim_name))] }
      }

      # Contact dimension handling (similar logic)
      if (!(cnt_dim_name %in% names(survey_data$contacts))) {
          exact_col <- paste0(cnt_dim_name, "_exact")
          est_min_col <- paste0(cnt_dim_name, "_est_min")
          est_max_col <- paste0(cnt_dim_name, "_est_max")

          if (exact_col %in% names(survey_data$contacts)) {
              survey_data$contacts[, (cnt_dim_name) := get(exact_col)]
               message(paste("Using", exact_col, "for", cnt_dim_name))
          } else if (est_min_col %in% names(survey_data$contacts) && est_max_col %in% names(survey_data$contacts)) {
               survey_data$contacts[, (cnt_dim_name) := rowMeans(.SD, na.rm = TRUE), .SDcols = c(est_min_col, est_max_col)]
               message(paste("Note: Created", cnt_dim_name, "using mean of", est_min_col, "and", est_max_col))
          } else {
               stop(paste("Cannot find or create required contact dimension:", cnt_dim_name))
          }
          # Ensure correct type
          if (cnt_dim_name == "cnt_age") { survey_data$contacts[, (cnt_dim_name) := as.integer(get(cnt_dim_name))] }
      }
  }


  # Filter by country if specified
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
      initial_participants <- nrow(survey_data$participants)
      survey_data$participants <- survey_data$participants[country %in% countries]
      if (nrow(survey_data$participants) == 0) {
        stop("No participants left after selecting countries.")
      }
      message(paste("Filtered participants by country, keeping", nrow(survey_data$participants), "of", initial_participants))
      # Filter contacts based on remaining participants
      initial_contacts <- nrow(survey_data$contacts)
      survey_data$contacts <- survey_data$contacts[part_id %in% survey_data$participants$part_id]
       message(paste("Filtered contacts by remaining participants, keeping", nrow(survey_data$contacts), "of", initial_contacts))
  }

  # Apply filters if provided (simplified version)
  if (!is.null(filter)) {
      original_rows_p <- nrow(survey_data$participants)
      original_rows_c <- nrow(survey_data$contacts)
      for (col in names(filter)) {
         filter_val <- filter[[col]]
        if (col %in% names(survey_data$participants)) {
          survey_data$participants <- survey_data$participants[get(col) == filter_val]
        }
         if (col %in% names(survey_data$contacts)) {
           survey_data$contacts <- survey_data$contacts[get(col) == filter_val]
         }
      }
      # Ensure consistency after filtering
      valid_part_ids <- unique(survey_data$participants$part_id)
      survey_data$contacts <- survey_data$contacts[part_id %in% valid_part_ids]
      valid_part_ids_after_contact_filter <- unique(survey_data$contacts$part_id)
      survey_data$participants <- survey_data$participants[part_id %in% valid_part_ids_after_contact_filter]

       if (nrow(survey_data$participants) == 0 || nrow(survey_data$contacts) == 0) {
        stop("No data left after applying filters.")
      }
       message(paste("Applied filters. Participants:", nrow(survey_data$participants), "of", original_rows_p,
                    " Contacts:", nrow(survey_data$contacts), "of", original_rows_c))
  }

   # Ensure required dimension columns exist (already checked implicitly above)
   required_participant_cols <- dimensions[grepl("^part_", dimensions)]
   required_contact_cols <- dimensions[grepl("^cnt_", dimensions)]

   # Handle age limits - must happen *after* age columns are created/verified
   if (!is.null(age_limits)) {
       if (!is.numeric(age_limits) || length(age_limits) != 2 || age_limits[1] >= age_limits[2]) {
           stop("`age_limits` must be a numeric vector of length 2 specifying min and max age.")
       }
       min_age <- age_limits[1]
       max_age <- age_limits[2]
       message(paste("Applying age limits:", min_age, "-", max_age))

       original_rows_p <- nrow(survey_data$participants)
       original_rows_c <- nrow(survey_data$contacts)

       # Filter participants if part_age exists
       if ("part_age" %in% names(survey_data$participants)) {
           survey_data$participants <- survey_data$participants[part_age >= min_age & part_age <= max_age]
       } else if ("part_age" %in% dimensions) {
           warning("age_limits provided but 'part_age' column not found after data preparation.")
       }
       # Filter contacts if cnt_age exists
        if ("cnt_age" %in% names(survey_data$contacts)) {
           survey_data$contacts <- survey_data$contacts[cnt_age >= min_age & cnt_age <= max_age]
       } else if ("cnt_age" %in% dimensions) {
            warning("age_limits provided but 'cnt_age' column not found after data preparation.")
       }
        # Ensure consistency after filtering
       part_ids_remain <- unique(survey_data$participants$part_id)
       survey_data$contacts <- survey_data$contacts[part_id %in% part_ids_remain]
       contact_ids_remain <- unique(survey_data$contacts$part_id)
       survey_data$participants <- survey_data$participants[part_id %in% contact_ids_remain]

        if (nrow(survey_data$participants) == 0 || nrow(survey_data$contacts) == 0) {
           stop("No data left after applying age limits.")
       }
   }


  # --- 1b. Weight Calculation Setup (if needed) ---
  need.survey.pop.for.weighting <- weigh.age
  survey.pop.processed <- NULL # Initialize: Pop data aggregated to participant age groups
  survey.pop.full <- NULL # Initialize: Pop data in detail (e.g., single year)
  part.age.group.breaks <- NULL # Initialize: Breaks used for participant age grouping
  part.age.group.present <- NULL # Initialize: Lower limits of participant age groups
  age.groups <- NULL # Initialize: Factor labels for participant age groups
  survey.year <- NA_integer_ # Initialize

  # Process population data ONLY if weigh.age is TRUE
  if (need.survey.pop.for.weighting) {
     message("Processing survey population data for age weighting...")
      # Determine age breaks for population processing based on dim_breaks[['part_age']]
      if (!"part_age" %in% names(dim_breaks) || !is.numeric(dim_breaks[['part_age']])) {
          stop("`dim_breaks` must contain numeric breaks for 'part_age' when `weigh.age = TRUE`.")
      }
      age.limits.for.pop <- dim_breaks[['part_age']]
      if (anyNA(age.limits.for.pop) || any(diff(age.limits.for.pop) <= 0)) {
         stop("'dim_breaks' for 'part_age' must be an increasing numeric vector.")
      }

      # Determine max age needed based on participant data AND breaks
      max_part_age_data <- -Inf
      if ("part_age" %in% names(survey_data$participants)) {
         max_part_age_data <- max(survey_data$participants$part_age, na.rm=TRUE)
      }
      max_age_pop <- max(max_part_age_data, max(age.limits.for.pop), na.rm = TRUE) + 1

      # Define age group breaks and levels based on dim_breaks[['part_age']]
      # These breaks are used *both* for grouping participants and processing pop data
      part.age.group.present <- age.limits.for.pop[age.limits.for.pop < max_age_pop]
      part.age.group.breaks <- c(part.age.group.present, max_age_pop)
      # Generate age group labels using interval notation
      lower <- part.age.group.present
      upper <- part.age.group.breaks[-1]
      age.groups <- paste0("[", format(lower, trim=TRUE), ",", format(upper, trim=TRUE), ")")

      # Process survey.pop (logic adapted from contact_matrix)
      if (is.character(survey.pop)) {
           if (!exists("wpp_age")) stop("Function 'wpp_age' (e.g. from wppExplorer) needed for population lookup by country name.")
           if (!requireNamespace("countrycode", quietly = TRUE)) stop("Package 'countrycode' needed for country name matching.")

           survey.countries <- survey.pop
           # Use countrycode for robust name matching
           corrected_countries <- suppressWarnings(countrycode::countrycode(survey.countries, "country.name", "country.name"))
           if(any(is.na(corrected_countries))) stop("Could not resolve country names: ", paste(survey.countries[is.na(corrected_countries)], collapse=", "))

           country.pop <- data.table(wpp_age(corrected_countries)) # Assumes wpp_age returns required columns
           country.pop$country <- suppressWarnings(countrycode::countrycode(country.pop$country, "country.name", "country.name")) # Standardize returned names

           # Determine survey year
           if ("year" %in% colnames(survey_data$participants)) {
             survey.year <- floor(median(survey_data$participants$year, na.rm = TRUE))
           } else if ("year" %in% colnames(country.pop)) {
             survey.year <- max(country.pop$year, na.rm = TRUE)
             warning("No 'year' column in participant data. Using latest year from population data: ", survey.year)
           } else {
             stop("Cannot determine year for population data.")
           }

           # Get demographic data closest to survey year
            if ("year" %in% colnames(country.pop)) {
                country.pop.year <- unique(country.pop$year)
                survey.year <- min(country.pop.year[which.min(abs(survey.year - country.pop.year))])
                message("Using population data for year: ", survey.year)
                survey.pop.base <- country.pop[year == survey.year & country %in% corrected_countries][, list(population = sum(population)), by = "lower.age.limit"] # Make sure lower.age.limit exists
            } else {
                 survey.pop.base <- country.pop[country %in% corrected_countries][, list(population = sum(population)), by = "lower.age.limit"]
            }


      } else if (is.data.frame(survey.pop) && all(c("lower.age.limit", "population") %in% names(survey.pop))) {
          survey.pop.base <- data.table(survey.pop)
      } else {
          stop("`survey.pop` must be country names or a data.frame with 'lower.age.limit' and 'population'.")
      }

       # Prepare survey.pop.base for pop_age
       survey.pop.base <- survey.pop.base[order(lower.age.limit)]
       survey.pop.base <- survey.pop.base[!is.na(population)]
       survey.pop.base[, upper.age.limit := c(survey.pop.base[-.N, lower.age.limit], Inf)] # Use Inf for last group temporarily

       # Check if pop_age function is available
       if (!exists("pop_age")) stop("Function `pop_age` (from socialmixr) needed for age weighting interpolation.")

       # Use pop_age for interpolation/aggregation
       # Get detailed (single-year) population for weighting calculation
       target_ages_detail <- seq(min(0, survey.pop.base$lower.age.limit), max(max_age_pop, survey.pop.base$lower.age.limit)-1)
       survey.pop.full <- data.table(pop_age(survey.pop.base, target_ages_detail))
       setnames(survey.pop.full, "lower.age.limit", "part_age", skip_absent=TRUE) # Rename for merging later
        if (!"population" %in% names(survey.pop.full)) setnames(survey.pop.full, "pop", "population") # Check for alternative name from pop_age

       # Aggregate to the participant age groups for standardization checks later
       survey.pop.processed <- data.table(pop_age(survey.pop.base, part.age.group.present))
       survey.pop.processed[, upper.age.limit := c(part.age.group.present[-1], max_age_pop)] # Match breaks used for participants
       message("Survey population processing complete.")
  }

  # Assign age group to participants *using the breaks derived for weighting/output*
  if ("part_age" %in% names(survey_data$participants)) {
     if (is.null(part.age.group.breaks)) { # Define breaks if not done for weighting
         if (!"part_age" %in% names(dim_breaks) || !is.numeric(dim_breaks[['part_age']])) {
            stop("Numeric breaks for 'part_age' must be provided in `dim_breaks`.")
         }
         age.limits.for.groups <- dim_breaks[['part_age']]
         max_age_groups <- max(survey_data$participants$part_age, na.rm = TRUE) + 1
         part.age.group.present_local <- age.limits.for.groups[age.limits.for.groups < max_age_groups]
         part.age.group.breaks <- c(part.age.group.present_local, max_age_groups)
          # Generate labels
         lower <- part.age.group.present_local
         upper <- part.age.group.breaks[-1]
         age.groups <- paste0("[", format(lower, trim=TRUE), ",", format(upper, trim=TRUE), ")")
     }

     survey_data$participants[, age.group := cut(part_age, breaks = part.age.group.breaks, labels = age.groups, right = FALSE, include.lowest = TRUE)]
  } else if (weigh.age || (!is.null(weight.threshold) && !is.na(weight.threshold))) {
       # If weighting by age group or thresholding, we need age groups based on part_age
       stop("'part_age' must be in dimensions and participant data for age weighting or thresholding.")
  }


 # --- 1c. Calculate Participant Weights ---
 message("Calculating participant weights...")
 survey_data$participants[, calculated_weight := 1.0] # Use float

 # Day of week weighting
 if (weigh.dayofweek) {
     if ("dayofweek" %in% names(survey_data$participants)) {
         # Ensure dayofweek is numeric/integer
         if (!is.numeric(survey_data$participants$dayofweek)) {
             warning("'dayofweek' column is not numeric, cannot perform day-of-week weighting.")
         } else {
             survey_data$participants[, is_weekday := dayofweek %in% 1:5]
             # Calculate weights within groups (weekday/weekend)
             # Ensure calculation is robust to groups with zero participants
             survey_data$participants[, temp_N_weekday := sum(is_weekday), by=.(is_weekday)]
             survey_data$participants[is_weekday == TRUE & temp_N_weekday > 0, weight_factor_dow := 5 / temp_N_weekday]
             survey_data$participants[is_weekday == FALSE & temp_N_weekday > 0, weight_factor_dow := 2 / temp_N_weekday]
             # Handle cases where a group might be empty (avoid NaN/Inf)
              survey_data$participants[is.na(weight_factor_dow), weight_factor_dow := 0]
             # Apply weight
             survey_data$participants[, calculated_weight := calculated_weight * weight_factor_dow]
             # Clean up temporary columns
             survey_data$participants[, c("is_weekday", "temp_N_weekday", "weight_factor_dow") := NULL]
             message("Applied day-of-week weighting.")
         }
     } else {
         warning("'weigh.dayofweek' is TRUE, but 'dayofweek' column not found in participant data. Ignoring.")
     }
 }

 # Age weighting
 if (weigh.age) {
     if (!is.null(survey.pop.full) && "part_age" %in% names(survey_data$participants)) {
         # Participant distribution by single year of age
         part_dist <- survey_data$participants[, .N, by = part_age][!is.na(part_age)]
         if(nrow(part_dist) == 0) {
              warning("No valid participant ages found for age weighting.")
         } else {
             part_dist[, prop_survey := N / sum(N)]

             # Population distribution by single year of age
             pop_dist <- copy(survey.pop.full) # Use the detailed pop data
              # Ensure column names match for merge ('part_age', 'population')
             if (!"part_age" %in% names(pop_dist)) setnames(pop_dist, "lower.age.limit", "part_age", skip_absent = TRUE)
             if (!"population" %in% names(pop_dist)) setnames(pop_dist, "pop", "population", skip_absent = TRUE)
             if (!all(c("part_age", "population") %in% names(pop_dist))) {
                 stop("Processed population data (`survey.pop.full`) does not have expected columns 'part_age' and 'population'.")
             }

             pop_dist <- pop_dist[!is.na(part_age) & !is.na(population)] # Clean NAs
             total_pop <- sum(pop_dist$population, na.rm=TRUE)
             if (total_pop > 0) {
                 pop_dist[, prop_pop := population / total_pop]

                 # Merge and calculate weight factor
                 weight_data <- merge(part_dist[, .(part_age, prop_survey)],
                                      pop_dist[, .(part_age, prop_pop)],
                                      by = "part_age", all.x = TRUE) # Keep all participant ages
                 # If pop data is missing for an age group present in survey, assign zero weight factor? Or 1? contact_matrix implies 0.
                 weight_data[is.na(prop_pop), prop_pop := 0]
                 weight_data[prop_survey == 0, weight_factor := 0] # Avoid division by zero if survey N=0 for an age
                 weight_data[prop_survey > 0, weight_factor := prop_pop / prop_survey]

                 # Apply weight factor to participants
                 survey_data$participants <- merge(survey_data$participants,
                                                   weight_data[, .(part_age, weight_factor)],
                                                   by = "part_age", all.x = TRUE)
                 # If weight_factor is NA (e.g., part_age was NA), keep original weight
                  survey_data$participants[!is.na(weight_factor), calculated_weight := calculated_weight * weight_factor]
                 survey_data$participants[, weight_factor := NULL] # Clean up
                 message("Applied age weighting.")
             } else {
                  warning("Total population in `survey.pop.full` is zero. Cannot perform age weighting.")
             }
         }
     } else {
          warning("Could not perform age weighting. Check `survey.pop` processing and `part_age` presence.")
     }
 }

 # User-defined weights
 if (!is.null(user_weights)) {
    missing_user_weights <- setdiff(user_weights, names(survey_data$participants))
    if (length(missing_user_weights) > 0) {
        warning("User weight columns not found in participant data: ", paste(missing_user_weights, collapse=", "), ". Ignoring these.")
    }
    valid_user_weights <- intersect(user_weights, names(survey_data$participants))
     if (length(valid_user_weights) > 0) {
         applied_weights_log <- c()
         for (w_col in valid_user_weights) {
            if(!is.numeric(survey_data$participants[[w_col]])) {
                warning("User weight column '", w_col, "' is not numeric. Skipping.")
                next
            }
            # Handle potential NAs in user weight column - treat as weight=1? Or 0? Assume 1.
             survey_data$participants[is.na(get(w_col)), (w_col) := 1]
             # Apply weight
             survey_data$participants[, calculated_weight := calculated_weight * get(w_col)]
             applied_weights_log <- c(applied_weights_log, w_col)
         }
         if (length(applied_weights_log) > 0) {
            message("Applied user-defined weights: ", paste(applied_weights_log, collapse=", "))
         }
     }
 }

 # Post-stratification standardization by age group (if age.group exists)
 if ("age.group" %in% names(survey_data$participants)) {
    # Calculate sum of weights per group, handle cases where sum is 0
    survey_data$participants[, sum_weight_group := sum(calculated_weight, na.rm=TRUE), by = age.group]
    survey_data$participants[sum_weight_group > 0, calculated_weight := calculated_weight / sum_weight_group * .N]
    # Set weight to 0 if the group sum was 0 or if individual weight was NA initially
     survey_data$participants[sum_weight_group <= 0 | is.na(calculated_weight), calculated_weight := 0]
    survey_data$participants[, sum_weight_group := NULL] # Clean up
    message("Standardized weights by participant age group.")
 } else {
    message("Participant age group column ('age.group') not found/created, skipping weight standardization by age group.")
 }

 # Weight truncation
 if (!is.null(weight.threshold) && !is.na(weight.threshold)) {
     truncated_count <- sum(survey_data$participants$calculated_weight > weight.threshold, na.rm=TRUE)
     if (truncated_count > 0) {
         survey_data$participants[calculated_weight > weight.threshold, calculated_weight := weight.threshold]
         message(paste("Truncated", truncated_count, "weights at threshold:", weight.threshold))

         # Re-normalize after truncation if age groups exist
         if ("age.group" %in% names(survey_data$participants)) {
             survey_data$participants[, sum_weight_group := sum(calculated_weight, na.rm=TRUE), by = age.group]
             survey_data$participants[sum_weight_group > 0, calculated_weight := calculated_weight / sum_weight_group * .N]
             survey_data$participants[sum_weight_group <= 0 | is.na(calculated_weight), calculated_weight := 0]
             survey_data$participants[, sum_weight_group := NULL] # Clean up
             message("Re-standardized weights by age group after truncation.")
         } else {
             # Global re-normalization if no age groups
             total_weight_after_trunc <- sum(survey_data$participants$calculated_weight, na.rm=TRUE)
             total_participants <- nrow(survey_data$participants)
             if (total_weight_after_trunc > 0 && total_participants > 0) {
                 survey_data$participants[, calculated_weight := calculated_weight / total_weight_after_trunc * total_participants]
                 message("Re-standardized weights globally after truncation (no age groups).")
             } else {
                 message("Total weight or participant count is zero after truncation, cannot re-standardize globally.")
                 survey_data$participants[, calculated_weight := 0]
             }
         }
     } else {
         message("No weights exceeded the truncation threshold.")
     }
 }


 # --- 1d. Merge Weights and Prepare for Aggregation ---
 # Select only necessary columns from participants for merge: part_id and calculated_weight
 participant_weights_to_merge <- survey_data$participants[, .(part_id, calculated_weight)]

 # Merge participant dimensions onto contacts data
 part_dims_to_merge <- intersect(required_participant_cols, names(survey_data$participants))
 # Ensure part_id is always included for merge, plus other required part_ dimensions
 merge_cols_participants <- unique(c("part_id", part_dims_to_merge))

 # Select required contact dimensions + part_id
 merge_cols_contacts <- unique(c("part_id", required_contact_cols))
 contacts_to_merge <- survey_data$contacts[, ..merge_cols_contacts]

 # Merge contact data with participant dimensions
 gam_data_full <- merge(
      contacts_to_merge,
      survey_data$participants[, ..merge_cols_participants],
      by = "part_id",
      all.x = FALSE # Keep only contacts with matching participants (already filtered)
 )
 # Merge calculated weights onto the combined data
 gam_data_full <- merge(
     gam_data_full,
     participant_weights_to_merge,
     by = "part_id",
     all.x = FALSE # Should not lose rows if weights calculated correctly for all participants
 )

  # Check if all dimensions are now in the merged data
  missing_dims_in_merged <- setdiff(dimensions, names(gam_data_full))
  if (length(missing_dims_in_merged) > 0) {
      stop(paste("Internal error: After merging, the following dimensions are missing:",
                paste(missing_dims_in_merged, collapse=", ")))
  }

  # Convert character dimensions to factors for GAM compatibility if needed
  for(dim_name in dimensions) {
      if(is.character(gam_data_full[[dim_name]])) {
          message(paste("Converting character dimension", dim_name, "to factor."))
          # Ensure levels used match dim_breaks if provided
           if(dim_name %in% names(dim_breaks) && !is.numeric(dim_breaks[[dim_name]])) {
                gam_data_full[, (dim_name) := factor(get(dim_name), levels = dim_breaks[[dim_name]])]
           } else {
               gam_data_full[, (dim_name) := as.factor(get(dim_name))]
           }
      } else if (is.factor(gam_data_full[[dim_name]])) {
          # If already factor, ensure levels match dim_breaks if specified
           if(dim_name %in% names(dim_breaks) && !is.numeric(dim_breaks[[dim_name]])) {
                current_levels <- levels(gam_data_full[[dim_name]])
                target_levels <- dim_breaks[[dim_name]]
                if (!identical(current_levels, target_levels)) {
                   message(paste("Re-leveling factor", dim_name, "to match dim_breaks"))
                   gam_data_full[, (dim_name) := factor(get(dim_name), levels = target_levels)]
                }
           }
      }
  }


 # Aggregate contacts: Calculate SUM of weights and count (N) for each combination
 # The sum of weights becomes the response variable.
 # N (number of raw contacts in the bin) will be used as the weight argument in gam().
 grouping_vars <- dimensions
 message("Aggregating contact data by specified dimensions...")
 gam_data_agg <- gam_data_full[, .(response = sum(calculated_weight, na.rm = TRUE), N = .N), by = c(grouping_vars)]
 message(paste("Aggregated data has", nrow(gam_data_agg), "rows."))

 # Check for zero counts - these might indicate issues or sparse data
 if (any(gam_data_agg$N == 0)) {
     warning("Some aggregated bins have N=0 contacts. This might affect model fitting if weights are based on N.")
     # Consider removing rows with N=0 if they cause issues?
     # gam_data_agg <- gam_data_agg[N > 0]
 }
 # Check for zero response (sum of weights)
 if (any(gam_data_agg$response == 0 & gam_data_agg$N > 0)) {
      warning("Some aggregated bins have N > 0 but sum of weights (response) is 0. Check weight calculations.")
 }


  # --- 2. Build or Use GAM Formula ---
  formula_string_to_return <- NULL # Initialize
  if (is.null(gam_formula)) {
    # --- Build GAM Formula Automatically ---
    message("Building GAM formula automatically (response ~ terms)...")
    formula_terms <- c()

    # Check argument validity
    if(!is.numeric(k_tensor) || length(k_tensor) != 2) stop("`k_tensor` must be a numeric vector of length 2.")
    if(!is.numeric(k_by) || length(k_by) != 1) stop("`k_by` must be a single numeric value.")

    numeric_suffixes <- dim_suffixes[sapply(dim_suffixes, function(sfx) is.numeric(gam_data_agg[[paste0("part_", sfx)]]))]
    categorical_suffixes <- dim_suffixes[sapply(dim_suffixes, function(sfx) !is.numeric(gam_data_agg[[paste0("part_", sfx)]]))]

    for (suffix in dim_suffixes) {
        part_var <- paste0("part_", suffix)
        cnt_var <- paste0("cnt_", suffix)

        # Check data type based on aggregated data
         is_numeric_part <- is.numeric(gam_data_agg[[part_var]])
         is_numeric_cnt <- is.numeric(gam_data_agg[[cnt_var]])

         if (is_numeric_part != is_numeric_cnt) {
              stop(paste("Dimension type mismatch for pair:", suffix, "(", part_var, "and", cnt_var, ")"))
         }

         if (is_numeric_part) { # Numeric pair (e.g., age)
             # Tensor product interaction
             formula_terms <- c(formula_terms,
                               paste0("te(", part_var, ", ", cnt_var,
                                      ", k = c(", k_tensor[1], ", ", k_tensor[2], "), bs = \"", bs_numeric, "\")"))
         } else { # Categorical pair (e.g., gender, ethnicity)
             # Check if factors have enough levels for interaction
              part_levels = levels(gam_data_agg[[part_var]])
              cnt_levels = levels(gam_data_agg[[cnt_var]])
              if(length(part_levels) < 2 || length(cnt_levels) < 2) {
                  warning(paste("Categorical pair", suffix, "has less than 2 levels in aggregated data. Interaction term might be problematic. Using simple factor terms instead."))
                  formula_terms <- c(formula_terms, part_var, cnt_var) # Add main effects only
              } else {
                 formula_terms <- c(formula_terms,
                                  paste0("interaction(", part_var, ", ", cnt_var, ")"))
              }
         }
    }

    # Add smooths for numeric variables varying by interactions of categorical variables
    if (length(numeric_suffixes) > 0 && length(categorical_suffixes) > 0) {
        # Create interaction term string for all categorical pairs
        categorical_interaction_terms <- c()
        valid_categorical_suffixes_for_by <- c() # Store suffixes with >1 level for 'by'
        for (sfx in categorical_suffixes) {
            part_cat_var <- paste0("part_", sfx)
            cnt_cat_var <- paste0("cnt_", sfx)
            # Ensure factors have sufficient levels in the aggregated data
             part_levels = levels(gam_data_agg[[part_cat_var]])
             cnt_levels = levels(gam_data_agg[[cnt_cat_var]])
             if (length(part_levels) >= 1 && length(cnt_levels) >= 1) { # Need at least 1 level to form term
                 categorical_interaction_terms <- c(categorical_interaction_terms, paste0("interaction(", part_cat_var, ", ", cnt_cat_var, ")"))
                 if (length(part_levels) >= 2 || length(cnt_levels) >= 2) { # Need >1 level somewhere for 'by' to make sense
                      valid_categorical_suffixes_for_by <- c(valid_categorical_suffixes_for_by, sfx)
                 }
             } else {
                  warning(paste("Skipping categorical pair", sfx, "from 'by' interaction due to insufficient levels."))
             }
        }

        if (length(categorical_interaction_terms) > 0 && length(valid_categorical_suffixes_for_by) > 0) {
             by_var_str <- paste(categorical_interaction_terms, collapse = ":") # Combine all interactions

             for (num_suffix in numeric_suffixes) {
                 part_num_var <- paste0("part_", num_suffix)
                 cnt_num_var <- paste0("cnt_", num_suffix)

                 # Add s(..., by = ...) terms
                 formula_terms <- c(formula_terms,
                                   paste0("s(", part_num_var, ", by = ", by_var_str, ", k = ", k_by, ", bs = \"", bs_numeric, "\")"))
                 formula_terms <- c(formula_terms,
                                   paste0("s(", cnt_num_var, ", by = ", by_var_str, ", k = ", k_by, ", bs = \"", bs_numeric, "\")"))
             }
        } else {
             warning("Could not create valid 'by' interaction term for smooths. Adding simple numeric smooths instead.")
             # Add simple smooths if 'by' interaction failed
             for (num_suffix in numeric_suffixes) {
                part_num_var <- paste0("part_", num_suffix)
                cnt_num_var <- paste0("cnt_", num_suffix)
                # Add only if not already covered by te() - though te() doesn't strictly include marginals
                # Typically, we *do* want the marginal smooths alongside the te()
                formula_terms <- c(formula_terms,
                                   paste0("s(", part_num_var, ", k = ", k_tensor[1], ", bs = \"", bs_numeric, "\")"))
                formula_terms <- c(formula_terms,
                                   paste0("s(", cnt_num_var, ", k = ", k_tensor[2], ", bs = \"", bs_numeric, "\")"))
             }
        }
    } else if (length(numeric_suffixes) > 0) {
         # If only numeric dimensions, add simple smooths (main effects to complement te())
         for (num_suffix in numeric_suffixes) {
            part_num_var <- paste0("part_", num_suffix)
            cnt_num_var <- paste0("cnt_", num_suffix)
            # Add marginal smooths
            formula_terms <- c(formula_terms,
                               paste0("s(", part_num_var, ", k = ", k_tensor[1], ", bs = \"", bs_numeric, "\")"))
            formula_terms <- c(formula_terms,
                               paste0("s(", cnt_num_var, ", k = ", k_tensor[2], ", bs = \"", bs_numeric, "\")"))
         }
    }

    # Assemble final formula string - USING 'response' as the outcome
    if (length(formula_terms) == 0) {
        stop("Could not generate any terms for the GAM formula based on dimensions.")
    }
    formula_str_generated <- paste("response ~", paste(unique(formula_terms), collapse = " + ")) # Use unique terms
    message("Generated GAM formula: ", formula_str_generated)
    formula_to_use <- as.formula(formula_str_generated) # Convert to formula for gam()
    formula_string_to_return <- formula_str_generated # Keep string for return
  } else {
    # --- Use Provided GAM Formula ---
     if (!inherits(gam_formula, "formula")) {
        stop("`gam_formula` must be a formula object or NULL.")
     }
     # Check if formula uses 'response' as outcome, warn if not
      resp_var <- all.vars(gam_formula[[2]])
      if (resp_var != "response") {
          warning("Provided `gam_formula` does not use 'response' as the outcome variable. Fitting will use the sum of weights ('response') regardless.")
          # Optionally, attempt to substitute 'response' into the formula
           gam_formula[[2]] <- quote(response)
      }
     message("Using provided GAM formula (ensuring outcome is 'response').")
     formula_to_use <- gam_formula
     formula_string_to_return <- deparse(formula_to_use) # Store provided formula as string
  }


  # --- 3. Fit GAM ---
  message("Fitting GAM model (using N as weights)...")
  # Use 'N' (raw contact count in bin) as weights argument for gam
  # This assumes the 'response' (sum of weights) scales with N,
  # and gives more importance to bins with more raw contacts.
  if (!"N" %in% names(gam_data_agg)) {
      stop("Internal error: Aggregated data does not contain 'N' column for GAM weights.")
  }
  gam_weights <- gam_data_agg$N

  gam_fit <- mgcv::gam(
      formula = formula_to_use, # Uses 'response' as outcome
      data = gam_data_agg,
      family = family,       # Family describes error distribution of 'response'
      weights = gam_weights, # Use raw count N as weights
      method = "REML",       # Default method, often robust
      ...                    # Pass additional args to gam
  )
  message("GAM fitting complete.")

  # --- 4. Create Prediction Grid ---
  message("Creating prediction grid...")
  prediction_grid_list <- lapply(dimensions, function(dim_name) {
    breaks <- dim_breaks[[dim_name]]
    if (is.numeric(breaks)) {
      # Create midpoints from breaks for prediction
      if(length(breaks) < 2) stop(paste("Numeric dimension", dim_name, "needs at least 2 breaks."))
      return(breaks[-length(breaks)] + diff(breaks) / 2)
    } else {
      # Use levels directly for factors/characters
       # Ensure levels used for prediction match factor levels in fitted data
       if (is.factor(gam_data_agg[[dim_name]])) {
           data_levels <- levels(gam_data_agg[[dim_name]])
            provided_levels <- as.character(breaks) # breaks should be the levels
            if (!all(provided_levels %in% data_levels)) {
                 warning(paste("Some levels provided in dim_breaks for", dim_name,
                              "are not present in the modeling data factors:",
                               paste(setdiff(provided_levels, data_levels), collapse=", "),
                               ". Prediction for these levels might be unreliable or produce errors."))
            }
             # Return factor with levels matching data, subsetted by provided breaks
             # Using data_levels ensures consistency with the model fit
             return(factor(provided_levels, levels=data_levels))

       } else if (is.character(breaks)) {
            # If input was character and converted to factor in data prep, use factor levels from data
            if (is.factor(gam_data_agg[[dim_name]])) {
                 data_levels <- levels(gam_data_agg[[dim_name]])
                 provided_levels <- as.character(breaks)
                 if (!all(provided_levels %in% data_levels)) {
                     warning(paste("Some levels provided in dim_breaks for character dim", dim_name,
                              "are not present in the modeling data factors:",
                               paste(setdiff(provided_levels, data_levels), collapse=", ")))
                 }
                  return(factor(provided_levels, levels=data_levels))
            } else {
                # If it remained character (unlikely), use breaks directly
                 return(breaks)
            }
       } else {
            stop(paste("Unsupported break type for dimension:", dim_name))
       }
    }
  })
  names(prediction_grid_list) <- dimensions

  # Ensure factor levels in prediction grid match those used in model fitting
  prediction_grid <- do.call(expand.grid, c(prediction_grid_list, list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)))


  # --- 5. Predict using GAM ---
  message("Predicting contact rates (sum of weights) on grid...")
  # Need to handle potential missing factor levels between model data and prediction grid
  predicted_values <- tryCatch({
        mgcv::predict.gam(
           gam_fit,
           newdata = prediction_grid,
           type = "response", # Get predictions on the scale of the response (sum of weights)
           se.fit = FALSE     # Optionally get standard errors later if needed
       )
    }, error = function(e) {
        # Common error is factor level mismatch
        if (grepl("factor .* has new levels", e$message)) {
            # Identify the problematic factor and levels
             factor_match <- regmatches(e$message, regexpr("'([^']*)'", e$message))
             problem_factor <- ifelse(length(factor_match) > 0, factor_match[1], "unknown factor")
             # Provide more context
             problematic_levels <- setdiff(levels(prediction_grid[[gsub("'", "", problem_factor)]]), levels(gam_data_agg[[gsub("'", "", problem_factor)]]))

            stop(paste("Error during prediction: Factor level mismatch for", problem_factor, ".",
                       "Levels in prediction grid not found in model data:", paste(problematic_levels, collapse=", "), ".",
                       "Ensure levels in `dim_breaks` for categorical dimensions",
                       "exactly match the levels present *after aggregation* in the data used for fitting.",
                       "Original error:", e$message), call. = FALSE)
        } else {
            stop(paste("Error during prediction:", e$message), call. = FALSE)
        }
  })
   message("Prediction complete.")

  # --- 6. Reshape into Matrix/Array ---
  # Determine the dimensions of the output array based on the number of levels/bins
   # For factors, use the length of the provided breaks/levels
   array_dims <- sapply(names(prediction_grid_list), function(dim_name) {
       breaks <- dim_breaks[[dim_name]]
       if (is.numeric(breaks)) {
           return(length(breaks) - 1) # Number of intervals
       } else {
           # Use the number of levels specified in dim_breaks
            return(length(breaks))
       }
   })
  names(array_dims) <- dimensions # Keep dimension names

  # Create dimnames for the array
  array_dimnames <- lapply(dimensions, function(dim_name) {
      breaks <- dim_breaks[[dim_name]]
      if (is.numeric(breaks)) {
          # Use interval notation like contact_matrix
           lower <- breaks[-length(breaks)]
           upper <- breaks[-1]
           # Format nicely
           format_interval <- function(l, u) {
               paste0("[", format(l, trim=TRUE), ",", format(u, trim=TRUE), ")")
           }
           return(format_interval(lower, upper))
      } else {
          # Use levels provided in dim_breaks directly
           return(as.character(breaks))
      }
  })
  names(array_dimnames) <- dimensions

  # Reshape the predicted values into the array
  # Ensure the order of prediction matches the order of the reshaped array
  # expand.grid generates data varying the first variable fastest.
  # The array() function fills by varying the first index fastest. So the order should match.
  contact_array <- array(
      data = predicted_values,
      dim = array_dims,
      dimnames = array_dimnames
  )

  # --- 7. Return Results ---
  # Assign gam_fit to a temp variable to avoid potential naming conflicts
  final_gam_fit <- gam_fit
  # Include the generated formula string in the return list
  final_formula_string <- formula_string_to_return

  return(
    list(
      matrix = contact_array, # This now represents predicted sum of weights
      gam_fit = final_gam_fit,
      prediction_grid = prediction_grid,
      dimensions = dimensions,
      dim_breaks = dim_breaks,
      gam_formula_str = final_formula_string # Return the formula string used
    )
  )
}

# Helper function (if not already present) - reduce_agegroups
# Ensure this exists or is defined if needed by other parts of contact_matrix.R
# reduce_agegroups <- function(ages, limits) { ... }

# Ensure pop_age exists if weigh.age=TRUE
# It should be available if socialmixr is loaded, but good practice to note dependency.

