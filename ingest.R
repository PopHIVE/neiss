# =============================================================================
# NEISS (National Electronic Injury Surveillance System) Data Ingestion
# Source: U.S. Consumer Product Safety Commission (CPSC), 2019-onward public
#         files (latest year auto-detected at run time; see neiss_latest_year)
#         https://www.cpsc.gov/cgibin/NEISSQuery/
#
# Produces stratified injury COUNTS for the PopHIVE platform. NEISS is a
# national probability sample of ~100 U.S. hospital emergency departments, so
# all geography = "00" (national); there is no state breakdown in the public
# files. Records carry survey weights (weight/psu/stratum) used to scale the
# sampled counts up to national estimates.
#
# Two age schemes x two injury breakdowns => four standard files:
#   data_infant_diagnosis.csv.gz    children <2, by age in MONTHS,  x diagnosis
#   data_infant_product.csv.gz      children <2, by age in MONTHS,  x product group
#   data_agegroup_diagnosis.csv.gz  12 age GROUPS,                  x diagnosis
#   data_agegroup_product.csv.gz    12 age GROUPS,                  x product group
# Every file is additionally stratified by sex, race, and hispanic ethnicity.
# For each stratum we emit neiss_injuries (raw sampled count) and
# neiss_injuries_weighted (sum of survey weights = national estimate).
#
# Plus two long files of individual CPSC products (section 9b), by age x sex:
#   data_infant_product_code.csv.gz    children <2, by age in MONTHS
#   data_agegroup_product_code.csv.gz  12 age GROUPS
# These count a visit under every product on its record (prod1-prod3) and use
# CPSC's own product titles, from the NEISS_FMT sheet in each workbook.
#
# The scrape + recoding is adapted from read_neiss_2019_2025.R (hadley/neiss
# approach updated for the 2019+ 25-column CPSC layout). Product GROUPS come from
# the NEISS product-code bands (a pure numeric function), so no product-title
# lookup or coding-manual PDF is needed here; only lookups.rds (diagnosis/sex/
# race code tables) is required.
# =============================================================================

library(dplyr)
library(tidyr)
library(readxl)

# 2019 is the first year of the modern 25-column CPSC layout this script reads
# (2013-2018 files have a different 19-column schema). The latest year is NOT
# hardcoded: CPSC posts each year's file ~April of the following year, so we
# detect it at run time (see neiss_latest_year() below).
min_year <- 2019

dir.create("raw", showWarnings = FALSE)

neiss_url <- function(y) sprintf(
  "https://www.cpsc.gov/cgibin/NEISSQuery/Data/Archived%%20Data/%d/neiss%d.xlsx",
  y, y
)

# -----------------------------------------------------------------------------
# 1. Determine the year range (detect the latest available year)
# -----------------------------------------------------------------------------
# CPSC sits behind Akamai, which 503s HEAD requests but honors a tiny ranged
# GET: a 200/206 means the file exists, 404 means it is not posted yet. We walk
# back from the current calendar year to the first year that responds.
remote_file_exists <- function(url) {
  tryCatch({
    h <- curl::new_handle(followlocation = TRUE)
    curl::handle_setheaders(h, Range = "bytes=0-0")
    curl::curl_fetch_memory(url, handle = h)$status_code %in% c(200L, 206L)
  }, error = function(e) NA)   # NA = probe failed (e.g. offline)
}

neiss_latest_year <- function(floor_year = min_year) {
  start <- as.integer(format(Sys.Date(), "%Y"))
  for (y in seq(start, floor_year)) {
    ok <- remote_file_exists(neiss_url(y))
    if (isTRUE(ok)) {
      message("Detected latest available NEISS year: ", y)
      return(y)
    }
  }
  NA_integer_   # could not detect (offline, or probing blocked)
}

# Years already downloaded locally (fallback if remote detection fails).
existing     <- list.files("raw", pattern = "^neiss\\d{4}\\.xlsx$")
local_years  <- as.integer(sub("^neiss(\\d{4})\\.xlsx$", "\\1", existing))
detected     <- neiss_latest_year()
max_year     <- max(c(min_year, detected, local_years), na.rm = TRUE)
years        <- min_year:max_year
message("Processing NEISS years: ", min_year, "-", max_year)

# -----------------------------------------------------------------------------
# 2. Download raw yearly .xlsx (skip any already present & valid)
# -----------------------------------------------------------------------------
# download.file(method = "libcurl") gets through Akamai where most clients get
# 403, but it is intermittent, so we retry + validate. Files already in raw/ are
# reused (fast, no re-download).
local  <- file.path("raw", sprintf("neiss%d.xlsx", years))
remote <- vapply(years, neiss_url, character(1))

valid_xlsx <- function(f) {
  file.exists(f) && isTRUE(tryCatch({ excel_sheets(f); TRUE },
                                    error = function(e) FALSE))
}

download_neiss <- function(url, dest, tries = 8) {
  for (i in seq_len(tries)) {
    if (valid_xlsx(dest)) return(invisible(TRUE))
    if (file.exists(dest)) file.remove(dest)          # drop corrupt/partial file
    message(sprintf("  [%d/%d] downloading %s", i, tries, basename(dest)))
    tryCatch(
      suppressWarnings(
        download.file(url, dest, method = "libcurl", mode = "wb", quiet = TRUE)
      ),
      error = function(e) message("    ", conditionMessage(e))
    )
    if (valid_xlsx(dest)) return(invisible(TRUE))
    Sys.sleep(3)
  }
  stop("Could not download a valid NEISS file: ", dest,
       "\n  Download it manually in a browser from:\n  ", url,
       "\n  and save it as: ", dest)
}

invisible(Map(download_neiss, remote, local))

# -----------------------------------------------------------------------------
# 3. Change detection (dcf)
# -----------------------------------------------------------------------------
raw_state <- as.list(tools::md5sum(local))
process   <- dcf::dcf_process_record()

if (!identical(process$raw_state, raw_state)) {

  # ---------------------------------------------------------------------------
  # 4. Read raw files (2019+ 25-column layout)
  # ---------------------------------------------------------------------------
  col_types <- c(
    case_num    = "text",    trmt_date   = "date",    age         = "numeric",
    sex         = "text",    race        = "text",    race_other  = "text",
    hispanic    = "text",    body_part   = "numeric", diag        = "numeric",
    diag_other  = "text",    body_part2  = "numeric", diag2       = "numeric",
    diag_other2 = "text",    disposition = "numeric", location    = "numeric",
    fmv         = "numeric", prod1       = "numeric", prod2       = "numeric",
    prod3       = "numeric", alcohol     = "numeric", drug        = "numeric",
    narrative   = "text",    stratum     = "text",    psu         = "numeric",
    weight      = "numeric"
  )

  raw <- lapply(local, read_excel, col_types = unname(col_types))
  all <- bind_rows(raw)
  names(all) <- names(col_types)

  # ---------------------------------------------------------------------------
  # 5. Recode via hadley/neiss code tables (download lookups.rds if absent)
  # ---------------------------------------------------------------------------
  lookups_file <- file.path("raw", "lookups.rds")
  if (!file.exists(lookups_file)) {
    download.file(
      "https://raw.githubusercontent.com/hadley/neiss/master/data-raw/lookups.rds",
      lookups_file, method = "libcurl", mode = "wb", quiet = TRUE
    )
  }
  lookups <- readRDS(lookups_file)
  lookup  <- function(needle, haystack) unname(haystack[as.character(needle)])

  # National population by single-year age x sex (Census Vintage 2023), used as
  # the denominator for the age x sex rate files (section 9). Downloaded once.
  pop_file <- file.path("raw", "nc-est2023-agesex-res.csv")
  if (!file.exists(pop_file)) {
    download.file(
      paste0("https://www2.census.gov/programs-surveys/popest/datasets/",
             "2020-2023/national/asrh/nc-est2023-agesex-res.csv"),
      pop_file, mode = "wb", quiet = TRUE
    )
  }

  # Higher-level product grouping by NEISS code band (leading digits). NEISS
  # ships no category field; these bands are the historical product-group
  # structure reconstructed from the code ranges (approximate, not an official
  # CPSC file). Every code maps to a group, so there is no missingness.
  # The bands put some products in misleading groups (beds 4076, tables 4057
  # and chairs 4074 fall in "sports & recreation activities"; floors 1807,
  # stairs 1842 and doors 1893 in "personal use, drugs & misc."). For
  # rankings of what injures people, use the individual-product files from
  # section 9b instead.
  prod_group <- function(code) {
    case_when(
      is.na(code)                 ~ NA_character_,
      code >= 100  & code <= 199  ~ "general household appliances",
      code >= 200  & code <= 299  ~ "kitchen appliances",
      code >= 300  & code <= 399  ~ "heating, cooling & ventilation",
      code >= 400  & code <= 499  ~ "housewares",
      code >= 500  & code <= 599  ~ "home communication, entertainment & hobby",
      code >= 600  & code <= 699  ~ "home furnishings & fixtures",
      code >= 700  & code <= 799  ~ "home structures & construction materials",
      code >= 800  & code <= 899  ~ "home workshop equipment & tools",
      code >= 900  & code <= 999  ~ "chemicals",
      code >= 1100 & code <= 1199 ~ "packaging & containers",
      code >= 1200 & code <= 1399 ~ "sports/recreation equipment & toys",
      code >= 1400 & code <= 1499 ~ "yard & garden equipment",
      code >= 1500 & code <= 1599 ~ "child nursery equipment",
      code >= 1600 & code <= 1999 ~ "personal use, drugs & misc.",
      code >= 3000 & code <= 5999 ~ "sports & recreation activities",
      TRUE                        ~ "other/unspecified"
    )
  }

  injuries <- all %>%
    mutate(
      year = as.integer(format(as.Date(trmt_date), "%Y")),
      # Age coding: children under 2 are coded in months as (code - 200), i.e.
      # 201-223 = 1-23 months. There is no code 200; the youngest infants
      # (under ~1 month) are coded as a plain 0. NEISS never codes a 1-year-old
      # as a plain "1" (they are month-coded 212-223), so plain 0 is the only
      # whole-year code that falls under 2 years.
      age_years  = ifelse(age >= 200, (age - 200) / 12, age),
      age_months = case_when(
        age == 0               ~ 0,          # neonate coded as 0 -> 0 months
        age >= 200 & age < 224 ~ age - 200,  # 201-223 -> 1-23 months
        TRUE                   ~ NA_real_
      ),
      sex = coalesce(lookup(sex, lookups$sex), "Unknown"),
      race = coalesce(lookup(race, lookups$race), "Not stated"),
      hispanic = case_when(
        hispanic == "1" ~ "Hispanic",
        hispanic == "2" ~ "Non-Hispanic",
        TRUE            ~ "Unknown"          # code 0 = unknown (and any NA)
      ),
      diagnosis     = coalesce(lookup(diag, lookups$diag), "Other Or Not Stated"),
      product_group = coalesce(prod_group(prod1), "other/unspecified"),
      geography = "00",
      time      = sprintf("%d-12-31", year)
    )


  # ---------------------------------------------------------------------------
  # 7. Aggregate to stratified counts
  # ---------------------------------------------------------------------------
  # Output is WIDE (like the other PopHIVE datasets): index columns
  # (geography, time, age, sex, race, hispanic) plus one value column per
  # breakdown category. Each category gets two columns:
  #   neiss_n_{category}  = raw count of sampled records
  #   neiss_wt_{category} = weighted national estimate (rounded sum of weights)
  # Stratum x category combinations with no injuries are filled with 0.
  slugify <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("_+", "_", x)
    gsub("^_|_$", "", x)
  }

  count_wide <- function(df, age_col, breakdown_col) {
    df %>%
      transmute(
        geography, time,
        age = .data[[age_col]],
        sex, race, hispanic,
        cat = slugify(.data[[breakdown_col]]),
        weight
      ) %>%
      group_by(geography, time, age, sex, race, hispanic, cat) %>%
      summarise(
        n  = n(),
        wt = round(sum(weight, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        id_cols     = c(geography, time, age, sex, race, hispanic),
        names_from  = cat,
        values_from = c(n, wt),
        names_glue  = "neiss_{.value}_{cat}",
        values_fill = 0
      ) %>%
      arrange(time, age, sex, race, hispanic)
  }

  # Dataset 1: children <2, stratified by age in months
  infants <- injuries %>%
    filter(!is.na(age_months), age_months < 24) %>%
    mutate(age_month = sprintf("%02d months", as.integer(age_months)))

  infant_diagnosis <- count_wide(infants, "age_month", "diagnosis")
  infant_product   <- count_wide(infants, "age_month", "product_group")

  # Dataset 2: standard age groups
  # The first band is "Under 2" so that it lines up exactly with the month-coded
  # records (NEISS codes every child under 2 in months, giving fractional
  # age_years of 0-1.92), keeping the infant files a clean subset of this band.
  age_labels <- c("Under 2", "2-4", "5-9", "10-14", "15-19", "20-29",
                  "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  agegroups <- injuries %>%
    mutate(
      age_group = cut(
        age_years,
        breaks = c(0, 2, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, Inf),
        labels = age_labels, right = FALSE, include.lowest = TRUE
      ),
      age_group = as.character(age_group),
      age_group = ifelse(is.na(age_group), "Unknown", age_group)
    )

  agegroup_diagnosis <- count_wide(agegroups, "age_group", "diagnosis")
  agegroup_product   <- count_wide(agegroups, "age_group", "product_group")

  # ---------------------------------------------------------------------------
  # 8. Write standardized outputs
  # ---------------------------------------------------------------------------
  vroom::vroom_write(infant_diagnosis,   "standard/data_infant_diagnosis.csv.gz",   ",")
  vroom::vroom_write(infant_product,     "standard/data_infant_product.csv.gz",     ",")
  vroom::vroom_write(agegroup_diagnosis, "standard/data_agegroup_diagnosis.csv.gz", ",")
  vroom::vroom_write(agegroup_product,   "standard/data_agegroup_product.csv.gz",   ",")

  report <- function(nm, d) cat(sprintf("  %-24s %6d rows x %3d cols\n",
                                        nm, nrow(d), ncol(d)))
  cat("\nWrote (wide format):\n")
  report("data_infant_diagnosis",   infant_diagnosis)
  report("data_infant_product",     infant_product)
  report("data_agegroup_diagnosis", agegroup_diagnosis)
  report("data_agegroup_product",   agegroup_product)

  # ---------------------------------------------------------------------------
  # 9. Age x sex rates per 100,000 (weighted estimates only)
  # ---------------------------------------------------------------------------
  # Separate, simpler files stratified by age x sex ONLY (not race/hispanic),
  # because the population denominator is available only by age and sex. Rates
  # use the UNROUNDED weighted national estimates as the numerator. The
  # denominator is a constant national population (mean of the Census Vintage
  # 2023 estimates for 2020-2023), applied to every NEISS year; treat rate
  # trends as driven mainly by the numerator. Sex = Unknown and age = Unknown
  # have no denominator and are dropped from the rate files.
  pop <- read.csv(pop_file) %>%
    filter(SEX %in% c(1L, 2L), AGE != 999L) %>%
    transmute(
      AGE,
      sex = ifelse(SEX == 1L, "Male", "Female"),
      pop = rowMeans(cbind(POPESTIMATE2020, POPESTIMATE2021,
                           POPESTIMATE2022, POPESTIMATE2023))
    )

  # Age-group denominators (80+ = ages 80 and over; single years summed).
  pop_group <- pop %>%
    mutate(age = as.character(cut(
      AGE, breaks = c(0, 2, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80, Inf),
      labels = age_labels, right = FALSE, include.lowest = TRUE))) %>%
    group_by(age, sex) %>%
    summarise(pop = sum(pop), .groups = "drop")

  # Infant month denominators: assume births are spread evenly across the year,
  # so each month of age holds ~1/12 of the single-year population (ages 0 and 1
  # cover months 0-11 and 12-23). This is an approximation (no published US
  # population by single month of age).
  pop_month <- bind_rows(
    tidyr::crossing(m = 0:11,  pop %>% filter(AGE == 0L) %>% select(sex, p = pop)),
    tidyr::crossing(m = 12:23, pop %>% filter(AGE == 1L) %>% select(sex, p = pop))
  ) %>%
    transmute(age = sprintf("%02d months", m), sex, pop = p / 12)

  # rate = 100000 * (unrounded weighted injuries) / population, pivoted wide.
  rate_wide <- function(df, age_col, breakdown_col, pop_lookup) {
    df %>%
      filter(sex %in% c("Male", "Female")) %>%
      transmute(geography, time, age = .data[[age_col]], sex,
                cat = slugify(.data[[breakdown_col]]), weight) %>%
      group_by(geography, time, age, sex, cat) %>%
      summarise(wt = sum(weight, na.rm = TRUE), .groups = "drop") %>%   # unrounded
      inner_join(pop_lookup, by = c("age", "sex")) %>%                  # drops Unknown age
      mutate(rate = round(1e5 * wt / pop, 2)) %>%
      tidyr::pivot_wider(
        id_cols     = c(geography, time, age, sex),
        names_from  = cat,
        values_from = rate,
        names_glue  = "neiss_rate_{cat}",
        values_fill = 0
      ) %>%
      arrange(time, age, sex)
  }

  infant_diagnosis_rate   <- rate_wide(infants,   "age_month", "diagnosis",     pop_month)
  infant_product_rate     <- rate_wide(infants,   "age_month", "product_group", pop_month)
  agegroup_diagnosis_rate <- rate_wide(agegroups, "age_group", "diagnosis",     pop_group)
  agegroup_product_rate   <- rate_wide(agegroups, "age_group", "product_group", pop_group)

  vroom::vroom_write(infant_diagnosis_rate,   "standard/data_infant_diagnosis_rate.csv.gz",   ",")
  vroom::vroom_write(infant_product_rate,     "standard/data_infant_product_rate.csv.gz",     ",")
  vroom::vroom_write(agegroup_diagnosis_rate, "standard/data_agegroup_diagnosis_rate.csv.gz", ",")
  vroom::vroom_write(agegroup_product_rate,   "standard/data_agegroup_product_rate.csv.gz",   ",")

  cat("\nWrote (age x sex rates per 100k):\n")
  report("data_infant_diagnosis_rate",   infant_diagnosis_rate)
  report("data_infant_product_rate",     infant_product_rate)
  report("data_agegroup_diagnosis_rate", agegroup_diagnosis_rate)
  report("data_agegroup_product_rate",   agegroup_product_rate)

  # ---------------------------------------------------------------------------
  # 9b. Individual products (CPSC product codes), by age x sex
  # ---------------------------------------------------------------------------
  # One row per year x age x sex x product code present in that cell (absent
  # products are 0 and omitted). Unlike the product_group files:
  #   * each product is a single CPSC product code with CPSC's title, e.g.
  #     "Stairs or steps", "Beds or bedframes, other or not specified";
  #   * each visit counts under every distinct product on its record (prod1,
  #     prod2, prod3), so product shares within a cell can sum to more than
  #     100%;
  #   * sex includes "All" (every sex, including Unknown); race and ethnicity
  #     are not stratified, which keeps the file small.
  # Columns:
  #   neiss_product_injuries           sampled visits involving the product
  #   neiss_product_injuries_weighted  national estimate (rounded sum of weights)
  #   neiss_product_pct_of_all         weighted share of ALL injury visits in the
  #                                    year x age x sex cell, in percent
  #   neiss_product_rank               rank by weighted estimate within the cell
  #                                    (1 = most); NA for 9999 "uncategorized"
  #   neiss_product_unstable           1 if fewer than 20 sampled visits or a
  #                                    national estimate under 1,200 (thresholds
  #                                    set here; no CV / sampling-error check,
  #                                    which needs variance estimation)

  # Product titles from the NEISS_FMT sheet; the newest workbook's title wins.
  # CPSC labels are "1842 - STAIRS OR STEPS" in capitals. Some appear truncated
  # (observed: longest label is 67 characters, some end in an unclosed "(").
  clean_title <- function(x) {
    x <- gsub("\\s+", " ", trimws(x))
    # drop a parenthetical left unclosed by the apparent truncation
    n_open  <- lengths(regmatches(x, gregexpr("(", x, fixed = TRUE)))
    n_close <- lengths(regmatches(x, gregexpr(")", x, fixed = TRUE)))
    cut <- n_open > n_close
    x[cut] <- sub("\\s*\\([^()]*$", "", x[cut])
    x <- tolower(x)
    x <- paste0(toupper(substr(x, 1, 1)), substring(x, 2))
    x <- gsub(",\\s*\\(", " (", x)                 # "accessories, (excl" -> "accessories (excl"
    x <- gsub("\\b(excl|incl)\\.(?=\\S)", "\\1. ", x, perl = TRUE)
    for (a in c("ATV", "TV", "CB", "VCR", "DVD", "CD", "PVC", "LP", "BB")) {
      x <- gsub(paste0("\\b", a, "(s?)\\b"), paste0(a, "\\1"), x, ignore.case = TRUE, perl = TRUE)
    }
    x <- gsub("\\bchristmas\\b", "Christmas", x, perl = TRUE)
    sub("[,; ]+$", "", x)
  }
  product_titles <- bind_rows(lapply(rev(local), function(f) {
    fmt <- read_excel(f, sheet = "NEISS_FMT", col_types = "text")
    names(fmt) <- c("fmt", "start", "end", "label")
    fmt[fmt$fmt == "PROD", c("start", "label")]
  })) %>%
    transmute(product_code = as.integer(start),
              product = sub("^\\s*\\d+\\s*-\\s*", "", label)) %>%
    filter(!is.na(product_code)) %>%
    distinct(product_code, .keep_all = TRUE) %>%
    mutate(product = clean_title(product))

  product_long <- function(df, age_col) {
    df <- df %>%
      transmute(rec = row_number(), time, age = .data[[age_col]], sex, weight,
                prod1, prod2, prod3)
    df <- bind_rows(df, mutate(df, sex = "All"))
    totals <- df %>%
      group_by(time, age, sex) %>%
      summarise(total_wt = sum(weight, na.rm = TRUE), .groups = "drop")
    df %>%
      tidyr::pivot_longer(c(prod1, prod2, prod3), values_to = "product_code") %>%
      filter(!is.na(product_code), product_code != 0) %>%
      distinct(rec, sex, product_code, .keep_all = TRUE) %>%   # once per visit
      group_by(time, age, sex, product_code) %>%
      summarise(n = n(), wt = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      inner_join(totals, by = c("time", "age", "sex")) %>%
      left_join(product_titles, by = "product_code") %>%
      group_by(time, age, sex) %>%
      mutate(
        neiss_product_rank = ifelse(
          product_code == 9999, NA_integer_,
          rank(ifelse(product_code == 9999, Inf, -wt), ties.method = "min")
        )
      ) %>%
      ungroup() %>%
      transmute(
        geography = "00", time, age, sex,
        product_code = as.integer(product_code),
        product = coalesce(product, paste("Product code", product_code)),
        neiss_product_injuries          = n,
        neiss_product_injuries_weighted = round(wt),
        neiss_product_pct_of_all        = round(100 * wt / total_wt, 2),
        neiss_product_rank              = as.integer(neiss_product_rank),
        neiss_product_unstable          = as.integer(n < 20 | wt < 1200)
      ) %>%
      arrange(time, age, sex, neiss_product_rank)
  }

  infant_product_code   <- product_long(infants,   "age_month")
  agegroup_product_code <- product_long(agegroups, "age_group")

  vroom::vroom_write(infant_product_code,   "standard/data_infant_product_code.csv.gz",   ",")
  vroom::vroom_write(agegroup_product_code, "standard/data_agegroup_product_code.csv.gz", ",")

  cat("\nWrote (individual products, long):\n")
  report("data_infant_product_code",   infant_product_code)
  report("data_agegroup_product_code", agegroup_product_code)

  # ---------------------------------------------------------------------------
  # 10. Record processed state
  # ---------------------------------------------------------------------------
  process$raw_state <- raw_state
  dcf::dcf_process_record(updated = process)
}
