require(tidyverse)
require(stringi)
require(zoo)
require(lubridate)
require(conflicted)
conflict_prefer("filter", "dplyr")

#### LOAD DATA ####

#I cleaned the data in a separate script
companies <- read.csv("crunchbase/companies.csv")
first_rounds <- read.csv("crunchbase/first_rounds.csv")
investors <- read.csv("crunchbase/investors.csv")
investments <- read.csv("crunchbase/investments.csv") %>%
  mutate(funded_at = as.Date(funded_at),
         funded_object_id = as.character(funded_object_id),
         funding_round_code = as.character(funding_round_code))
funding_rounds <- read.csv("crunchbase/funding_rounds.csv") %>%
  mutate(funded_at = as.Date(funded_at),
         object_id = as.character(object_id),
         funding_round_code = as.character(funding_round_code))
ipos <- read.csv("crunchbase/ipos.csv")
funds <- read.csv("crunchbase/funds.csv")
accelerator <- read.csv("crunchbase/accelerators.csv")
family_office <- read.csv("crunchbase/family_office.csv")
institutional <- read.csv("crunchbase/institutional.csv")

investment_cols <- c("id","funding_round_id", "funded_at","funding_round_type",
                     "funding_round_code", "raised_amount_usd", "company_name",
                     "investor_name", "funded_object_id", "investor_object_id", "type")
funding_cols <- c("name", "id", "funded_at",
                  "funding_round_type", "funding_round_code",
                  "round_n", "object_id")

#### SIMULATION ####

SimulateVirality <- function(n_customers,
                             predisposition = c(.2, .6, .2),
                             conversion_rates = c(.9, .5, 0)) {
  ###################################
  # Simulates the number of new customers per quarter.
  #
  # Customers can come from sales and marketing or referral partners.
  #
  # When a company becomes a customers, every investor in that
  # company gets an account. When a customer raises a round,
  # every investor in that round gets an account.
  #
  # Every time an investor gets an account, they have a chance to have
  # the `aha` moment and become a referral partner, which is determined
  # by their pre-disposition, either `open`, `typical` or `closed`.
  #
  # Referral partners bring in 25% of their portfolio every quarter, unless
  # there's more than 75% of their portfolio on the platform. In that case,
  # they bring all of their remaining investments as customers.
  #
  # Track investors as they go from unaware to aware to partner.
  #
  # Args:
  #   n_customers: Number of customers that sales and marketing generates
  #                each quarter. It is constant quarter to quarter.
  #   pre_dispositions: Percent of investors with each of the pre-dispositions.
  #                     The first element is the percent of investors `open`,
  #                     the second element is the percent of investors `typical`,
  #                     and the third element is the percent of investors `closed`.
  #   conversion_rates: Rate at which each group of investors convert to partners.
  #                     The first element is the conversion rate for `open` investors,
  #                     the second element is the conversion rate for `typical` investors
  #                     and the third element is the conversion rate for `closed` investors.
  #
  ###################################

  # Define quarters.
  simulation_start_date <- ymd("2009-01-01")
  simulation_end_date <- ymd("2010-09-30")
  quarter_seq <- seq(as.yearqtr(simulation_start_date),
                     as.yearqtr(simulation_end_date),
                     .25)

  # Assign each investor an attitude towards partnership
  assign_predispositions <-
    data.frame(investor_object_id = unique(investments$investor_object_id),
               group              = sample(c("open", "typical", "closed"),
                                            length(unique(investments$investor_object_id)),
                                              replace = TRUE, prob = predisposition),
               stringsAsFactors   = FALSE) %>%
    as_tibble()


  # Per quarter, simulate new customers, newly aware investors and new referral partners.
  for(i in 1:(length(quarter_seq)-1)){
    # Begin
    current_quarter <- quarter_seq[i]
    next_quarter <- quarter_seq[i+1]
    print(current_quarter)

    # Generate tables for tracking customers and investors
    if(i == 1){
      file.create("summary.txt")
      all_customers <- data.frame(customer_id       = character(0),
                                  date_started      = as.Date(character(0)),
                                  source            = character(0),
                                  stage             = character(0),
                                  start_dates       = as.Date(character(0)),
                                  stringsAsFactors  = FALSE) %>%
                       as_tibble()
      all_aware_dates <- data.frame(investor_object_id  = character(0),
                              first_aware         = as.Date(character(0)),
                              stringsAsFactors    = FALSE) %>%
                       as_tibble()
      all_partner_dates <- data.frame(investor_object_id  = character(0),
                                 first_partner       = as.Date(character(0)),
                                 stringsAsFactors    = FALSE) %>%
                       as_tibble()
      customers_from_partners_tibble <- data.frame(customer_id       = character(0),
                                                   source            = character(0),
                                                   stringsAsFactors  = FALSE) %>%
                                        as_tibble()
    } else {
      #### Track every new customer, from referral partners or sales and marketing ####

      ## New customers from referral partners ##

      # Get the portfolio per investor.
      partner_portfolios <-
        investments[investment_cols] %>%
        filter(funded_at < as.Date(current_quarter) &
                 investor_object_id %in% all_partner_dates$investor_object_id)

      # Create a list to store the new customers
      customers_from_partners <- vector(mode = "list", length = nrow(all_partner_dates))

      # Identify possible customers in each investor's portfolio and then add new customers.
      for (j in 1:nrow(all_partner_dates)){
        portfolio_per_partner <-
          unique(with(partner_portfolios,
                      partner_portfolios[investor_object_id ==
                                           all_partner_dates$investor_object_id[j], ]$funded_object_id))
        potential_customers <-
          portfolio_per_partner[!portfolio_per_partner %in% all_customers$customer_id]

        # Find the number of new customers. Either 25% of their portfolio,
        # if that is greater than the number of opportunities, then all of their remaining investments.
        customer_count <- ifelse(floor(length(portfolio_per_partner) * .25) < length(potential_customers),
                                floor(length(portfolio_per_partner) * .25), length(potential_customers))

        if(customer_count == 0) {
          next
        } else {

          # Select the new customers from their portfolio
          customers_from_partners[[j]] <- sample(potential_customers, customer_count, replace = FALSE)
        }
      }

      customers_from_partners_tibble <- data.frame(customer_id = unique(unlist(customers_from_partners)),
                                                   source      = "referral_partners",
                                                   stringsAsFactors = FALSE) %>%
                                        as_tibble()
    }
    ## Sales and Marketing ##

    #Identify potential customers; companies not in the set
    #of current customers with a seed round as their most recent funding.
    potential_customers <-
      funding_rounds[funding_cols] %>%
      filter(!(object_id %in% all_customers$customer_id |
                 object_id %in% customers_from_partners_tibble$customer_id)) %>%
      filter(funded_at < as.Date(current_quarter)) %>%
      arrange(-as.numeric(funded_at)) %>%
      group_by(object_id) %>%
      mutate(rank = row_number()) %>%
      ungroup() %>%
      filter(rank == 1 & funding_round_code == "seed")

    if(nrow(potential_customers) < n_customers) {stop("Not enough seed startups")}

    # Choose new customers and denote their source as `marketing`.
    customers_from_marketing <- data.frame(customer_id       = potential_customers[sample(1:nrow(potential_customers), n_customers), ]$object_id,
                                           source            = "marketing",
                                           stringsAsFactors  = FALSE) %>%
                                as_tibble()

    # Collate customers generated this quarter and
    # assign a random date for their start date as users.
    customers_this_quarter <-
      rbind(customers_from_partners_tibble,
            customers_from_marketing) %>%
      mutate(start_date = as.Date(runif(nrow(.),
                                        as.numeric(as.Date(current_quarter)),
                                        as.numeric(as.Date(current_quarter)) + (365/4) - 1)))

    #### Track investors ####

    ## Identify every interaction with an investor ##

    # Investors that get an account from...
    # one of their investments joining the software
    prior_stakeholder <-
      customers_this_quarter %>%
      left_join(investments[investment_cols],
                by = c("customer_id" = "funded_object_id")) %>%
      filter(funded_at < as.Date(quarter_seq[i+1])) %>%
      mutate(investor_source = "new_customer")

    # one of their new investments is already on the software
    new_stakeholder <-
      all_customers %>%
      left_join(investments[investment_cols],
                by = c("customer_id" = "funded_object_id")) %>%
      # Any new investments in current customers, this quarter
      filter(funded_at >= as.Date(current_quarter) &
               funded_at < as.Date(next_quarter)) %>%
      mutate(investor_source = "investment") %>%
      select(-stage)

    investments_in_customers <- rbind(prior_stakeholder, new_stakeholder) %>% as_tibble()

    # Note the investors who discover the software for the first time this quarter
    aware_this_quarter <-
      investments_in_customers %>%
      filter(!investor_object_id %in% all_aware_dates$investor_object_id) %>%
      group_by(investor_object_id) %>%
      summarize(first_aware = min(start_date)) %>%
      ungroup()

    # Note the investors who become partners this quarter.
    partner_this_quarter <-
      investments_in_customers %>%
      filter(!investor_object_id %in% all_partner_dates$investor_object_id) %>%
      left_join(assign_predispositions, by = "investor_object_id") %>%
      mutate(partner =  as.numeric(
        ifelse(group == "closed", sample(0:1, sum(group == "closed"), replace = TRUE, prob = c(conversion_rates[3], 1 - conversion_rates[3])),
                 ifelse(group == "open", sample(0:1, sum(group == "open"), replace = TRUE, prob = c(conversion_rates[1], 1 - conversion_rates[1])),
                          ifelse(group == "typical", sample(0:1, sum(group == "typical"), replace = TRUE, prob = c(conversion_rates[2], 1 - conversion_rates[2])),
                                   "-9999"))))) %>%
      filter(partner == 1) %>%
      group_by(investor_object_id) %>%
      summarize(first_partner = min(start_date)) %>%
      ungroup()

    #### Summarize #####
    ## Collect customers, investors aware of the product and referral partners ##

    # Add the most recent funding per customer
    all_customers <-
      select(all_customers, -stage) %>%
      rbind(customers_this_quarter) %>%
      left_join(funding_rounds[funding_cols], by = c("customer_id" = "object_id")) %>%
      filter(funded_at < as.Date(next_quarter)) %>%
      group_by(customer_id, source, start_date) %>%
      summarize(stage = sample(unique(funding_round_code[funded_at == max(funded_at)]), size = 1)) %>%
      ungroup()

    all_aware_dates <- rbind(all_aware_dates, aware_this_quarter)
    all_partner_dates <- rbind(all_partner_dates, partner_this_quarter)

    ## Report summary statistics on the customers, investors and partners ##
    sink("summary.txt", append = TRUE)
    print(current_quarter)
    # Customers
    print(paste0("New customers: ", nrow(customers_this_quarter)))
    print(paste0("From marketing: ", nrow(customers_this_quarter[customers_this_quarter$source == "marketing", ])))
    print(paste0("From partners: ", nrow(customers_this_quarter[customers_this_quarter$source == "partners", ])))
    # Newly aware investors
    print(paste0("Newly aware: ", nrow(aware_this_quarter)))
    # Referral partners
    print(paste0("Newly partner: ", nrow(partner_this_quarter)))
    # Summary
    print(paste0("Total customers: ", nrow(all_customers)))
    print(paste0("Total aware: ", nrow(all_aware_dates)))
    print(paste0("Total partners: ", nrow(all_partner_dates)))
    sink()
  }

  #### Collect output ####
  output <- vector(mode = "list", length = 3)
  output[[1]] <- all_customers
  output[[2]] <- all_aware_dates
  output[[3]] <- all_partner_dates
  output[[4]] <- simulation_start_date

  names(output) <- c("all_customers",
                     "all_aware_dates",
                     "all_partner_dates",
                     "simulation_start_date")
  output

}

simulation <- SimulateVirality(300)

# Transform and write records for the D3.js visualization
customers <-
  simulation$all_customers %>%
  mutate(week = as.numeric(start_date - simulation$simulation_start_date) %/% 7 + 1,
         quarter = (as.numeric(start_date - simulation$simulation_start_date) %/% 7 + 1) %/% 13 + 1) %>%
  group_by(week, quarter, stage, source) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(investor_type_frmted = ifelse(source == "marketing", "Marketing", paste("Partner:", str_to_title(str_replace(source, "_", " ")))))

stages <-
  distinct(investments[c("investor_object_id", "investor_name")]) %>%
  left_join(simulation$all_aware_dates, by = "investor_object_id") %>%
  left_join(simulation$all_partner_dates, by = "investor_object_id") %>%
  mutate(id = row_number()) %>%
  group_by(id, investor_name) %>%
  summarize(unaware = ifelse(is.na(first_aware), -99,
                             (as.numeric(first_aware - simulation$simulation_start_date) %/% 7 + 1) %/% 13 + 1),
            aware = ifelse(is.na(first_aware), NA,
                           ifelse(is.na(first_partner), -99,
                                  (as.numeric(first_partner - simulation$simulation_start_date) %/% 7 + 1) %/% 13 + 1)),
            partner = ifelse(is.na(first_partner), NA, -99)) %>%
  gather(key = "stage",
         value = "quarters",
         3:5) %>%
  ungroup() %>%
  na.omit(.) %>%
  mutate(group = "VCs") %>%
  select(id, group, stage, quarters, investor_name) %>%
  filter(id < 3500)

cumulative <-
  customers %>%
  group_by(week, investor_type_frmted) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  arrange(week) %>%
  group_by(investor_type_frmted) %>%
  mutate(cumsum = cumsum(n)) %>%
  ungroup() %>%
  mutate(date_start = simulation$simulation_start_date + (week - 1) * 7) %>%
  select(-n)

write.table(customers, "d3_visualization/customers.tsv", sep = '\t', row.names = FALSE)
write.table(stages, "d3_visualization/stages.tsv", sep = '\t', row.names = FALSE)
write.table(cumulative, "d3_visualization/cumulative.tsv", sep = '\t', row.names = FALSE) 
