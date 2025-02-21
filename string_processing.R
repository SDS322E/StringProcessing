## String processing

library(tidyverse)

competitions <- read_tsv("WCA_export_Competitions.tsv.bz2")

## Look at all data
competitions |> 
    glimpse()

## In what US states are competitions held?
competitions |> 
    filter(countryId == "USA") |> 
    select(starts_with("venue")) 

## Restrict columns
competitions |> 
    filter(countryId == "USA") |> 
    select(matches("venue[a-zA-Z]+")) 

## Take a random sample of competitions
competitions |> 
    filter(countryId == "USA") |> 
    select(venueAddress) |> 
    sample_n(20)

## Find competitions in Austin, TX
competitions |> 
    filter(countryId == "USA") |> 
    filter(str_detect(venueAddress, "Austin")) |> 
    select(name, venueAddress)
    
## Find competitions in Austin, TX
competitions |> 
    filter(countryId == "USA") |> 
    filter(str_detect(venueAddress, 
                      "Austin, TX|Austin, Texas|Austin TX|Austin Texas")) |> 
    select(venueAddress, year)

## Select some random rows to get the pattern
competitions |> 
    filter(countryId == "USA") |> 
    select(venueAddress, venueDetails) |> 
    sample_n(20)

## Remove the ZIP code
competitions |> 
    filter(countryId == "USA") |> 
    select(venueAddress) |> 
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |> 
    select(state) |> 
    sample_n(20)


## Remove trailing USAs
competitions |> 
    filter(countryId == "USA") |> 
    select(venueAddress) |> 
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |> 
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |> 
    select(state) |> 
    sample_n(20)

## Remove the street address
competitions |> 
    filter(countryId == "USA") |> 
    select(venueAddress) |> 
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |> 
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |> 
    mutate(state = str_replace(state, ".*, ", "")) |> 
    select(state) 

## Clean up
competitions |> 
    filter(countryId == "USA") |> 
    select(venueAddress) |> 
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |> 
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |> 
    mutate(state = str_replace(state, ".*, ", "")) |> 
    mutate(state = str_replace(state, ",$", "")) |> 
    select(state) |> 
    sample_n(20)

## Count the number of competitions in each state
competitions |> 
    filter(countryId == "USA") |> 
    select(venueAddress) |> 
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |> 
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |> 
    mutate(state = str_replace(state, ".*, ", "")) |> 
    mutate(state = str_replace(state, ",$", "")) |> 
    filter(state %in% state.name 
           | state %in% state.abb) |> 
    select(state) |> 
    group_by(state) |> 
    summarize(n = n()) |> 
    arrange(state)

## Bar plot of number of competitions in each state
competitions |> 
    filter(countryId == "USA") |> 
    select(venueAddress) |> 
    mutate(state = str_replace(venueAddress, " +[0-9]+$", "")) |> 
    mutate(state = str_replace(state, "USA$|[0-9]+.*USA$", "")) |> 
    mutate(state = str_replace(state, ".*, ", "")) |> 
    mutate(state = str_replace(state, ",$", "")) |> 
    filter(state %in% state.name 
           | state %in% state.abb) |> 
    ggplot(aes(y = state)) + 
    geom_bar()




