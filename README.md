#data cleaning and manipulation practice from DataCamp
install.packages(tidyverse)
library(tidyverse)

#PART 1
netflix_df %>% 
  # Split the duration column into value and unit columns
  separate(duration, into = c("value", "unit"), sep = " ", convert = TRUE)

#PART 2
  phone_nr_df %>%
  # Unite the country_code and national_number columns
  unite("international_number", country_code, national_number, sep = " ")

  #PART 3
  tvshow_df %>% 
  separate_rows (cast, sep = ", ") %>% 
  rename(actor = cast) %>% 
  count(actor, sort = TRUE) %>% 
  head()

  #PART 4
  drink_df %>% 
  # Separate the ingredients over rows
  separate_rows(ingredients, sep = "; ") %>% 
  # Separate ingredients into three columns
  separate(
    ingredients, 
    into = c("ingredient", "quantity", "unit"), 
    sep = " ", 
    convert = TRUE
  ) %>% 
  # Group by ingredient and unit
  group_by(ingredient, unit) %>% 
  # Calculate the total quantity of each ingredient
  summarize(quantity = sum(quantity))

  #PART 5
  director_df %>% 
  # Drop rows with NA values in the director column
  drop_na(director) %>% 
  # Spread the director column over separate rows
  separate_rows(director, sep = ", ") %>% 
  # Count the number of movies per director
  count(director, sort = TRUE)

  #PART 6
  sales_df %>% 
  # Impute the year column
  fill(year, .direction = "up") %>%
  # Create a line plot with sales per quarter colored by year.
  ggplot(aes(x = quarter, y = sales, color = year, group = year)) +
  geom_line()

#PART 7
country_to_continent_df %>% 
  left_join(nuke_df, by = "country_code") %>%  
  # Impute the missing values in the n_bombs column with 0L
  replace_na(list(n_bombs = 0L)) %>% 
  # Group the dataset by continent
  group_by(continent) %>% 
  # Sum the number of bombs per continent
  summarize(n_bombs_continent = sum(n_bombs)) %>% 
  # Plot the number of bombs per continent
  ggplot(aes(x = continent, y = n_bombs_continent)) +
  geom_col()

  #PART 7
  nuke_df %>% 
  # Pivot the data to a longer format
  pivot_longer(
    -year, 
    # Overwrite the names of the two new columns
    names_to = "country", 
    values_to = "n_bombs"
  ) %>% 
  # Replace NA values for n_bombs with 0L
  replace_na(list(n_bombs = 0L)) %>% 
  # Plot the number of bombs per country over time
  ggplot(aes(year, n_bombs, color = country)) +
  geom_line()

  #PART 8
  obesity_df %>% 
  # Pivot the male and female columns
  pivot_longer(c(male, female),
               names_to = "sex",
               values_to = "pct_obese") %>% 
  # Create a scatter plot with pct_obese per country colored by sex
  ggplot(aes(x = pct_obese, color = sex,
             y = forcats::fct_reorder(country, both_sexes))) +
  geom_point() +
  scale_y_discrete(breaks = c("India", "Nauru", "Cuba", "Brazil",
                              "Pakistan", "Gabon", "Italy", "Oman",
                              "China", "United States of America")) +
  labs(x = "% Obese", y = "Country")

#PART 9
bond_df %>% 
  # Pivot the data to long format
  pivot_longer(
    -Bond, 
    # Overwrite the names of the two newly created columns
    names_to = "decade", 
    values_to = "n_movies", 
    # Drop na values
    values_drop_na = TRUE, 
    # Transform the decade column data type to integer
    names_transform = list(decade = as.integer)
  ) %>% 
  ggplot(aes(x = decade + 5, y = n_movies, fill = Bond))+
  geom_col()

  #PART 10
  bird_df %>%
  # Pivot the data to create a 2 column data frame
  pivot_longer(
    starts_with("points_"),
    names_to = "points",
    names_prefix = "points_",
    names_transform = list(points = as.integer),
    values_to = "species",
    values_drop_na = TRUE
  ) %>%
  group_by(species) %>% 
  summarize(total_points = sum(points)) %>% 
  slice_max(total_points, n = 5)

  #PART 11
  stock_df %>% 
  # Pivot the data to create 3 new columns: year, week, price
  pivot_longer(
    -company,
    names_to = c("year", "week"),
    values_to = "price",
    names_sep = "_week",
    names_transform = list(
      year = as.integer,
      week = as.integer)
  ) %>%
  # Create a line plot with price per week, color by company
  ggplot(aes(x = week, y = price, color = company)) +
  geom_line() +
  facet_grid(. ~ year)
  
  
