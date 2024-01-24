```{r}
mother_jones <- mother_jones %>%
  mutate(age_of_shooter = as.numeric(age_of_shooter))
mother_jones %>%
  mutate(age_of_shooter = as.numeric(age_of_shooter)) %>%
  ggplot(mapping = aes(x = age_of_shooter)) +
  geom_histogram() + 
  labs(x = "Age of Shooter", y = "Frequency", title = "Distribution of Professor Ratings")

mother_jones %>%
  mutate(age_of_shooter = as.numeric(age_of_shooter)) %>%
  summarize(Max = max(age_of_shooter, na.rm = TRUE), 
            Min = min(age_of_shooter, na.rm = TRUE),
            Medium = median(age_of_shooter, na.rm = TRUE),
            SD = sd(age_of_shooter, na.rm = TRUE),
            Mean = mean(age_of_shooter, na.rm = TRUE))
```

The most surprising summary stat is that the youngest shooter is 11 years old. 

Describing the relationship between age of shooter and year
```{r}
mother_jones %>%
  mutate(age_of_shooter = as.numeric(age_of_shooter)) %>%
  ggplot(mapping = aes(x = year, y = age_of_shooter)) +
  geom_jitter() + 
  labs(x = "Year", y = "Age of Shooter", title = "Age of Shooters Throughout the Years")

m_age_of_shooter = lm(year ~ age_of_shooter, data = mother_jones)
m_age_of_shooter
```

Top five states and age of shooter analysis
```{r}
mother_jones_top_five <- mother_jones %>%
  filter(state == "California" | state == "Texas" | state == "Florida" | state ==  "Colorado" | state == "Washington") 

mother_jones %>%
  count(state)  %>%
  arrange(desc(n))
mother_jones_top_five %>%
  mutate(age_of_shooter = as.numeric(age_of_shooter)) %>%
  filter(total_victims < 600) %>%
  ggplot(mother_jones_top_three, mapping = aes(x = state, y = age_of_shooter)) +
  geom_boxplot() 

age_mod <- lm(age_of_shooter ~ state, data = mother_jones_top_five)
age_mod
tidy(age_mod)
mother_jones_top_five %>%
  count(state)
```

Because distribution varies between states, there is an association between a state and age of shooter. 

$\widehat{age_of_shooter} = 37.591 - 6.448stateColorado - 0.227stateFlorida - 7.174stateTexas - 10.877stateWashington$
  
  Slope: The average age of shooter in Colorado is about 6.448 years less than the average age of shooter in California. The average age of shooter in Florida is about .227 years less than the average age of shooter in California. The average age of shooter in Texas is about 7.174 years less than the average age of shooter in California. The average age of shooter in Washington is about 10.877 years less than the average age of shooter in California. 

Intercept: California's age of shooter is expected on average to be 37.59 years old.

From this model, I concluded that Florida's average age of shooter is not much different from California's average age of shooter.

```{r}
mother_jones_top_five$weapon_type_update <- gsub('[0-9]+', '', mother_jones_top_five$weapon_type_update)
mother_jones_top_five <- mother_jones_top_five %>%
    mutate(weapon_type_update = case_when(
      weapon_type_update == " semiautomatic handguns" ~ "Semiautomatic handgun",
      weapon_type_update == " handguns" ~ "Handgun",
      TRUE                                                              ~ weapon_type_update
    )) #%>%
  #filter(weapon_type_update == "Semiautomatic handgun" | weapon_type_update == "Semiautomatic rifle" | weapon_type_update == "Handgun")

mother_jones_top_five %>%
  count(weapon_type_update) %>%
  arrange(desc(n)) 

mother_jones_top_five %>%
    mutate(age_of_shooter = as.numeric(age_of_shooter)) %>%
ggplot(mother_jones_top_five, mapping = aes(x = weapon_type_update, y = age_of_shooter, )) +
    geom_boxplot() 
```

```{r}
mother_jones_top_five %>%
  count(year) %>%
  ggplot(aes(x = year, y = n, group = 1)) +
  geom_line() +
  labs(x = "Year", y = "Frequency", title = "Distribution of Mass Shooting in the Top Five States Throughout the Years")
```

Between 2014 to 2017, there is an incredible increase in mass shootings in those five states. Is it because people are starting to have more access to weapons? 


```{r}
mother_jones_top_five %>%
  ggplot(aes(x = weapons_obtained_legally)) +
  geom_bar() +
  labs(x = "Weapons Obtained Legally", y = "Frequency")
```


```{r us_map, echo = FALSE}
ggplot(mother_jones, mapping = aes(x = longitude, y = latitude)) +
  geom_point() +
  labs(x = "Longitude",
        y = "Latitude",
       title = "Visualization of Mass Shootings Locations accross the U.S.")
```

The reason that the longitude and latitude plot worked well is because most of the cases were around the border. 



















Top five states and age of shooter analysis
```{r}
mother_jones_top_five <- mother_jones %>%
  filter(state == "California" | state == "Texas" | state == "Florida" | state ==  "Colorado" | state == "Washington") 

mother_jones %>%
  count(state)  %>%
  arrange(desc(n))
mother_jones_top_five %>%
  mutate(age_of_shooter = as.numeric(age_of_shooter)) %>%
  filter(total_victims < 600) %>%
  ggplot(mother_jones_top_three, mapping = aes(x = state, y = age_of_shooter)) +
    geom_boxplot() 

age_mod <- lm(age_of_shooter ~ state, data = mother_jones_top_five)
age_mod
tidy(age_mod)
mother_jones_top_five %>%
  count(state)
```

Because distribution varies between states, there is an association between a state and age of shooter. 

$\widehat{age_of_shooter} = 37.591 - 6.448stateColorado - 0.227stateFlorida - 7.174stateTexas - 10.877stateWashington$

Slope: The average age of shooter in Colorado is about 6.448 years less than the average age of shooter in California. The average age of shooter in Florida is about .227 years less than the average age of shooter in California. The average age of shooter in Texas is about 7.174 years less than the average age of shooter in California. The average age of shooter in Washington is about 10.877 years less than the average age of shooter in California. 

Intercept: California's age of shooter is expected on average to be 37.59 years old.

From this model, I concluded that Florida's average age of shooter is not much different from California's average age of shooter.

```{r}
mother_jones_top_five$weapon_type_update <- gsub('[0-9]+', '', mother_jones_top_five$weapon_type_update)
mother_jones_top_five <- mother_jones_top_five %>%
  mutate(weapon_type_update = case_when(
    weapon_type_update == " semiautomatic handguns" ~ "Semiautomatic handgun",
    weapon_type_update == " handguns" ~ "Handgun",
    TRUE                                                              ~ weapon_type_update
  )) #%>%
#filter(weapon_type_update == "Semiautomatic handgun" | weapon_type_update == "Semiautomatic rifle" | weapon_type_update == "Handgun")

mother_jones_top_five %>%
  count(weapon_type_update) %>%
  arrange(desc(n)) 

mother_jones_top_five %>%
  mutate(age_of_shooter = as.numeric(age_of_shooter)) %>%
  ggplot(mother_jones_top_five, mapping = aes(x = weapon_type_update, y = age_of_shooter, )) +
  geom_boxplot() 
```

```{r}
mother_jones_top_five %>%
  count(year) %>%
  ggplot(aes(x = year, y = n, group = 1)) +
  geom_line() +
  labs(x = "Year", y = "Frequency", title = "Distribution of Mass Shooting in the Top Five States Throughout the Years")
```

Between 2014 to 2017, there is an incredible increase in mass shootings in those five states. Is it because people are starting to have more access to weapons? 
  
  
  ```{r}
mother_jones_top_five %>%
  ggplot(aes(x = weapons_obtained_legally)) +
  geom_bar() +
  labs(x = "Weapons Obtained Legally", y = "Frequency")
```


```{r us_map, echo = FALSE}
ggplot(mother_jones, mapping = aes(x = longitude, y = latitude)) +
  geom_point() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Visualization of Mass Shootings Locations accross the U.S.")
```

The reason that the longitude and latitude plot worked well is because most of the cases were around the border. 



















