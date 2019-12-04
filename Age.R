

library(scales)
library(lubridate)

mon_ord <- c("September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July", "August")


birthdates <- read_xls(here("data","aa000492.xls")) %>% 
    mutate(month = months(studob) )


birthmm <- current %>%
    left_join(birthdates) %>%
    filter(!is.na(month)) 

birthnew <- birthmm %>%
    mutate(fakeyear = if_else(months(studob) %in% c("September", "October", "November", "December"),
                              2012,
                              2013),
           studob.bak = studob) 

year(birthnew$studob) <- birthnew$fakeyear



birth.mean <- birthmm  %>%
    mutate( month = factor(month, levels = mon_ord)  ) %>%
    group_by(month) %>%
    summarize(mean = mean(lapl, na.rm = TRUE),
              number = n())





birthnew %>% 
    ggplot(aes(x = studob, y = lapl)) +
    geom_point(height = 0.05) + 
    geom_smooth(method = lm) +
    scale_x_datetime(labels = date_format("%b"), date_breaks = "1 month" ) +
    theme_hc() +
    labs(title = "There is no significant change in average LA PL for younger students at Toro Park.",
         x = "Student Birth Month and Day",
         y = "Mean LA PL")

ggsave(here::here("figs","Age compared to Mean LAPL.png"))


birthnew %>% 
    ggplot(aes(x = studob, y = mathpl)) +
    geom_point(height = 0.05) + 
    geom_smooth(method = lm) +
    scale_x_datetime(labels = date_format("%b"), date_breaks = "1 month" ) +
    theme_hc() +
    labs(title = "There is no significant change in average Math PL for younger students at Toro Park.",
         x = "Student Birth Month and Day",
         y = "Mean Math PL")

ggsave(here::here("figs","Age compared to Mean MathPL.png"))

lm(data = birthnew, lapl ~ studob) -> model
lm(data = birthnew, mathpl ~ studob) -> model

summary(model)



birth.mean %>% 
       ggplot(aes(x = month, y = mean)) + 
#    ggplot(aes(x = studob, y = lapl)) +
    geom_point(height = 0.05) + 
    geom_smooth(method = lm) +
#    scale_x_datetime(labels = date_format("%b") ) +
    theme_hc() +
    labs(title = "There is no significant change in average LA PL with larger class sizes at Toro Park.",
         x = "Number of Students",
         y = "Mean LA PL")
