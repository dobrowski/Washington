

classsize <- all.data %>%
    group_by(cougrade,teacher.last, trimester) %>%
    summarize(mean = mean(lapl, na.rm = TRUE),
              number = n())

# 
# ggplot(classsize) +
#     geom_violin(aes(color=number, y = mean, x = cougrade))
# 
#                 
# 
# ggplot(classsize) +
#     geom_violin(aes(x=number, y = mean, color = cougrade))
# 

all.data %>%
    group_by(cougrade,teacher.last, trimester) %>%
    summarize(mean = mean(lapl, na.rm = TRUE),
              number = n()) %>% 
    filter(cougrade %in% c("KN", "01", "02", "03" )) %>%
    ggplot(aes(x = number, y = mean)) + 
    geom_point() + 
    geom_smooth(method='lm') +
    theme_hc() +
    labs(title = "There is no significant change in average LA PL with larger class sizes at Toro Park.",
         x = "Number of Students",
         y = "Mean LA PL") + 
    scale_y_continuous(limits = c(1.5,4))

ggsave(here::here("figs","Class size compared to Mean LAPL.png"))



all.data %>%
    group_by(cougrade,teacher.last, trimester) %>%
    summarize(mean = mean(mathpl, na.rm = TRUE),
              number = n()) %>% 
    filter(cougrade %in% c("KN", "01", "02", "03" )) %>%
    ggplot(aes(x = number, y = mean)) + 
    geom_point() + 
    geom_smooth(method='lm') +
    theme_hc() +
    labs(title = "There is a slight decrease in average Math PL with larger class sizes at Toro Park.",
         x = "Number of Students",
         y = "Mean Math PL") + 
    scale_y_continuous(limits = c(1.5,4))

ggsave(here::here("figs","Class size compared to Mean MathPL.png"))


lm(data = classsize, number ~ mean) -> model

summary(model)
