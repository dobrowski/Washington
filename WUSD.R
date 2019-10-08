


### Load Libraries -----

library(tidyverse)
library(here)
library(readxl)
library(ggthemes)
library(janitor)
library(ggrepel)
library(scales)
library(htmlwidgets)
library(DT)
library(plotly)

###  Load data ----

loc <- "test"

slash <- "/"

all.data <- read_xls(here("data","mmdata","MM 18-19" , "toro tri1.xls")) %>%
    mutate(trimester = "1819-1")


all.data <- all.data %>%
    filter(trimester == "xxxx")

for( k in c("15-16","16-17","17-18","18-19", "19-20"))
for( j in c("toro","wus","sb"))
for( i in 1:3){
    tryCatch({
    
    
    temp <- read_xls(here("data","mmdata",paste0("MM ",k)  , paste0(j," tri",i,".xls"))) %>%
        mutate(trimester = paste0(k,"-",i)  )    #readline(prompt = "Enter year and trimester (e.g. 1819-1): "))
    
print(paste(k,j,i))
    
        
    all.data <- all.data %>%
        bind_rows(temp)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



all.data <- all.data %>%
    mutate_at(vars(ends_with("pl")), list( ~na_if(.,0)) ) %>% 
    mutate(teacher.last = sub( ",.*", "", teacher ),
           student = paste0(sub( ".*,", "", stuname )," ",  sub( ",.*", "", stuname ))) 

###  Student Over time ----

# Run for every student once a trimester.  Save all the graphs and put them up on shared drive.  
# Change to student names (First Name Last na,me)  by Grade and then by Teacher in folder structure
# 
# 
# stud  <- "4729"
# stud  <- "4864"
# stud <- "4800"
# #stud <- "1201"
# stud <- "3919"
# 
# graphthis <- all.data %>%
#     filter(stuid == stud) %>%
#     select(trimester, ends_with("pl")) %>%
#     gather(key = "key", value = "value", -trimester)
# 
# ggplot(data = graphthis, aes(x = trimester, y = value, group = key, color = key, label = value), size = 1) +
#     geom_line() + 
#     theme_hc()  +
#     geom_text_repel(data = graphthis %>% filter(trimester == max(trimester)) ,
#                      aes(label = key) , 
#                     hjust = "right", 
#                     segment.size = .2,
#                     segment.color = "grey",
#                     fontface = "bold", 
#                     size = 3, 
#                     nudge_x = .5, 
#                     direction = "y") +
#     geom_label(aes(label = value), 
#                size = 2.5, 
#                label.padding = unit(0.05, "lines"), 
#                label.size = 0.0) +
#     theme(legend.position = "none") +
#     ylim(0,4) + 
#     labs(title = paste0("Performance Levels over time for Student ", stud),
#          x = "Trimester",
#          y= "Performance Level")


#  Redo for all students 

current <- all.data %>%
    filter(trimester == max(trimester)) %>%
    select(stuid, student, cougrade, teacher.last) 
    
graph.history <- function(stud){
    
all.data %>%
    filter(stuid == stud) %>%
    select(trimester, ends_with("pl")) %>%
    gather(key = "key", value = "value", -trimester) %>%
        ggplot( aes(x = trimester, y = value, group = key, color = key, label = value), size = 1) +
    geom_line() + 
    theme_hc()  +
    geom_text_repel(data = graphthis %>% filter(trimester == max(trimester)) ,
                    aes(label = key) , 
                    hjust = "right", 
                    segment.size = .2,
                    segment.color = "grey",
                    fontface = "bold", 
                    size = 3, 
                    nudge_x = .5, 
                    direction = "y") +
    geom_label(aes(label = value), 
               size = 2.5, 
               label.padding = unit(0.05, "lines"), 
               label.size = 0.0) +
    theme(legend.position = "none") +
    ylim(0,4) + 
    labs(title = paste0("Performance Levels over time for Student ", stud),
         x = "Trimester",
         y= "Performance Level")



} 

# graph.history("3919")


current.nest <- current %>%
    mutate(student.id = stuid) %>%
    group_by(student) %>%
    nest() %>%
    mutate(graphthese = data %>%
               map(~ all.data %>%
                       filter(stuid == .x$stuid) %>%
                       select(trimester, ends_with("pl")) %>%
                       gather(key = "key", value = "value", -trimester)),
            real.graph = graphthese %>%
               map2(student,~ ggplot(data = .x, aes(x = trimester, y = value, group = key, color = key, label = value), size = 1) +
                         geom_line() + 
                         theme_hc()  +
                         geom_text_repel(data = .x %>% filter(trimester == max(trimester)) ,
                                         aes(label = key) , 
                                         hjust = "right", 
                                         segment.size = .2,
                                         segment.color = "grey",
                                         fontface = "bold", 
                                         size = 3, 
                                         nudge_x = .5, 
                                         direction = "y") +
                         geom_label(aes(label = value), 
                                    size = 2.5, 
                                    label.padding = unit(0.05, "lines"), 
                                    label.size = 0.0) +
                         theme(legend.position = "none") +
                         ylim(0,4) + 
                         labs(title = paste0("Performance Levels over time for ", .y),
                              x = "Trimester",
                              y= "Performance Level")
               )
    ) %>%
    unnest( cols = c(data)) %>%
    mutate(stufile = paste0( cougrade, slash,  teacher.last, slash,    student, ".png"))  # to create path and filename




dir.create(here(loc))
# Makes the grade level folders
walk(current.nest$cougrade, ~ dir.create(here(loc, .x)))
# Makes the teacher folders
walk2(current.nest$cougrade, current.nest$teacher.last, ~ dir.create(here(loc, .x, .y)))
#  Saves the graphs in the current teachers folder
walk2(current.nest$stufile, current.nest$real.graph , ~ggsave(filename = here(loc, .x), plot = .y, height = 7, width = 7) ) 

 

###  Big change between Trimester -----

#  This shows the student ID of all students that moved more than 1 point on an PL in the last trimester

# # Need to show which direction + / - 
# test <- all.data %>% 
#     filter(trimester %in% tail(unique(all.data$trimester), 2)) %>%
#     group_by(stuid) %>%
#     mutate(la.range = range(lapl)[2] - range(lapl)[1],
#            math.range = range(mathpl)[2] - range(mathpl)[1],
#            sci.range = range(scipl)[2] - range(scipl)[1],
#            ss.range = range(sspl)[2] - range(sspl)[1]) %>% 
#     select(stuid, stuname, teacher, ends_with("range")) %>%
#     gather(-stuid, -stuname, -teacher ,key = "PL", value = "value") %>%
#     filter(value >= 1) %>%
#     distinct() %>%
#     arrange(teacher, stuname)




bigchange <- all.data %>% 
    filter(trimester %in% tail(unique(all.data$trimester), 2)) %>%
    group_by(student, teacher.last) %>%
    mutate(tri = case_when( 
        trimester == tail(unique(all.data$trimester), 2)[1] ~ "old" ,
        trimester == tail(unique(all.data$trimester), 2)[2] ~ "new" )
        ) %>%
    select(student, teacher.last, cougrade, tri, ends_with("pl")) %>%
    gather(key = "key", value = "value", -tri, -cougrade, -teacher.last,  -student ) %>%
        pivot_wider(names_from = c(tri,key), values_from = value) %>%
    mutate(change_lapl = old_lapl - new_lapl,
           change_mathpl = old_mathpl - new_mathpl,
           change_scipl = old_scipl - new_scipl,
           change_sspl = old_sspl - new_sspl) %>%
    select(cougrade,  teacher.last,  student, starts_with("change")) %>%
    pivot_longer(cols = starts_with("change"), names_to = "pl", values_to = "change") %>% 
    filter(change >=1 | change <= -1) %>%
    arrange(cougrade, teacher.last, desc(change), student)

bigchange.nest <- bigchange %>% 
    group_by(cougrade) %>%
    nest() %>% 
    mutate(dt = data %>%
               map(~ datatable(.x, options = list(pageLength = 20)))) %>%
    mutate(filename = paste0( cougrade, slash,  "1 Big Changes for " , cougrade, ".html"))




dir.create(here(loc))
# Makes the grade level folders
walk(bigchange.nest$cougrade, ~ dir.create(here(loc, .x)))
#  Saves the graphs in the current teachers folder
walk2(bigchange.nest$filename, bigchange.nest$dt , ~saveWidget(widget = .y, file = here(loc, .x), selfcontained = TRUE ) ) 


# 
# dt <- datatable(bigchange, options = list(pageLength = 20))
# 
# saveWidget(widget = dt, file = "df.html")
# 


### Top students by grade ----



current.nest <- all.data %>%
    filter(trimester == max(trimester) ) %>%
    select(cougrade, teacher.last, student, ends_with("pl")) %>%
    group_by(cougrade, teacher.last) %>%
    nest() %>%
    mutate(dt = data %>%
               map(~ datatable(.x, options = list(pageLength = 30)))) %>%
    mutate(filename = paste0( cougrade, slash, teacher.last, slash,  "0 Latest Trimester for " , teacher.last, ".html"))

    #    mutate(htmlfile = paste0( cougrade, "/",  teacher.last, "/", "Latest trimester student PLs for ", teacher.last, ".html"))  # to create path and filename
 #   mutate(htmlfile = paste0( cougrade, " ",  teacher.last, " ", "Latest trimester student PLs for ", teacher.last, ".html"))  # to create path and filename





dir.create(here(loc))

# Makes the grade level folders
walk(current.nest$cougrade, ~ dir.create(here(loc, .x)))
# Makes the teacher folders
walk2(current.nest$cougrade, current.nest$teacher.last, ~ dir.create(here(loc, .x, .y)))
#  Saves the tables in the current teachers folder
walk2(current.nest$filename, current.nest$dt,   ~saveWidget(widget = .y, file = here(loc, .x), selfcontained = TRUE ) )  




# top <- all.data %>%
#     filter(trimester == max(trimester) ) %>%
#     split(.$cougrade) %>%
#    map(~top_frac(.1, wt = mathpl) )
# 
# 
# top <- all.data %>%
#     filter(trimester == max(trimester) ) %>%
#     select(cougrade, teacher.last, student, ends_with("pl")) %>%
#     split(.$cougrade) %>%
#     map_df(~top_n(x = .x, n = round(nrow(.x) * 0.1), wt = mathpl) )
# 
# 
# top.function <- function(amount, variable){
#     all.data %>%
#         filter(trimester == max(trimester) ) %>%
#         select(cougrade, teacher.last, student, ends_with("pl")) %>%
#         split(.$cougrade) %>%
#         map_df(~top_n(x = .x, n = round(nrow(.x) * amount), wt = (!!sym(variable))) )
#     
# }
# 
# 
# top.function(.1, "lapl") %>% datatable( options = list(pageLength = 20))
# top.function(.1, "mathpl")
# top.function(.1, "scipl")
# top.function(.1, "sspl")
# 
# top.function(-.1, "lapl")
# top.function(-.1, "mathpl")
# top.function(-.1, "scipl")
# top.function(-.1, "sspl")
# 
# 
# 
# 
# top <- all.data %>%
#     filter(trimester == max(trimester) ) %>%
#     select(cougrade, teacher.last, student, ends_with("pl")) %>%
#     split(.$cougrade) %>%
#     map_df(~top_n(x = .x, n = round(nrow(.x) * 0.1), wt = mathpl) )







# 
# top <- all.data %>%
#     filter(trimester == max(trimester),
#            cougrade == "08") %>%
#     top_frac(-.1,"mathpl") %>%
#     mutate(subject = "mathpl") %>%
#     filter(cougrade == "9999")
# 
# for(i in c("mathpl", "lapl"))
# for(g in unique(all.data$cougrade)){
# 
# hold <- all.data %>%
#     filter(trimester == max(trimester),
#            cougrade == g) %>%
#     top_frac(.1,{{i}}) %>%
#     mutate(subject = i)
# 
# top <- bind_rows(top, hold) %>%
#     distinct()
# 
# }
# 



### Distibution by grade ----

#hoverover to see who in in that bar 
# 
# p <- all.data %>%
#     filter(trimester == max(trimester),
#            cougrade == "08") %>%
#     ggplot() +
#     geom_histogram(aes( x = mathpl, text = student)) +
#     scale_y_continuous(breaks= pretty_breaks()) +
#     theme_hc()
# 
# ggplotly(p, tooltip = "text" )
# 
# saveWidget(widget = ggplotly(p), file = "test.html")

# 

### Distibution by teacher ----

# #  For a single teacher
# 
# teach <- "Frazier"
# 
# all.data %>% 
#     filter(trimester == max(trimester),
#            str_detect(teacher, teach)) %>%
#     ggplot() + 
#     geom_histogram(aes( x = mathpl)) +
#     scale_y_continuous(breaks= pretty_breaks()) +
#     theme_hc()



#  For distribution for all teachers.  

current <- all.data %>%
    filter(trimester == max(trimester)) %>%
    select(stuid, student, cougrade, teacher.last, ends_with("pl")) 


current.nest <- current %>%
    mutate(student.id = stuid) %>%
    group_by(cougrade, teacher.last) %>%
    nest() %>%
    mutate(lapl.graph = data %>%
               map2(teacher.last ,~ ggplot(.x) + 
                       geom_histogram(aes( x = .x$lapl, text = student)) +
                       scale_y_continuous(breaks= pretty_breaks()) +
                       theme_hc() +
                       labs(x = "Language Arts PL Score",
                            y = "Number of students",
                            title = paste0("Students by Language Arts PL Score for ", .y )) ),    
           mathpl.graph = data %>%
               map2(teacher.last ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$mathpl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Math PL Score",
                             y = "Number of students",
                             title = paste0("Students by Math PL Score for ", .y )) ),  
           scipl.graph = data %>%
               map2(teacher.last ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$scipl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Science PL Score",
                             y = "Number of students",
                             title = paste0("Students by Science PL Score for ", .y )) ),  
           sspl.graph = data %>%
               map2(teacher.last ,~ ggplot(.x) + 
                        geom_histogram(aes( x = .x$sspl, text = student)) +
                        scale_y_continuous(breaks= pretty_breaks()) +
                        theme_hc() +
                        labs(x = "Social Studies PL Score",
                             y = "Number of students",
                             title = paste0("Students by Social Studies PL Score for ", .y )) ),  
           ) %>%
    mutate(lafile = paste0( cougrade, slash,  teacher.last, slash, "1 LA Distribution for " , teacher.last, ".png"),
           mathfile = paste0( cougrade, slash,  teacher.last, slash, "2 Math Distribution for " , teacher.last, ".png"),
           scifile = paste0( cougrade, slash,  teacher.last, slash, "3 Science Distribution for " , teacher.last, ".png"),
           ssfile = paste0( cougrade, slash,  teacher.last, slash, "4 Social Studies Distribution for " , teacher.last, ".png")) %>%
   mutate(lafileh = paste0( cougrade, slash,  teacher.last, slash, "1 LA Distribution for " , teacher.last, ".html"),
          mathfileh = paste0( cougrade, slash,  teacher.last, slash, "2 Math Distribution for " , teacher.last, ".html"),
          scifileh = paste0( cougrade, slash,  teacher.last, slash, "3 Science Distribution for " , teacher.last, ".html"),
          ssfileh = paste0( cougrade, slash,  teacher.last, slash, "4 Social Studies Distribution for " , teacher.last, ".html"))  # to create path and filename



dir.create(here(loc))
# Makes the grade level folders
walk(current.nest$cougrade, ~ dir.create(here(loc, .x)))
# Makes the teacher folders
walk2(current.nest$cougrade, current.nest$teacher.last, ~ dir.create(here(loc, .x, .y)))
#  Saves the graphs in the current teachers folder
walk2(current.nest$lafileh, current.nest$lapl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 
walk2(current.nest$mathfileh, current.nest$mathpl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 
walk2(current.nest$scifileh, current.nest$scipl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 
walk2(current.nest$ssfileh, current.nest$sspl.graph , ~saveWidget(widget = ggplotly(.y, tooltip = "text"), file = here(loc, .x) ) ) 

walk2(current.nest$lafile, current.nest$lapl.graph , ~ggsave(filename = here(loc, .x), plot = .y, height = 5, width = 8) ) 
walk2(current.nest$mathfile, current.nest$mathpl.graph , ~ggsave(filename = here(loc, .x), plot = .y, height = 5, width = 8) ) 
walk2(current.nest$scifile, current.nest$scipl.graph , ~ggsave(filename = here(loc, .x), plot = .y, height = 5, width = 8) ) 
walk2(current.nest$ssfile, current.nest$sspl.graph , ~ggsave(filename = here(loc, .x), plot = .y, height = 5, width = 8) ) 



#### End -----
