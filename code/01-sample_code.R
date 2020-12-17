library(tidyverse)
library(ggimage)
library(jpeg)
library(rebus)
library(d3treeR)

img <- readJPEG(here::here("images", "sweater.jpeg"))


# potentially useful function: read_csv
sweaters <- read_csv(here::here("data", "use_this_data", "holiday_sweaters-2020-12-15-clean.csv"))

animals <- c("octopus", "t-rex", "sheep", "sloth", "invert", "santaur", "shark", "crocodile",
             "reindeer", "dolphin", "pug", "grinch", "snowmen", "snowman", "cat", "snoopy", "polar bear", 
             "giraffe", "llama", "satan")



sweaters2 <- sweaters %>% 
  filter(hs_tf == "Yes") %>% 
  mutate(image_desc = str_to_lower(image_desc),
         creature = str_extract_all(image_desc, pattern = or1(animals))) %>%
  select(sweater, creature) %>%
  unnest()

length(unique(sweaters2$sweater))

sweaters3 <- sweaters2 %>%
  group_by(creature) %>%
  summarise(n = n()) %>%
  ungroup()

p <- treemap(sweaters3,
             index=c("creature"),
             vSize="n",
             type="index",
             palette = "Set2",
             bg.labels=c("white"),
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")
             )  
)  

# Compute percentages
sweaters3$fraction = sweaters3$n / sum(sweaters3$n)

# Compute the cumulative percentages (top of each rectangle)
sweaters3$ymax = cumsum(sweaters3$fraction)

# Compute the bottom of each rectangle
sweaters3$ymin = c(0, head(sweaters3$ymax, n=-1))

library("scales")

show_col(excel_pal()(21))

ggplot(sweaters3, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=creature)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) 

ggplot(sweaters3, aes(x = colors, y = sum)) +
  geom_col(aes(color = colors, fill = colors)) +
  scale_fill_manual(values = c("mediumorchid1", "lightyellow3", "salmon3", "salmon2", "slateblue4", "yellowgreen", "lawngreen", "tan", "ivory", "khaki2", "chocolate", "blue1", "darkkhaki", "forestgreen", "firebrick3", "goldenrod1", "grey0", "lightblue1", "honeydew2", "olivedrab", "ghostwhite")) +
  geom_errorbar(aes(ymin = sum - 2, ymax = sum + 3)) +
  annotate("text", x = 14, y = 51, label = "This is the \n highest bar!", size = 2) +
  theme_dark() +
  labs(x = "X-axis",
       y = "Y-axis",
       title = "The most beautiful plot ever")

ggsave(here::here("figures", "sample-plot.jpg"), width = 7, height = 4, dpi = 150)


