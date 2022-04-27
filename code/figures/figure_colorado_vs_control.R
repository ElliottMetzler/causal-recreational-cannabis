df <- df <- read_csv(here("data", "clean", "clean_2000.csv"),
                     show_col_types = F) %>% as.data.frame
df_plot <- df %>% filter(ever_legalized != 1 | state == "Colorado") %>% 
  group_by(year,ever_legalized) %>% 
  summarise(cdr = mean(crude_death_rate))

plot <- ggplot(data = df_plot, aes(x = year, y = cdr, group = as.factor(ever_legalized), linetype = as.factor(ever_legalized)))+
  geom_line()+
  geom_vline(xintercept =  2012, linetype = "dashed")+
  scale_x_continuous()+
  scale_linetype_discrete(name = "", labels = c("Control States", "Colorado"))+
    theme_minimal()

plot

ggsave(here("figures/col_vs_control.jpg"),plot)
