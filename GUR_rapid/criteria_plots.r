
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
vessel_years_trips <<- subsetter_tow$data %>%
  filter(catch>=0) %>%
  group_by(vessel,fyear) %>%
  summarise(trips=length(unique(trip)))

qualify <- function(.){
  trips_ <- max(.$trips_min)
  years_ <- max(.$years_min)
  vessel_years_trips %>%
    filter(trips>=trips_) %>%
    group_by(vessel) %>%
    summarise(years=length(fyear)) %>%
    filter(years>=years_)
}

criteria_vessels <- expand.grid(trips_min=c(1,3,5,10), years_min=1:10) %>%
  group_by(trips_min, years_min) %>%
  do(vessels=qualify(.))
# Calculate number of vessels and catch for each combination
criteria_vessels <- criteria_vessels %>% bind_cols(
  criteria_vessels %>%
    do(data.frame(
      num = nrow(.$vessels),
      catch = sum(subsetter_tow$data$catch[subsetter_tow$data$vessel %in% .$vessels$vessel])
    ))
)
# Normalise catches
criteria_vessels <- within(criteria_vessels, {
  catch <- catch / sum(subsetter_tow$data$catch) * 100
})

# Plot it
plot_base <- ggplot(criteria_vessels,aes(x=years_min, colour=factor(trips_min), shape=factor(trips_min))) +
  scale_shape_manual(values=1:10) +
  labs(x='Min (fyear)', colour='Min (trips)', shape='Min (trips)')

plot_catch <- plot_base + geom_point(aes(y=catch),size=2,alpha=0.7) +
  ylim(0,NA) + labs(y='Percentage of catch') + scale_x_continuous(breaks = c(0,2,4,6,8,10))
leg <- get_legend(plot_catch)
plot_catch <- plot_catch + theme(legend.position="none")

plot_vessels <- plot_base + geom_point(aes(y=num),size=2,alpha=0.7) +
  ylim(0,NA) + labs(y='Number of vessels') + theme(legend.position="none") + scale_x_continuous(breaks = c(0,2,4,6,8,10))

plot_grid(plot_catch, plot_vessels, leg,  align = 'hv', labels=c('A', 'B'), rel_widths =c(1,1,0.4),ncol = 3)
