library(rvest)

# Mens --------------


year = 2017

event_url = paste('http://www.worldsurfleague.com/events/',paste(year,'/mct', sep = ''),sep = '')

events = read_html(event_url)

events2 = html_nodes(events,'.tour-event-range')

events3 = html_text(events2) 