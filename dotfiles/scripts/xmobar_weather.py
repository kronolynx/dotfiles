#!/usr/bin/python3

# pip3 install python-yr 

from yr.libyr import Yr

weather = Yr(location_name='Spain/Madrid/Madrid')
data = weather.now()

weather = data['symbol']['@name']
temperature = data['temperature']['@value']

print ("<fc=#FFFFFF>%s %s °C</fc>" % (weather, temperature))
