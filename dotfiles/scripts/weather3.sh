#!/bin/bash 

METRIC=1 #Should be 0 or 1; 0 for F, 1 for C
 
if [ -z $1 ]; then
echo
echo "USAGE: weather.sh <locationcode e.g tokyo>"
echo
exit 0;
fi
 
curl -s http://rss.accuweather.com/rss/liveweather_rss.asp\?metric\=${METRIC}\&locCode\=$1 | perl -ne 'use utf8; if (/Currently/) {chomp;/\<title\>Currently: (.*)?\<\/title\>/; my @values=split(":",$1); if( $values[0] eq "Sunny" || $values[0] eq "Mostly Sunny" || $values[0] eq "Partly Sunny" || $values[0] eq "Intermittent Clouds" || $values[0] eq "Hazy Sunshine" || $values[0] eq "Hazy Sunshine" || $values[0] eq "Hot") 
{
my $sun = "<fc=#FDFDFD> </fc>";
binmode(STDOUT, ":utf8");
print "$sun";
}
if( $values[0] eq "Mostly Cloudy" || $values[0] eq "Cloudy" || $values[0] eq "Dreary (Overcast)" || $values[0] eq "Fog")
{
my $cloud = "<fc=#FDFDFD> </fc>";
binmode(STDOUT, ":utf8");
print "$cloud";
}
if( $values[0] eq "Showers" || $values[0] eq "Mostly Cloudy w/ Showers" || $values[0] eq "Partly Sunny w/ Showers" || $values[0] eq "T-Storms"|| $values[0] eq "Mostly Cloudy w/ T-Storms"|| $values[0] eq "Partly Sunny w/ T-Storms"|| $values[0] eq "Rain")
{
my $rain = "<fc=#FDFDFD> </fc>";
binmode(STDOUT, ":utf8");
print "$rain";
}
if( $values[0] eq "Windy")
{
my $wind = "<fc=#FDFDFD> </fc>";
binmode(STDOUT, ":utf8");
print "$wind";
} 
if($values[0] eq "Flurries" || $values[0] eq "Mostly Cloudy w/ Flurries" || $values[0] eq "Partly Sunny w/ Flurries"|| $values[0] eq "Snow"|| $values[0] eq "Mostly Cloudy w/ Snow"|| $values[0] eq "Ice"|| $values[0] eq "Sleet"|| $values[0] eq "Freezing Rain"|| $values[0] eq "Rain and Snow"|| $values[0] eq "Cold")
{
my $snow = "";
binmode(STDOUT, ":utf8");
print "$rain";
}
if($values[0] eq "Clear" || $values[0] eq "Mostly Clear" || $values[0] eq "Partly Cloudy"|| $values[0] eq "Intermittent Clouds"|| $values[0] eq "Hazy Moonlight"|| $values[0] eq "Mostly Cloudy"|| $values[0] eq "Partly Cloudy w/ Showers"|| $values[0] eq "Mostly Cloudy w/ Showers"|| $values[0] eq "Partly Cloudy w/ T-Storms"|| $values[0] eq "Mostly Cloudy w/ Flurries" || $values[0] eq "Mostly Cloudy w/ Snow")
{
my $night = "<fc=#FDFDFD> </fc>";
binmode(STDOUT, ":utf8");
print "$night";
}
if($values[1] > 30){
print"<fc=#FDFDFD>$values[1]</fc>";
} elsif ($values[1] > 16) {
print"<fc=#FDFDFD>$values[1]</fc>";
} else {
print"<fc=#FDFDFD>$values[1]</fc>";
}
 }'