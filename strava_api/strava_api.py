# In this bit of code I use python to set up and use an API connector to my Strava account data, visualize activity maps and analyze activity data.
# This youtube video explains how to get the neccesary access and refresh tokens for Strava data: https://www.youtube.com/watch?v=sgscChKfGyg&t=258s&ab_channel=franchyze923

### Import required modules
import requests
import pandas as pd
from pandas import json_normalize
import urllib3
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from datetime import datetime
import time
import matplotlib.pyplot as plt
import folium
import polyline
import base64
from tqdm import tqdm
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning) # disable some https connection-related warnings

### Use API connector to pull Strava data
# set urls to access strava data
auth_url = "https://www.strava.com/oauth/token"
activities_url = "https://www.strava.com/api/v3/athlete/activities"

payload = {
    'client_id': "my_client_id",
    'client_secret': "my_client_secret",
    'refresh_token': "my_refresh_token",
    'grant_type': "my_refresh_token",
    'f': "json"
}

# request access token
print("Requesting Token...\n")
res = requests.post(auth_url, data=payload, verify=False)
access_token = res.json()['access_token']
print("Access Token = {}\n".format(access_token))

# use token to access activity data
header = {'Authorization': 'Bearer ' + access_token}
param = {'per_page': 500, 'page': 1}
strava_activities_data = requests.get(activities_url, headers=header, params=param).json()

# convert JSON data to flat table
activities = json_normalize(strava_activities_data)
if "activities" in locals(): print("Successfully requested data.\n")
  
### Wrangle and clean data
# break start date into start time and date
activities['start_date_local'] = pd.to_datetime(activities['start_date_local'])
activities['start_time'] = activities['start_date_local'].dt.time
activities['start_date_local'] = activities['start_date_local'].dt.date

# switch speed to km/h
activities['average_speed_kmh'] = activities['average_speed'] * 3.6
activities['average_speed_mph'] = activities['average_speed'] * 2.237

# add decoded summary polylines
activities['map.polyline'] = activities['map.summary_polyline'].apply(polyline.decode)

# drop irrelevant columns
activities.drop(
    [
        'map.summary_polyline', 'resource_state','external_id','upload_id',
        'location_city', 'location_state', 'has_kudoed', 'start_date', 'athlete.resource_state', 
        'utc_offset','map.resource_state', 'athlete.id', 'visibility', 'heartrate_opt_out', 'upload_id_str',
        'from_accepted_tag', 'map.id', 'manual', 'private', 'flagged',
    ], 
    axis=1, 
    inplace=True
)

# separate out activities of interest (in this case cycling activities)
rides = activities.loc[activities['type'] == 'Ride']

# set index based on start date
rides.set_index('start_date_local', inplace=True)

### Plot ride map
# plot map of most recent or longest ride
which_ride = 'last'
# select activity
if which_ride == 'longest':
    my_ride = rides[rides.distance == rides.distance.max()].squeeze() # longest ride, based on maximum distance
elif which_ride == 'last':
    my_ride = rides.iloc[0, :] # most recent ride, based on index 

# get center coordinates
centroid = [ 
    np.mean([coord[0] for coord in my_ride['map.polyline']]),
    np.mean([coord[1] for coord in my_ride['map.polyline']])
]
# create folium
ride_map = folium.Map(location=centroid, zoom_start=10)
folium.PolyLine(my_ride['map.polyline'], color='red').add_to(ride_map) # add route as layer
ride_map.fit_bounds(ride_map.get_bounds(), padding=(20, 20)) # zoom in to route area
display(ride_map)
# save map
ride_map.save("ride_map.html")
