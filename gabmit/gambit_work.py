import pandas as pd
from datetime import datetime, timedelta
import os
from dateutil import parser
import sqlite3
import numpy as np

dir = r"c:\Users\Student\Desktop\wynn programs\raiddays"
playercount_data_path = os.path.join(dir, "gambit_may.csv") #normalisation data no longer able to be used since privatisation, this would need to be normalised against itself
april15_data_path = os.path.join(dir, "may15.csv") #valor raid data, individuals raid completions
gambit_data_path = os.path.join(dir, "gambitss.csv") #csv boolean represntation of present gambits per day
playercount_data = pd.read_csv(playercount_data_path)
april15_data = pd.read_csv(april15_data_path)
gambit_data = pd.read_csv(gambit_data_path)
sessions = pd.read_csv(playercount_data_path)  #columns: player_uuid, join_time, retrieved_time
sessions['join_time'] = pd.to_datetime(sessions['join_time'])
sessions['retrieved_time'] = pd.to_datetime(sessions['retrieved_time'])

def get_raid_day(ts): #sets the day in line with gambit flip
    if not isinstance(ts, datetime):
        dt = parser.parse(str(ts))
    else:
        dt = ts
    if dt.hour < 17:
        dt -= timedelta(days=1)
    return dt.date()

sessions['raid_day'] = sessions['join_time'].apply(get_raid_day)
sessions['session_hours'] = (sessions['retrieved_time'] - sessions['join_time']).dt.total_seconds() / 3600

print("sessions loaded:", sessions.shape)
player_day_playtime = sessions.groupby(['player_uuid', 'raid_day'])['session_hours'].sum().reset_index()
print("player_day_playtime:", player_day_playtime.shape)
playtime_per_day = player_day_playtime.groupby('raid_day')['session_hours'].sum().reset_index()
playtime_per_day.rename(columns={'session_hours': 'playtime_hours'}, inplace=True)
print("playtime_per_day:", playtime_per_day.shape)

def adjust_to_raid_day(unix_time):
    dt = datetime.utcfromtimestamp(unix_time)
    if dt.hour < 17:
        dt -= timedelta(days=1)
    return dt.date()
april15_data['raid_day'] = april15_data['time'].apply(adjust_to_raid_day)
print("april15_data loaded:", april15_data.shape)
print(april15_data.head())

raids = [
    'g_The Nameless Anomaly',
    'g_Nest of the Grootslangs',
    'g_The Canyon Colossus',
    'g_Orphion\'s Nexus of Light'
]
f_raids = april15_data[
    (april15_data['label'].isin(raids)) & (april15_data['delta'] <= 200)
]

raid_counts = f_raids.groupby(['raid_day', 'label'])['delta'].sum().reset_index()

md = raid_counts.merge(playtime_per_day, left_on='raid_day', right_on='raid_day', how='left')
md['normalized_delta'] = md['delta'] / md['playtime_hours']
gambit_data = pd.read_csv(gambit_data_path)
gambit_data['date'] = pd.to_datetime(gambit_data['Date'], infer_datetime_format=True).dt.date
fd = md.merge(gambit_data, left_on='raid_day', right_on='date', how='inner')
fd = fd.drop(columns=['date'])

gambit_columns = [col for col in gambit_data.columns if col not in ['Date', 'date']]
for gambit in gambit_columns:
    fd[gambit] = fd[gambit].astype(bool)

gambit_danger = {}
for gambit in gambit_columns:
    withgambit = fd[fd[gambit]]['normalized_delta'].mean()
    withoutgambit = fd[~fd[gambit]]['normalized_delta'].mean()
    gambit_danger[gambit] = withgambit - withoutgambit

sorted_gambit_danger = sorted(gambit_danger.items(), key=lambda x: x[1], reverse=True)
print("Gambit Danger:")
for gambit, impact in sorted_gambit_danger:
    print(f"{gambit}: {impact:.4f}")

for raid in raids:
    print(f"\nGambit Danger Analysis for {raid}:")
    raid_fd = fd[fd['label'] == raid]
    raid_gambit_danger = {}
    for gambit in gambit_columns:
        withgambit = raid_fd[raid_fd[gambit]]['normalized_delta'].mean()
        withoutgambit = raid_fd[~raid_fd[gambit]]['normalized_delta'].mean()
        raid_gambit_danger[gambit] = withgambit - withoutgambit
    sorted_raid_gambit_danger = sorted(raid_gambit_danger.items(), key=lambda x: x[1], reverse=True)
    for gambit, impact in sorted_raid_gambit_danger:
        print(f"{gambit}: {impact:.4f}")

raids = pd.read_csv(april15_data_path)
raids_small = raids.loc[:, ['uuid', 'label', 'time', 'delta']].copy()

raids_small['uuid'] = raids_small['uuid'].astype(str)
raids_small['time'] = pd.to_numeric(raids_small['time'], errors='coerce')

sessions_small = sessions.loc[:, ['player_uuid', 'join_time', 'retrieved_time', 'raid_day']].copy()
sessions_small['player_uuid'] = sessions_small['player_uuid'].astype(str)

sessions_small['join_time'] = pd.to_datetime(sessions_small['join_time'], utc=True, errors='coerce')
sessions_small['retrieved_time'] = pd.to_datetime(sessions_small['retrieved_time'], utc=True, errors='coerce')

def _chunked_merge(left, right, left_on, right_on, chunksize=20000):
    parts = []
    for start in range(0, len(left), chunksize):
        part = left.iloc[start:start+chunksize]
        merged_part = pd.merge(part, right, left_on=left_on, right_on=right_on, how='inner')
        parts.append(merged_part)
    if parts:
        return pd.concat(parts, ignore_index=True)
    return pd.DataFrame(columns=list(left.columns) + list(right.columns))

try:
    merged = pd.merge(
        raids_small,
        sessions_small,
        left_on='uuid',
        right_on='player_uuid',
        suffixes=('', '_sess'),
        how='inner'
    )
except Exception as e:
    merged = _chunked_merge(raids_small, sessions_small, left_on='uuid', right_on='player_uuid', chunksize=20000)

merged['raid_time'] = pd.to_datetime(merged['time'], unit='s', utc=True, errors='coerce')

merged['join_time'] = pd.to_datetime(merged['join_time'], utc=True, errors='coerce')
merged['retrieved_time'] = pd.to_datetime(merged['retrieved_time'], utc=True, errors='coerce')

in_session = merged[
    (merged['raid_time'] >= merged['join_time']) &
    (merged['raid_time'] <= merged['retrieved_time'])
].copy()

del raids_small, sessions_small


print("merged:", merged.shape)
print("in_session:", in_session.shape)

in_session['raid_day'] = in_session['raid_day']
raidsinc = [
    'g_The Nameless Anomaly',
    'g_Nest of the Grootslangs',
    'g_The Canyon Colossus',
    'g_Orphion\'s Nexus of Light'
]
f_raids = in_session[
    (in_session['label'].isin(raidsinc)) & (in_session['delta'] <= 200)
]
raid_counts = f_raids.groupby(['raid_day', 'label'])['delta'].sum().reset_index()

md = raid_counts.merge(playtime_per_day, on='raid_day', how='left')
md['normalized_delta'] = md['delta'] / md['playtime_hours']


print("\nend analyssi")

db_path = os.path.join(dir, "public_uuids.db")
conn = sqlite3.connect(db_path)
query = """
SELECT timestamp, archetype
FROM character_data
"""
adf = pd.read_sql_query(query, conn)
conn.close()

def get_raid_day_from_unix(ts):
    try:
        dt = datetime.utcfromtimestamp(float(ts))
    except (ValueError, TypeError):
        dt = parser.parse(str(ts))
    if dt.hour < 17:
        dt -= timedelta(days=1)
    return dt.date()

adf['raid_day'] = adf['timestamp'].apply(get_raid_day_from_unix)

adf = adf[adf['raid_day'] > datetime(2025, 4, 19).date()]

acounts = adf.groupby(['raid_day', 'archetype']).size().reset_index(name='archetype_count')
fd_arche = fd.merge(acounts, on='raid_day', how='left')

agsens = {}
for archetype in fd_arche['archetype'].dropna().unique():
    agsens[archetype] = {}
    sub = fd_arche[fd_arche['archetype'] == archetype]
    for gambit in gambit_columns:
        # Drop rows where gambit is NA
        sub_g = sub.dropna(subset=[gambit])
        withgambit = sub_g.loc[sub_g[gambit] == True, 'normalized_delta'].mean()
        withoutgambit = sub_g.loc[sub_g[gambit] == False, 'normalized_delta'].mean()
        agsens[archetype][gambit] = withgambit - withoutgambit

print("\nArchetype Gambit Sensitivity (delta in normalized raid completions per hour):")
for archetype, gambit_impacts in agsens.items():
    print(f"\n{archetype}:")
    sorted_impacts = sorted(gambit_impacts.items(), key=lambda x: x[1], reverse=True)
    for gambit, impact in sorted_impacts:
        print(f"  {gambit}: {impact:.4f}")

archetype_day = adf.groupby(['raid_day', 'archetype']).size().reset_index(name='count')
total_day = adf.groupby('raid_day').size().reset_index(name='total')
archetype_day = archetype_day.merge(total_day, on='raid_day')
archetype_day['archetype_share'] = archetype_day['count'] / archetype_day['total']

for gambit in gambit_columns:
    gambit_per_day = fd.groupby('raid_day')[gambit].any()
    archetype_day[gambit] = archetype_day['raid_day'].map(gambit_per_day)

sensitivity = {}
for archetype in archetype_day['archetype'].unique():
    sensitivity[archetype] = {}
    sub = archetype_day[archetype_day['archetype'] == archetype]
    for gambit in gambit_columns:
        sub_g = sub.dropna(subset=[gambit])
        withgambit = sub_g.loc[sub_g[gambit] == True, 'archetype_share'].mean()
        withoutgambit = sub_g.loc[sub_g[gambit] == False, 'archetype_share'].mean()
        sensitivity[archetype][gambit] = withgambit - withoutgambit

print("\nArchetype Gambit Sensitivity (absolute change in daily share):")
for archetype, gambit_impacts in sensitivity.items():
    print(f"\n{archetype}:")
    sorted_impacts = sorted(gambit_impacts.items(), key=lambda x: x[1], reverse=True)
    for gambit, impact in sorted_impacts:
        print(f"  {gambit}: {impact:.4f}")

print("\nArchetype Overall Sensitivity Score (mean across gambits):")
for archetype, gambit_impacts in sensitivity.items():
    mean_sens = sum(gambit_impacts.values()) / len(gambit_impacts)
    print(f"{archetype}: {mean_sens:.4f}")

overall_scores = []
for archetype, gambit_impacts in sensitivity.items():
    if archetype == "N/A":
        continue
    mean_sens = sum(gambit_impacts.values()) / len(gambit_impacts)
    overall_scores.append((archetype, mean_sens))

overall_scores = sorted(overall_scores, key=lambda x: x[1], reverse=True)
print("\nArchetype Overall Sensitivity Score (mean across gambits, sorted):")
for archetype, mean_sens in overall_scores:
    print(f"{archetype}: {mean_sens:.4f}")



print("\nFUrther analysis")

arcr_sens = {}
for archetype in fd_arche['archetype'].dropna().unique():
    if archetype == "N/A":
        continue
    arcr_sens[archetype] = {}
    
    for raid in raidsinc:
        arcr_sens[archetype][raid] = {}
        sub = fd_arche[(fd_arche['archetype'] == archetype) & (fd_arche['label'] == raid)]
        
        for gambit in gambit_columns:
            sub_g = sub.dropna(subset=[gambit])
            if len(sub_g) > 0:
                withgambit = sub_g.loc[sub_g[gambit] == True, 'normalized_delta'].mean()
                withoutgambit = sub_g.loc[sub_g[gambit] == False, 'normalized_delta'].mean()
                sensitivity_val = withgambit - withoutgambit
                arcr_sens[archetype][raid][gambit] = sensitivity_val
            else:
                arcr_sens[archetype][raid][gambit] = 0.0

print("\nEnd of further analysis")
for archetype in sorted(arcr_sens.keys()):
    print(f"\n{archetype}:")
    for raid in raidsinc:
        print(f"  {raid}:")
        gambit_impacts = arcr_sens[archetype][raid]
        sorted_impacts = sorted(gambit_impacts.items(), key=lambda x: x[1], reverse=True)
        for gambit, impact in sorted_impacts:
            if not np.isnan(impact):
                print(f"    {gambit}: {impact:.4f}")

