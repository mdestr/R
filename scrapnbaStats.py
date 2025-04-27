from basketball_reference_scraper.teams import get_roster, get_roster_stats
import pandas as pd

# Liste des abréviations des équipes NBA
TEAMS = [
    'OKC', 'MIL', 'DEN', 'MIN', 'BOS', 'PHO', 'PHI', 'DET', 'NYK', 'ORL',
    'CHA', 'DAL', 'NOP', 'GSW', 'LAL', 'SAS', 'ATL', 'BRK', 'CLE', 'MIA',
    'MEM', 'LAC', 'SAC', 'TOR', 'HOU', 'WAS', 'CHI', 'IND', 'POR', 'UTA'
]

YEAR = 2025
all_data = []

for team in TEAMS:
    try:
        print(f"Processing {team}...")
        
        # 1. Get roster data
        roster_df = get_roster(team, YEAR)
        
        # 2. Get stats data
        stats_df = get_roster_stats(team, YEAR, data_format='PER_GAME', playoffs=False)
        
        # 3. Merge both datasets on PLAYER column
        merged = pd.merge(roster_df, stats_df, on='PLAYER', how='left')
        merged['TEAM'] = team
        
        # 4. Add to final dataset
        all_data.append(merged)
        
    except Exception as e:
        print(f"Error with {team}: {str(e)}")

# Combine all teams data
final_df = pd.concat(all_data, ignore_index=True)

# Clean and reorder columns
# cols = ['TEAM', 'NUMBER', 'PLAYER', 'POS', 'AGE', 'HEIGHT', 'WEIGHT', 
#         'G', 'GS', 'MP', 'PTS', 'TRB', 'AST', 'STL', 'BLK', 'FG%', '3P%', 'FT%',
#         'BIRTH_DATE', 'NATIONALITY', 'EXPERIENCE', 'COLLEGE', 'Awards']

cols = ['TEAM', 'NUMBER', 'PLAYER', 'POS', 'HEIGHT', 'WEIGHT', 'BIRTH_DATE', 
  'NATIONALITY', 'EXPERIENCE', 'COLLEGE', 'G', 'MP', 'FG', 'FGA', 'FG%', 
  '3P', '3PA', '3P%', '2P', '2PA', '2P%', 'FT', 'FTA', 'FT%', 'ORB', 
  'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS']

        
final_df = final_df.reindex(columns=[c for c in cols if c in final_df.columns])

# Save to CSV
final_df.to_csv(f'nba_players_merged_{YEAR}.csv', index=False)
print(f"Data saved for {len(final_df)} players from {len(TEAMS)} teams")
