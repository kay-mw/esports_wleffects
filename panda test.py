import requests
import pandas as pd

headers = {
    "accept": "application/json",
    "authorization": "Bearer V6AgJUND-4v26c2PFZTRu2_V4yJ9GvZTMar1I9fe6RhRTFCnW9o"
}

response = []
for page in range(1,600):
    url = "https://api.pandascore.co/csgo/matches/past?per_page=100&filter%5Bstatus%5D=finished&page="+str(page)
    page_response = requests.get(url, headers=headers).json()
    response.extend(page_response)

for item in response:
    for game_number,game in enumerate(item['games']):
        item['game_'+str(game_number)]=game
    for opponent_number,opponent in enumerate(item['opponents']):
        item['opponent_'+str(opponent_number)]=opponent['opponent']
    for result_number,result in enumerate(item['results']):
        item['result_'+str(result_number)]=result


df = pd.json_normalize(response)
df.drop(columns=['opponents','modified_at','slug','streams_list','live.opens_at','live.supported','live.url',
                 'videogame_title.slug','videogame.slug','winner.image_url','winner.modified_at','winner.slug',
                 'serie.modified_at','serie.slug','tournament.detailed_stats','tournament.live_supported',
                 'tournament.modified_at','tournament.slug','league.image_url','league.modified_at','league.slug',
                 'league.url','detailed_stats','opponent_0.image_url','opponent_0.modified_at','opponent_0.slug',
                 'opponent_1.image_url','opponent_1.modified_at','opponent_1.slug','games','results','game_0.complete',
                 'game_0.detailed_stats','game_0.finished','game_0.id','game_0.match_id','game_0.position',
                 'game_0.winner.type','game_0.winner_type','game_1.complete','game_1.detailed_stats','game_1.finished',
                 'game_1.id','game_1.match_id','game_1.position','game_1.winner.type','game_1.winner_type',
                 'game_2.complete','game_2.detailed_stats','game_2.finished','game_2.id','game_2.match_id',
                 'game_2.position','game_2.winner.type','game_2.winner_type','game_3.complete','game_3.detailed_stats',
                 'game_3.finished','game_3.id','game_3.match_id','game_3.position','game_3.winner.type',
                 'game_3.winner_type','game_4.complete','game_4.detailed_stats','game_4.finished','game_4.id',
                 'game_4.match_id','game_4.position','game_4.winner.type','game_4.winner_type'],inplace=True)
df.style

print(df)

df.to_csv('csgo_data_game_filtered.csv')