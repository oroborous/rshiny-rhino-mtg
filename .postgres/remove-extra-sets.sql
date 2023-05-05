delete from mtg_prices where uuid in (select uuid from mtg_cards where setcode not in (
'ARN', 'ATQ', 'LEG', 'DRK', 'FEM', 'ICE', 'HML',
'ALL', 'MIR', 'VIS', 'WTH', 'TMP', 'STH', 'EXO',
'USG', 'ULG', 'UDS', 'MMQ', 'NEM', 'PCY', 'INV',
'PLS', 'APC', 'ODY', 'TOR', 'JUD', 'ONS', 'LGN',
'SCG', 'MRD', 'DST', '5DN', 'CHK', 'BOK', 'SOK',
'RAV', 'GPT', 'DIS', 'CSP', 'TSP', 'PLC', 'FUT',
'LRW', 'MOR', 'SHM', 'EVE', 'ALA', 'CON', 'ARB',
'ZEN', 'WWK', 'ROE', 'SOM', 'MBS', 'NPH', 'ISD',
'DKA', 'AVR', 'RTR', 'GTC', 'DGM', 'THS', 'BNG',
'JOU', 'KTK', 'FRF', 'BFZ', 'OGW', 'SOI', 'EMN',
'KLD', 'AER', 'AKH', 'HOU', 'XLN', 'RIX', 'LEA',
'LEB', '2ED', '3ED', '4ED', '5ED', '6ED', '7ED',
'8ED', '9ED', '10E', 'M10', 'M11', 'M12', 'M13',
'M14', 'M15', 'M16', 'ORI', 'M19', 'M20', 'M21'));

delete from mtg_avg_prices where uuid in (select uuid from mtg_cards where setcode not in (
'ARN', 'ATQ', 'LEG', 'DRK', 'FEM', 'ICE', 'HML',
'ALL', 'MIR', 'VIS', 'WTH', 'TMP', 'STH', 'EXO',
'USG', 'ULG', 'UDS', 'MMQ', 'NEM', 'PCY', 'INV',
'PLS', 'APC', 'ODY', 'TOR', 'JUD', 'ONS', 'LGN',
'SCG', 'MRD', 'DST', '5DN', 'CHK', 'BOK', 'SOK',
'RAV', 'GPT', 'DIS', 'CSP', 'TSP', 'PLC', 'FUT',
'LRW', 'MOR', 'SHM', 'EVE', 'ALA', 'CON', 'ARB',
'ZEN', 'WWK', 'ROE', 'SOM', 'MBS', 'NPH', 'ISD',
'DKA', 'AVR', 'RTR', 'GTC', 'DGM', 'THS', 'BNG',
'JOU', 'KTK', 'FRF', 'BFZ', 'OGW', 'SOI', 'EMN',
'KLD', 'AER', 'AKH', 'HOU', 'XLN', 'RIX', 'LEA',
'LEB', '2ED', '3ED', '4ED', '5ED', '6ED', '7ED',
'8ED', '9ED', '10E', 'M10', 'M11', 'M12', 'M13',
'M14', 'M15', 'M16', 'ORI', 'M19', 'M20', 'M21'));

delete from mtg_collections where uuid in (select uuid from mtg_cards where setcode not in (
'ARN', 'ATQ', 'LEG', 'DRK', 'FEM', 'ICE', 'HML',
'ALL', 'MIR', 'VIS', 'WTH', 'TMP', 'STH', 'EXO',
'USG', 'ULG', 'UDS', 'MMQ', 'NEM', 'PCY', 'INV',
'PLS', 'APC', 'ODY', 'TOR', 'JUD', 'ONS', 'LGN',
'SCG', 'MRD', 'DST', '5DN', 'CHK', 'BOK', 'SOK',
'RAV', 'GPT', 'DIS', 'CSP', 'TSP', 'PLC', 'FUT',
'LRW', 'MOR', 'SHM', 'EVE', 'ALA', 'CON', 'ARB',
'ZEN', 'WWK', 'ROE', 'SOM', 'MBS', 'NPH', 'ISD',
'DKA', 'AVR', 'RTR', 'GTC', 'DGM', 'THS', 'BNG',
'JOU', 'KTK', 'FRF', 'BFZ', 'OGW', 'SOI', 'EMN',
'KLD', 'AER', 'AKH', 'HOU', 'XLN', 'RIX', 'LEA',
'LEB', '2ED', '3ED', '4ED', '5ED', '6ED', '7ED',
'8ED', '9ED', '10E', 'M10', 'M11', 'M12', 'M13',
'M14', 'M15', 'M16', 'ORI', 'M19', 'M20', 'M21'));

delete from mtg_collections where useremail = 'fry@email.com'
and uuid in (select uuid from mtg_cards where setcode not in (
'ARN', 'ATQ', 'LEG', 'DRK', 'FEM', 'ICE', 'HML',
'ALL', 'MIR', 'VIS', 'WTH', 'TMP', 'STH', 'EXO',
'USG', 'ULG', 'UDS', 'MMQ', 'NEM', 'PCY', 'INV',
'PLS', 'APC', 'ODY', 'TOR', 'JUD', 'ONS', 'LGN',
'SCG', 'MRD', 'DST', '5DN', 'CHK', 'BOK', 'SOK',
'RAV', 'GPT', 'DIS', 'CSP', 'TSP', 'PLC', 'FUT',
'LRW', 'MOR', 'SHM', 'EVE', 'ALA', 'CON', 'ARB',
'ZEN', 'WWK', 'ROE', 'SOM', 'MBS', 'NPH', 'ISD',
'DKA', 'AVR', 'RTR', 'GTC', 'DGM', 'THS', 'BNG',
'JOU', 'KTK', 'FRF', 'BFZ', 'OGW', 'SOI', 'EMN',
'KLD', 'AER', 'AKH', 'HOU', 'XLN', 'RIX'));

delete from mtg_collections where useremail = 'stacy@email.com'
and uuid in (select uuid from mtg_cards where setcode not in (
'LEA',
'LEB', '2ED', '3ED', '4ED', '5ED', '6ED', '7ED',
'8ED', '9ED', '10E', 'M10', 'M11', 'M12', 'M13',
'M14', 'M15', 'M16', 'ORI', 'M19', 'M20', 'M21'));

delete from mtg_cards where uuid in (select uuid from mtg_cards where setcode not in (
'ARN', 'ATQ', 'LEG', 'DRK', 'FEM', 'ICE', 'HML',
'ALL', 'MIR', 'VIS', 'WTH', 'TMP', 'STH', 'EXO',
'USG', 'ULG', 'UDS', 'MMQ', 'NEM', 'PCY', 'INV',
'PLS', 'APC', 'ODY', 'TOR', 'JUD', 'ONS', 'LGN',
'SCG', 'MRD', 'DST', '5DN', 'CHK', 'BOK', 'SOK',
'RAV', 'GPT', 'DIS', 'CSP', 'TSP', 'PLC', 'FUT',
'LRW', 'MOR', 'SHM', 'EVE', 'ALA', 'CON', 'ARB',
'ZEN', 'WWK', 'ROE', 'SOM', 'MBS', 'NPH', 'ISD',
'DKA', 'AVR', 'RTR', 'GTC', 'DGM', 'THS', 'BNG',
'JOU', 'KTK', 'FRF', 'BFZ', 'OGW', 'SOI', 'EMN',
'KLD', 'AER', 'AKH', 'HOU', 'XLN', 'RIX', 'LEA',
'LEB', '2ED', '3ED', '4ED', '5ED', '6ED', '7ED',
'8ED', '9ED', '10E', 'M10', 'M11', 'M12', 'M13',
'M14', 'M15', 'M16', 'ORI', 'M19', 'M20', 'M21'));

-- refresh materialized view mv_prices;

-- refresh materialized view mv_price_history;