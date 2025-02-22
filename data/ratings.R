# Manually taken from metacritic on 2021-03-18

ratings <- tibble::tribble(
  ~"game", ~"system", ~"score",
  "1-2-Switch","NS", 58,
  "Animal Crossing: City Folk","Wii", 73,
  "Animal Crossing: Wild World","DS", 86,
  "Army of Two","X360", 72,
  "Assassin's Creed","PS3", 81,
  "Assassin's Creed","X360", 81,
  "Assassin's Creed Odyssey","PS4", 83,
  "Assassin's Creed Odyssey","XOne", 87,
  "Assassin's Creed Origins","PS4", 81,
  "Assassin's Creed Origins","XOne", 85,
  "Astro Bot Rescue Mission","PS4", 90,
  "Battlefield V","PS4", 73,
  "Battlefield V","XOne", 78,
  "Bayonetta 2","NS", 92,
  "Big Brain Academy","DS", 74,
  "Big Brain Academy: Wii Degree","Wii", 68,
  "Brain Age 2: More Training in Minutes a Day","DS", 77,
  "Brain Age: Train Your Brain in Minutes a Day","DS", 77,
  "Call of Duty 4: Modern Warfare","X360", 94,
  "Call of Duty 4: Modern Warfare","PS3", 94,
  "Call of Duty: Black Ops IIII","PS4", 83,
  "Call of Duty: Black Ops IIII","XOne", 85,
  "Call of Duty: World at War","X360", 84,
  "Call of Duty: World at War","PS3", 85,
  "Call of Duty: WWII","PS4", 79,
  "Call of Duty: WWII","XOne", 80,
  "Captain Toad: Treasure Tracker","NS", 82,
  "Carnival Games","Wii", 56,
  "Cooking Mama","DS", 67,
  "Cooking Mama 2: Dinner With Friends","DS", 70,
  "Crash Bandicoot N. Sane Trilogy","PS4", 80,
  "Crash Bandicoot N. Sane Trilogy","NS", 79,
  "Crash Bandicoot N.Sane Trilogy","XOne", 48,
  "Crisis Core: Final Fantasy VII","PSP", 83,
  "Daxter","PSP", 85,
  "Deca Sports","Wii", 50,
  "Destiny 2","PS4", 85,
  "Detroit: Become Human","PS4", 78,
  "Devil May Cry 4","PS3", 84,
  "Devil May Cry 4","X360", 84,
  "Diablo III: Eternal Collection","NS", 88,
  "Donkey Kong Country: Tropical Freeze","NS", 86,
  "Dragon Ball Fighter Z","PS4", 86,
  "Dragon Quest V: Hand of the Heavenly Bride","DS", 84,
  "Dragon Quest XI","PS4", 86,
  "EA Sports UFC 3","PS4", 75,
  "Fable II","X360", 89,
  "Fallout 3","X360", 93,
  "Fallout 3","PS3", 90,
  "Fallout 4","PS4", 87,
  "Fallout 76","PS4", 53,
  "Fallout 76","XOne", 49,
  "Far Cry 2","X360", 85,
  "Far Cry 5","PS4", 81,
  "Far Cry 5","XOne", 82,
  "Farming Simulator 19", "PC", 73,
  "FIFA 18","PS4", 84,
  "FIFA 19","PS4", 83,
  "FIFA 19","XOne", 83,
  "FIFA 19","NS", 71,
  "FIFA Soccer 09","PS3", 87,
  "FIFA Soccer 09","X360", 87,
  "Flash Focus: Vision Training in Minutes a Day","DS", 59,
  "Fortnite","PS4", 78,
  "Fortnite","NS", 83,
  "Fortnite","XOne", 85,
  "Forza Horizon 4","XOne", 92,
  "Game Party","Wii", 25,
  "Gears of War 2","X360", 93,
  "God of War (PS4)","PS4", 94,
  "God of War: Chains of Olympus","PSP", 91,
  "Gran Turismo 5 Prologue","PS3", 80,
  "Gran Turismo Sport","PS4", 75,
  "Grand Theft Auto IV","X360", 98,
  "Grand Theft Auto IV","PS3", 98,
  "Grand Theft Auto V","PS4", 97,
  "Grand Theft Auto V","XOne", 97,
  "Grand Theft Auto: Liberty City Stories","PSP", 88,
  "Grand Theft Auto: Vice City Stories","PSP", 86,
  "Guitar Hero III: Legends of Rock","Wii", 86,
  "Guitar Hero III: Legends of Rock","X360", 85,
  "Guitar Hero III: Legends of Rock","PS3", 83,
  "Guitar Hero: On Tour","DS", 71,
  "Guitar Hero: World Tour","Wii", 86,
  "Guitar Hero: World Tour","X360", 85,
  "Halo 3","X360", 94,
  "Horizon: Zero Dawn","PS4", 89,
  "Imagine: Babyz","DS", 35,
  "Imagine: Fashion Designer","DS", 30,
  "Just Dance 2019","NS", 72,
  "Kingdom Hearts 1.5 + 2.5 Remix","PS4", 84,
  "Kirby","NS", 73,
  "Kirby Super Star Ultra","DS", 76,
  "Kung Fu Panda","X360", 75,
  "Left 4 Dead","X360", 89,
  "LEGO Indiana Jones: The Original Adventures","X360", 77,
  "LEGO Indiana Jones: The Original Adventures","DS", 80,
  "LEGO Indiana Jones: The Original Adventures","Wii", 78,
  "LEGO Marvel Super Heroes 2","PS4", 73,
  "LEGO Star Wars: The Complete Saga","DS", 80,
  "LEGO Star Wars: The Complete Saga","Wii", 80,
  "LEGO The Incredibles","NS", 65,
  "Link's Crossbow Training","Wii", 60,
  "LittleBigPlanet","PS3", 95,
  "Madden NFL 09","X360", 83,
  "Madden NFL 09","PS3", 85,
  "Madden NFL 19","PS4", 80,
  "Madden NFL 19","XOne", 81,
  "Mario & Sonic at the Olympic Games","Wii", 67,
  "Mario & Sonic at the Olympic Games","DS", 70,
  "Mario + Rabbids Kingdom Battle","NS", 85,
  "Mario Kart","3DS", 85,
  "Mario Kart 8 Deluxe","NS", 92,
  "Mario Kart DS","DS", 91,
  "Mario Kart Wii","Wii", 82,
  "Mario Party 8","Wii", 62,
  "Mario Party DS","DS", 72,
  "Mario Tennis Aces","NS", 75,
  "Metal Gear Solid 4: Guns of the Patriots","PS3", 94,
  "Minecraft","NS", 86,
  "MineCraft","XOne", 88,
  "MineCraft","PS4", 89,
  "MLB The Show 18","PS4", 82,
  "Monster Hunter Freedom Unite","PSP", 81,
  "Monster Hunter: World","PS4", 90,
  "Monster Hunter: World","XOne", 90,
  "MySims","DS", 67,
  "NBA 2K18","PS4", 82,
  "NBA 2K19","PS4", 80,
  "NBA 2K19","XOne", 85,
  "Need for Speed: Payback","PS4", 61,
  "New Super Mario Bros.","DS", 89,
  "Ni no Kuni II: Revenant Kingdom","PS4", 84,
  "Nintendo Labo: Toy-Con 01 Variety Kit","NS", 77,
  "Nintendogs","DS", 83,
  "Overwatch","PS4", 90,
  "Personal Trainer: Cooking","DS", 81,
  "PES 2009: Pro Evolution Soccer","PS3", 77,
  "PlayerUnknown's Battlegrounds","XOne", 85,
  "PlayStation VR Worlds","PS4", 59,
  "Pokemon: Let's Go, Eevee!","NS", 80,
  "Pokemon: Let's Go, Pikachu!","NS", 79,
  "Pokemon Diamond / Pearl Version","DS", 85,
  "Pokemon Mystery Dungeon: Explorers of Time / Darkness","DS", 60,
  "Pokemon Platinum Version","DS", 83,
  "Pokemon Ranger: Shadows of Almia","DS", 68,
  "Pokemon: Ultra Sun and Ultra Moon","3DS", 84,
  "Pokken Tournament","NS", 76,
  "Professor Layton and the Curious Village","DS", 85,
  "Project Octopath Traveler","NS", 83,
  "Ratchet & Clank: Size Matters","PSP", 85,
  "Red Dead Redemption 2","PS4", 97,
  "Red Dead Redemption 2","XOne", 97,
  "Resident Evil VII: Biohazard","PS4", 86,
  "Resistance 2","PS3", 87,
  "Resistance: Fall of Man","PS3", 86,
  "Rhythm Heaven","DS", 83,
  "Rock Band","X360", 92,
  "Rock Band","Wii", 80,
  "Rock Band 2","X360", 92,
  "Rocket League","PS4", 85,
  "Saints Row 2","X360", 81,
  "Sea of Thieves","XOne", 69,
  "Sega Superstars Tennis","X360", 67,
  "Shadow of the Colossus","PS4", 91,
  "Shadow of the Tomb Raider","PS4", 75,
  "Shadow of the Tomb Raider","XOne", 82,
  "SoulCalibur IV","X360", 85,
  "SoulCalibur VI","PS4", 84,
  "Spider-Man (PS4)","PS4", 87,
  "Splatoon 2","NS", 83,
  "Spyro Reignited Trilogy","PS4", 82,
  "Spyro Reignited Trilogy","XOne", 83,
  "Star Wars Battlefront II (2017)","PS4", 73,
  "Star Wars: The Force Unleashed","X360", 73,
  "Star Wars: The Force Unleashed","Wii", 71,
  "Star Wars: The Force Unleashed","PS3", 71,
  "Starlink: Battle for Atlas","NS", 64,
  "Super Mario 64 DS","DS", 85,
  "Super Mario Galaxy","Wii", 97,
  "Super Mario Odyssey","NS", 97,
  "Super Mario Party","NS", 76,
  "Super Smash Bros. (2018)","NS", 93,
  "Super Smash Bros. Brawl","Wii", 93,
  "The Crew 2","PS4", 64,
  "The Elder Scrolls V: Skyrim","NS", 84,
  "The Last of Us","PS4", 93,
  "The Legend of Zelda: Breath of the Wild","NS", 97,
  "The Legend of Zelda: Phantom Hourglass","DS", 90,
  "The Sims 4","PS4", 66,
  "Tom Clancy's Rainbow Six: Siege","PS4", 73,
  "Tom Clancy's Rainbow Six: Vegas 2","X360", 82,
  "Uncharted: Drake's Fortune","PS3", 88,
  "Wall-E","DS", 54,
  "Wii Fit","Wii", 80,
  "Wii Music","Wii", 63,
  "Wii Play","Wii", 58,
  "Wii Sports","Wii", 76,
  "WWE 2K19","PS4", 76,
  "Xenoblade Chronicles 2","NS", 83
)
