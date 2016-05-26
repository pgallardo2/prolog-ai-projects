% room/5
% room(Name, Description, Agents, Objects, Exits)

% updates 4-14-16
%
% In throneRoom, changed "vampire" to "vampireKing"

:-dynamic room/5.

room(treasureRoom,'This room is filled with piles of jewelry and coins. There is a very nice set of steel armor against a wall along stacks of ancient paintings and statues.   An old cloth doll of a young girl lays long forgotten on the floor.',[vampire],[crown],[exit(clockTower,visible, unlocked)]).

room(clockTower,'Gears, cogs and chains are bolted to the walls and arrange intricately throughout the room.  The wooden floors creak as you walk, and you catch the sounds and smells of rats. A gas lamp casts an erie glow.  It seems that something is missing within the machine.',[],[],[exit(treasureRoom,visible,locked), exit(royalChamber,visible,unlocked)]).

room(royalChamber,'The small arched-ceiling hallway seems to impose upon your will as to take each step.  There is a door to the west with a brass lock that wont open. The clock tower is to the north, the bedroom to the south and the west stairs behind you.',[],[candle],[exit(clockTower,visible,unlocked),exit(storage,hidden,locked), exit(bedroom,visible,unlocked), exit(westStairs,visible,unlocked)]).

room(storage,'This room is full of boxes, barrels, and clothing with a sarcophagus in the corner.',[vampire],[helmet],[exit(royalChamber,visible,unlocked)]).

room(westStairs,'It seems to take forever to reach then end of the stairs.  On the landing half way up, an inset bookcase holds a tattered portrait and a statue of a young girl with glowing red gems for eyes.  A barrier of force prevents you from moving upward.',[],[],[exit(lobby,visible,unlocked),exit(royalChamber,hidden,unlocked)]).

room(lobby, 'You are in a dark, cramped foyer. To the north, a thick wooden door opens to a courtyard.East stairs descend to a cellar and  west stairs climb into darkness.',[],[brassKey,woodenStake],[exit(westStairs,visible,unlocked),exit(eastStairs,visible,unlocked),exit(courtyard,visible,unlocked)]).

room(eastStairs,'The stone spiral stairwell is dark and small.  Half-way down you find a candelabra within an inset in the stairwell and a tray of old dishes placed on the stairs.',[vampire],[],[exit(lobby,visible,unlocked),exit(basement,visible,unlocked)]).

room(courtyard, 'You are in the castle`s courtyard.  To the west there are doors with colored glass. To the east, a passage covered with vines leads into darkness.  To the south, a door leads to the foyer. A fountain rests in the room`s center. A chapel lies in the distant west and the foyer to the south. A dark path leads to the dining hall in the east.',[], [cloak], [exit(diningHall, visible, unlocked), exit(lobby, visible, unlocked), exit(chapel, visible, unlocked)]).

room(chapel, 'Overturned benches lay scattered throughout the room.   On the south wall is an array of more colored glass windows forming symbols that do not remind you of anything.  At the west end of the room, what used to be a tall pulpit is now burnt and charred.  A crumbling doorway leads north into a darkened passage.Benches are thrown about the room. You debate whether to return to the courtyard or continue on to the art room.',[], [crucifix], [exit(artRoom, visible, unlocked), exit(courtyard, visible, unlocked)]).

room(artRoom, 'A long narrow room lined with covered paintings and statues.  Cobwebs fill every corner of the room, and a broken mirror lies against the west wall, reflecting the room a hundred times over in its many pieces.  Covering the north wall is an enormous tapestry depicting this castle beneath a terrible dark maroon vortex. Fabric covers the paintings and statues that line the long narrow room.  An enormous tapestry covers the north wall.',[], [], [exit(chapel, visible, unlocked), exit(library, hidden, locked)]).

room(library,'All of the tall walls of this room are lined with ancient and rotting books.  A ladder on wheels leans against the northern bookshelf.  In the center of the room on a pitted metal table is an ornery depicting a solar system with 13 planets, revolving around a very small sun.',[vampire], [ancientBook], [exit(artRoom, visible, unlocked)]).

room(bedroom,'A very large bed with many fine pillows on it commands the middle of the room.  A master wardrobe and sitting area take up the north half of the room.  And a tall vanity mirror stands against the north wall, and you think you can feel a draft coming from somewhere.',[],[],[exit(royalChamber,visible,unlocked),exit(secretStairs,hidden,locked)]).

room(secretStairs,'Strange markings and carvings line both walls of this tight spiral stairwell. All you near the bottom, you encounter impenetrable darkness.',[],[],[exit(bedroom,visible,unlocked),exit(throneRoom,hidden,unlocked)]).

room(throneRoom,'A great chamber surrounded by columns with a great and dirty carpet leading from the double doors to the stage. The throne itself is made of gold and occupied by a rotting corpse covered in blood.',[vampireKing],[],[exit(secretStairs, visible,unlocked),exit(win,hidden,locked)]).

room(pantry,'This room smells terribly. A set of soiled, unmade beds are tucked in the dark corners. You realize what is making you terrible uncomfortable when you notice the bars on the outside of the windows, and locks on the outside of the door.',[],[magicPotion],[exit(kitchen,visible,unlocked)]).

room(kitchen,'Tables line the wall with crude cookware hanging above them. You search the tables for items of use but only find dull knives and rotted food. In the center is a blackened fire pit covered in what must be burnt feathers from some great strange bird.',[],[garlic],[exit(pantry,visible,unlocked), exit(diningHall,visible,unlocked)]).

room(diningHall,'Matching suits of armor line the hall as a tattered red carpet stretches between them.   The last one armor on the left is missing its strangely shaped helmet.  To the north a door leads into a kitchen.  The eastern doorway is shut fast.',[],[],[exit(kitchen,visible,unlocked),exit(weaponRoom,hidden,locked), exit(courtyard,visible,unlocked)]).

room(weaponRoom,'The walls of this room are lined with strangely shaped and sized armor and weapons.  Most of the helmets seem as if they would only fit on a small child, whereas the jagged and crude weapons are clearly too heavy for most grown men.',[],[rustySword],[exit(diningHall,visible,unlocked)]).

room(wineCellar,'The only clue that this was once a wine cellar is the broken bottles in the corner, and large areas with no dust against the wall where racks used to be.',[],[holyWater],[exit(basement,visible,unlocked)]).

room(basement,'There are small footprints in the dusty stone floor. Mold is creeping in between the cracks of the wall and a musty grandfather clock stands in the corner facing the wall.',[],[stone],[exit(eastStairs,visible,unlocked),exit(wineCellar,visible,unlocked),exit(crypt,visible,unlocked),exit(prison,visible,unlocked)]).

room(crypt,'Cobwebs cover everything in this dank room except a small rocking horse placed against the side wall. Unfinished coffins and tombstones litter the room. The southern wall has a large symbol of a crescent moon.',[],[silverKey],[exit(basement,visible,unlocked)]).

room(bigCell,'There was once a fire in this cell that burned all of its contents into an ashy mess in the corner.',[],[dragonKey],[exit(prison,visible,unlocked)]).

room(prison,'This small room has little in it besides a rotten wooden table and chairs.   On the table there is a ring of rusty keys and a stack of crude paper covered in a strange script.  Two cells are present, each with the door closed.  A human skeleton hand is between the bars of the small cell, holding the silver lock with broken fingers.',[],[],[exit(basement,visible,unlocked),exit(bigCell,visible,locked), exit(smallCell,visible,locked)]).

room(smallCell,'It is very cramped and moldy in here, and the prisoner`s skeleton has fallen to the floor into a pile of bones.',[],[gear],[exit(prison,visible,unlocked)]).

%%%Front Entrance%
room(frontEntrance,'You are in the front of a misterious castle, there is a door that you can open.',[],[],[exit(lobby,visible,unlocked)]).

%%%Win%%%%%%%%%%%%
room(win,'Congratulations! You have won the game!!',[],[],[exit(endGame,_,_)]).