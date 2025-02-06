#Clear existing data and graphics
rm(list=ls())
graphics.off()

#Load Hmisc library
#install.packages("Hmisc")
library(Hmisc)

#Read Data
setwd(dir = "/Users/local-margaret/Desktop/R-projects/FL100/")
data=read.csv("data/raw data/CTSC24532USDAWHNRCNu_DATA_2021-06-07_1529.csv")

#Setting Labels
label(data$subject_id)="Participant ID"
label(data$redcap_survey_identifier)="Survey Identifier"
label(data$booknum)="Booklet Number"
label(data$date_ffq)="Date Completed"
label(data$sex_ffq)="Sex"
label(data$pregnant)="Pregnant"
label(data$age_ffq)="Age"
label(data$weight)="Weight (lbs)"
label(data$heightfeet)="Height (ft)"
label(data$heightinches)="Height (Inch)"
label(data$breakfastsandwichfreq)="Breakfast egg sandwich, frequency "
label(data$breakfastsandwichquan)="Breakfast egg sandwich, quantity"
label(data$eggsfreq)="Other eggs, frequency "
label(data$eggsquan)="other eggs, quantity"
label(data$yogurtfreq)="Yogurt, frequency (Not frozen yogurt)"
label(data$yogurtquan)="Yogurt, quantity (NOT frozen yogurt)"
label(data$cottagecheesefreq)="Cottage cheese, ricotta cheese, frequency"
label(data$cottagecheesequan)="Cottage cheese, ricotta cheese, quantity"
label(data$creamcheesefreq)="Cream cheese, frequency"
label(data$creamcheesequan)="Cream cheese, quantity"
label(data$slicedcheesefreq)="Cheese, frequency"
label(data$slicedcheesequan)="Cheese, quantity"
label(data$coldcerealfreq)="Cold cereal, frequency"
label(data$coldcerealquan)="Cold cereal, quantity"
label(data$wholegraincerealfreq)="Oatmeal, whole grain cereal, frequency"
label(data$wholegraincerealquan)="Oatmeal, whole grain cereal, quantity"
label(data$gritsfreq)="Grits, cream of wheat, cornmeal mush, frequency"
label(data$gritsquan)="Grits, cream of wheat, cornmeal mush, quantity"
label(data$milkoncerealfreq)="Milk, or milk substitutes on cereal"
label(data$milkoncerealquan)="Dummy placeholder, not asked on questionnaire"
label(data$brownricefreq)="Brown rice, frequency"
label(data$brownricequan)="Brown rice, quantity"
label(data$whitericefreq)="White rice, frequency"
label(data$whitericequan)="White rice, quantity"
label(data$pancakefreq)="Pancakes, frequency"
label(data$pancakequan)="Pancakes, waffles, quantity"
label(data$pastriesfreq)="Breakfast pastries, frequency"
label(data$pastriesquan)="Breakfast pastries, quantity"
label(data$biscuitfreq)="Biscuits, frequency"
label(data$biscuitquan)="Biscuits, quantity"
label(data$cornbreadfreq)="Cornbread, frequency"
label(data$cornbreadquan)="Cornbread, quantity"
label(data$bunsfreq)="Burger rolls, buns, frequency"
label(data$bunsquan)="Burger rolls, buns, quantity"
label(data$bagelfreq)="Bagels, English muffin, frequency"
label(data$bagelquan)="Bagels, English muffin, quantity"
label(data$tortillasfreq)="Tortillas (flour), frequency"
label(data$tortillasquan)="Flour tortillas, quantity"
label(data$otherbreadsfreq)="Other bread, frequency"
label(data$otherbreadsquan)="Other bread, quantity - How many slices?"
label(data$broccolifreq)="Broccoli, frequency "
label(data$broccoliquan)="Broccoli, quantity"
label(data$carrotsfreq)="Carrots, frequency"
label(data$carrotsquan)="Carrots, quantity"
label(data$cornfreq)="Corn, frequency"
label(data$cornquan)="Corn, quantity"
label(data$greenbeansfreq)="Green beans, frequency"
label(data$greenbeansquan)="Green beans, quantity"
label(data$cookedgreensfreq)="Cooked greens, frequency"
label(data$cookedgreensquan)="Cooked greens, quantity"
label(data$cabbagefreq)="Cabbage, Cole slaw, frequency"
label(data$cabbagequan)="Cabbage, Cole slaw, quantity"
label(data$greensaladfreq)="Green salad, frequency"
label(data$greensaladquan)="Green salad, quantity"
label(data$rawtomatoesfreq)="Tomatoes, frequency"
label(data$rawtomatoesquan)="Tomatoes, quantity"
label(data$saladdressingsfreq)="Salad dressing, frequency"
label(data$saladdressingsquan)="Salad dressing, quantity"
label(data$avocadofreq)="Avocado, guacamole, frequency"
label(data$avocadoquan)="Avocado, guacamole, quantity"
label(data$sweetpotatoesfreq)="Sweet potato, frequency"
label(data$sweetpotatoesquan)="Sweet potato, quantity"
label(data$friesfreq)="French Fries, frequency"
label(data$friesquan)="French Fries, quantity"
label(data$potatoesfreq)="Potato, (not fried), frequency"
label(data$potatoesquan)="Potato, (not fried), quantity"
label(data$otherveggiesfreq)="Other vegetables, frequency"
label(data$otherveggiesquan)="Other vegetables, quantity"
label(data$melonsseasonalfreq)="Melons, in season, frequency"
label(data$melonsseasonalquan)="Melons, in season, quantity"
label(data$berriesseasonalfreq)="Berries, in season, frequency"
label(data$berriesseasonalquan)="Berries, in season, quantity"
label(data$bananasfreq)="Bananas, frequency"
label(data$bananasquan)="Bananas, quantity"
label(data$applesfreq)="Apples, frequency"
label(data$applesquan)="Apples, quantity"
label(data$orangesfreq)="Oranges, frequency"
label(data$orangesquan)="Oranges, quantity"
label(data$peachesfreq)="Peaches, frequency"
label(data$peachesquan)="Peaches, quantity"
label(data$otherfreshfruitfreq)="Any other fresh fruit (grapes, plums, etc.), frequency"
label(data$otherfreshfruitquan)="Any other fresh fruit (grapes, plums, etc.), quantity"
label(data$driedfruitfreq)="Dried fruit, frequency"
label(data$driedfruitquan)="Dried fruit, quantity"
label(data$cannedfruitfreq)="Canned fruit, frequency"
label(data$cannedfruitquan)="Canned fruit, quantity"
label(data$refriedbeansfreq)="Refried beans, frequency"
label(data$refriedbeansquan)="Refried beans, quantity"
label(data$beansfreq)="Other beans, frequency"
label(data$beansquan)="Other beans, quantity"
label(data$tofufreq)="Tofu, frequency"
label(data$tofuquan)="Tofu, quantity"
label(data$meatsubstitutesfreq)="Meat substitutes, frequency"
label(data$meatsubstitutesquan)="Meat substitutes, quantity"
label(data$lentilsoupfreq)="Bean soup, frequency"
label(data$lentilsoupquan)="Bean soup, quantity "
label(data$vegetablesoupfreq)="Vegetable soup, frequency"
label(data$vegetablesoupquan)="Vegetable soup, quantity "
label(data$othersoupfreq)="Other soup, frequency"
label(data$othersoupquan)="Other soup, quantity "
label(data$pizzafreq)="Pizza, frequency"
label(data$pizzaquan)="Pizza, quantity"
label(data$macandcheesefreq)="Mac N Cheese, frequency"
label(data$macandcheesequan)="Mac N Cheese, quantity "
label(data$spaghettifreq)="Spaghetti with meat sauce, frequency"
label(data$spaghettiquan)="Spaghetti with meat sauce, quantity "
label(data$othernoodlesfreq)="Other noodles, frequency"
label(data$othernoodlesquan)="Other noodles, quantity "
label(data$eggrollfreq)="Egg roll, wontons, frequency"
label(data$eggrollquan)="Egg roll, won tons, quantity"
label(data$eatmeat)="Did you ever eat chicken or meat in the past year"
label(data$hamburgerfreq)="Hamburger (cheeseburger), frequency"
label(data$hamburgerquan)="Hamburger (cheeseburger), quantity"
label(data$hotdogfreq)="Hot dogs, frequency"
label(data$hotdogquan)="Hot dogs, quantity, How many?"
label(data$baconsausagefreq)="Bacon or breakfast sausage, frequency"
label(data$baconsausagequan)="Bacon or breakfast sausage, quantity,How many pieces?"
label(data$lunchmeatfreq)="Lunch Meats, frequency"
label(data$lunchmeatquan)="Lunch Meats, quantity- How many slices?"
label(data$meatballsfreq)="Meat loaf, frequency"
label(data$meatballsquan)="Meat loaf, quantity"
label(data$steakfreq)="Beef, frequency"
label(data$steakquan)="Beef, quantity"
label(data$tacofreq)="Tacos, frequency"
label(data$tacoquan)="Tacos, quantity"
label(data$ribsfreq)="Ribs, frequency"
label(data$ribsquan)="Ribs, quantity"
label(data$porkchopsfreq)="Pork, frequency"
label(data$porkchopsquan)="Pork, quantity"
label(data$beefporkdishfreq)="Other beef dish, frequency"
label(data$beefporkdishquan)="Other beef dish, quantity "
label(data$liverfreq)="Liver, frequency"
label(data$liverquan)="Liver, quantity "
label(data$varietymeatfreq)="Pigs feet, variety meats, frequency"
label(data$varietymeatquan)="Pigs feet, variety meats, quantity "
label(data$veallambgamefreq)="Veal, lamb, frequency"
label(data$veallambgamequan)="Veal, lamb, quantity "
label(data$friedorbreadedchickenfreq)="Fried chicken, frequency"
label(data$friedorbreadedchickenquan)="Fried chicken, quantity, How many medium pieces do you eat?"
label(data$roastchickenfreq)="Roast chicken, frequency"
label(data$roastchickenquan)="Roast chicken, quantity"
label(data$otherchickendishfreq)="Other chicken dishes, frequency"
label(data$otherchickendishquan)="Other chicken dishes, quantity ("
label(data$eatfish)="Did you ever eat fish or seafood in the past year?"
label(data$oystersfreq)="Oysters, frequency"
label(data$oystersquan)="Oysters, quantity "
label(data$shellfishfreq)="Shellfish, frequency"
label(data$shellfishquan)="Shellfish, quantity"
label(data$tunafreq)="Tuna, frequency"
label(data$tunaquan)="Tuna, quantity "
label(data$salmonfreq)="Salmon, frequency"
label(data$salmonquan)="Salmon, frequency"
label(data$friedorbreadedfishfreq)="Fried fish, frequency"
label(data$friedorbreadedfishquan)="Fried fish, quantity"
label(data$otherfishfreq)="Other fish, frequency"
label(data$otherfishquan)="Other fish, quantity"
label(data$peanutbutterfreq)="Peanut butter, frequency"
label(data$peanutbutterquan)="Peanut butter, quantity"
label(data$walnutsfreq)="Walnuts, flax seed, frequency"
label(data$walnutsquan)="Walnuts, flax seed, quantity"
label(data$othernutsfreq)="Other nuts, frequency"
label(data$othernutsquan)="Other nuts, quantity"
label(data$proteinbarsfreq)="Protein bars, Energy bars, frequency"
label(data$proteinbarsquan)="Protein bars, Energy bars, quantity"
label(data$cerealbarsfreq)="Breakfast, cereal bars, frequency"
label(data$cerealbarsquan)="Breakfast, cereal bars, quantity"
label(data$popcornfreq)="Popcorn, frequency"
label(data$popcornquan)="Popcorn, quantity"
label(data$wholegraincrackersfreq)="Whole grain crackers, frequency"
label(data$wholegraincrackersquan)="Whole grain crackers, quantity"
label(data$othercrackersfreq)="Other crackers, frequency"
label(data$othercrackersquan)="Other crackers, quantity"
label(data$cornchipsfreq)="Tortilla, corn chips, frequency"
label(data$cornchipsquan)="Tortilla, corn chips, quantity"
label(data$otherchipsfreq)="Other chips, frequency"
label(data$otherchipsquan)="Other chips, quantity"
label(data$donutsfreq)="Donuts, frequency"
label(data$donutsquan)="Donuts, quantity"
label(data$cakesfreq)="Cake, frequency"
label(data$cakesquan)="Cake, quantity, pieces"
label(data$cookiesfreq)="Cookies, frequency"
label(data$cookiesquan)="Cookies, quantity"
label(data$pumpkinpiefreq)="Pumpkin Pie, frequency"
label(data$pumpkinpiequan)="Pumpkin Pie, quantity, How many pieces? "
label(data$otherpiesfreq)="Other Pies, frequency"
label(data$otherpiesquan)="Other Pies, quantity, How many pieces? "
label(data$icecreamfreq)="Ice cream, frozen yogurt, frequency"
label(data$icecreamquan)="Ice cream, frozen yogurt, quantity "
label(data$puddingfreq)="Pudding, frequency"
label(data$puddingquan)="Pudding, quantity "
label(data$sauceicecreamfreq)="Chocolate syrup, frequency"
label(data$sauceicecreamquan)="Chocolate syrup, quantity"
label(data$popsiclesfreq)="Popsicles, jello, frozen fruit bars, frequency"
label(data$popsiclesquan)="Popsicles, jello, frozen fruit bars, quantity"
label(data$chocolatecandyfreq)="Chocolate candy, frequency"
label(data$chocolatecandyquan)="Chocolate candy, quantity"
label(data$othercandiesfreq)="Any other candy (not chocolate), frequency"
label(data$othercandiesquan)="Any other candy (not chocolate), quantity"
label(data$margarinefreq)="Margarine, frequency"
label(data$margarinequan)="Margarine, quantity"
label(data$butterfreq)="Butter, frequency"
label(data$butterquan)="Butter, quantity, pats/teaspoons"
label(data$mayofreq)="Mayonnaise, frequency"
label(data$mayoquan)="Mayonnaise, quantity"
label(data$salsafreq)="Ketchup, salsa frequency"
label(data$salsaquan)="Ketchup, salsa quantity"
label(data$barbecuesaucefreq)="Mustard, BBQ sauce, frequency"
label(data$barbecuesaucequan)="Mustard, BBQ sauce, quantity"
label(data$otherrichsaucesfreq)="Gravy, rich sauces, frequency"
label(data$otherrichsaucesquan)="Gravy, rich sauces, quantity"
label(data$jamfreq)="Jelly, frequency"
label(data$jamquan)="Jelly, quantity"
label(data$picklesfreq)="Pickles, frequency"
label(data$picklesquan)="Pickles, quantity"
label(data$saltfreq)="Salt, frequency"
label(data$saltquan)="Salt, quantity, How many shakes from the salt shaker each day?"
label(data$cocoafreq)="Chocolate milk, cocoa, frequency"
label(data$cocoaquan)="Chocolate milk, cocoa, quantity, 12 oz serving"
label(data$milkfreq)="Milk (default 2%), frequency"
label(data$milkquan)="Milk , quantity  (8 ounce servings)"
label(data$mealreplacementdrinksfreq)="Meal replacement drinks, frequency"
label(data$mealreplacementdrinksquan)="Meal replacement drinks, quantity"
label(data$tomatojuicefreq)="Tomato juice, frequency"
label(data$tomatojuicequan)="Tomato juice, quantity, 8 oz servings"
label(data$orangejuicefreq)="Real orange juice, frequency"
label(data$orangejuicequan)="Real orange juice, quantity, 8 oz servings"
label(data$otherfruitjuicesfreq)="Other real juice, frequency"
label(data$otherfruitjuicesquan)="Other real juice, quantity, 8 oz servings"
label(data$hicfreq)="Hi-C, frequency"
label(data$hicquan)="Hi-C, quantity, 12 oz serving"
label(data$somejuicefreq)="Drinks with some juice, frequency"
label(data$somejuicequan)="Drinks with some juice, quantity, 12 oz servings"
label(data$icedteafreq)="Ice tea, frequency"
label(data$icedteaquan)="Ice tea, quantity, 16 oz glasses or bottles"
label(data$sportsdrinksfreq)="Gatorade, sports drink, frequency"
label(data$sportsdrinksquan)="Gatorade, sports drink, quantity"
label(data$energydrinksfreq)="Energy drinks, frequency"
label(data$energydrinksquan)="Energy drinks, quantity"
label(data$lemonadefreq)="Kool-Aid, lemonade, etc. frequency"
label(data$lemonadequan)="Kool-Aid, lemonade, etc. quantity"
label(data$sodafreq)="Sodas, frequency"
label(data$sodaquan)="Sodas, quantity"
label(data$beerfreq)="Beer, frequency"
label(data$beerquan)="Beer, quantity"
label(data$winefreq)="Wine, frequency"
label(data$winequan)="Wine, quantity"
label(data$cocktailsfreq)="Liquor, frequency"
label(data$cocktailsquan)="Liquor, quantity, How many drinks?"
label(data$waterfreq)="Water, frequency"
label(data$waterquan)="Water, quantity, How many glasses?"
label(data$coffeedrinksfreq)="Milky coffee drinks, lattes, frequency"
label(data$coffeedrinksquan)="Milky coffee drinks, lattes, quantity"
label(data$coffeefreq)="Coffee, frequency"
label(data$coffeequan)="Coffee, quantity, How many do you drink?"
label(data$hotteafreq)="Hot tea, frequency"
label(data$hotteaquan)="Hot tea, quantity,How many cups?"
label(data$coffeedrinkskind)="Type of Milky coffee drinks, lattes"
label(data$coffeedrinkstype)="Milk coffee drinks: Type of milk"
label(data$decafcoffeetype)="Coffee type: Decaf"
label(data$regularcoffeetype)="Coffee type: Regular"
label(data$bothkindscoffeetype)="Coffee type: Both kinds"
label(data$dontdrinkcoffeetype)="Coffee type: Dont drink coffee"
label(data$creamincoffee)="Type of cream in coffee"
label(data$sugarincoffee)="Sugar added to coffee"
label(data$coffeesugarteaspoons)="Sugar added to coffee, quantity, How many teaspoons?"
label(data$decafhotteatype)="Hot tea type: Decaf"
label(data$regularhotteatype)="Hot tea type: Regular"
label(data$bothkindshotteatype)="Hot tea type: Both kinds"
label(data$dontdrinkhotteatype)="Hot tea type: Dont drink tea"
label(data$creamintea)="Type of cream added to tea"
label(data$sugarintea)="Sugar added to tea"
label(data$teasugarteaspoons)="Sugar added to tea, quantity, How many teaspoons?"
label(data$milktype)="What type of milk"
label(data$mealreplacementdrinkstype)="Type of Slim Fast: low-carb, regular"
label(data$orangejuicetype)="Type of real orange juice"
label(data$icedteatype)="Type of tea: home, bottled, sugar, not"
label(data$lemonadetype)="Type of Kool-Aid, lemonade, etc. "
label(data$energydrinkstype)="Type of Energy Drink"
label(data$sodatype)="Type of Soda: Diet/low-calorie, regular"
label(data$sodacaffeine)="Soda Type: Caffeine or Decaffeinated"
label(data$beertype)="Type of Beer"
label(data$winetype)="Type of Wine"
label(data$slicedcheesetype)="Type of Cheese: low-fat, regular"
label(data$yogurtkind)="Kind of yogurt: plain, flavored"
label(data$yogurttype)="Kind of yogurt: low-fat, non-fat, regular"
label(data$saladdressingstype)="Type of Salad Dressing"
label(data$spaghettitype)="Type of Spaghetti or lasagna"
label(data$othernoodlestype)="Type of noodles or pasta"
label(data$hamburgertype)="Type Hamburger: beef, with cheese, turkey"
label(data$fatonmeattype)="How often eat fat on meat (beef or pork)"
label(data$chickenskintype)="How often eat skin on chicken or turkey"
label(data$hotdogtype)="Hot dogs: low-fat/turkey, regular"
label(data$lunchmeattype)="Type of lunch meat: low-fat/turkey, regular"
label(data$cakestype)="Type of cakes: low-sugar, low-fat"
label(data$cookiestype)="Type of Cookies: low-carb, low-fat, reg"
label(data$icecreamtype)="Type of Ice Cream or frozen yogurt"
label(data$proteinbarstype)="Type of protein bars, energy bars "
label(data$bageltype)="Type of Bagel, English muffin"
label(data$bunstype)="Type of buns, rolls"
label(data$otherbreadstype)="Breads, type"
label(data$tortillastype)="Tortillas, type"
label(data$popcorntype)="Type of Popcorn"
label(data$crackerstype)="Type of other crackers"
label(data$mayotype)="Type of mayonnaise"
label(data$allbranorigtype)="Cold cereal: All Bran Original"
label(data$allbrancomptype)="Cold cereal: All Bran Complete, Complete"
label(data$applejackstype)="Cold cereal: Apple Jacks, Cookie Crisp"
label(data$branflakestype)="Cold cereal: Bran Flakes"
label(data$capncrunchtype)="Cold cereal: Capn Crunch"
label(data$cheeriosplaintype)="Cold cereal: Cheerios, plain or Multi-grain"
label(data$cheerioshonnuttype)="Cold cereal: Cheerios, Honey Nut, flavors"
label(data$chexwheattype)="Cold cereal: Chex, Wheat"
label(data$chexothertype)="Cold cereal: Chex, Other"
label(data$cinntoastcrtype)="Cold cereal: Cinnamon Toast Crunch"
label(data$cocoakrispiestype)="Cold cereal: Cocoa Krispies, Pebbles, Puffs"
label(data$cornflakestype)="Cold cereal: Corn Flakes, Corn Puffs"
label(data$cornpopstype)="Cold cereal: Corn Pops"
label(data$fiberonetype)="Cold cereal: Fiber-One, Bran Buds"
label(data$frootloopstype)="Cold cereal: Froot Loops"
label(data$frostedflakestype)="Cold cereal: Frosted Flakes"
label(data$frostedminiwheatstype)="Cold cereal: Frosted Mini-Wheats"
label(data$granolatype)="Cold cereal: Granola"
label(data$grapenutstype)="Cold cereal: Grape Nuts"
label(data$honbunchoatstype)="Cold cereal: Honey Bunches of Oats"
label(data$kashigolnorhr2hrtype)="Cold cereal: Kashi GOLEAN, Heart to Heart"
label(data$lifetype)="Cold cereal: Life"
label(data$luckycharmstype)="Cold cereal: Lucky Charms, Fruity Pebbles"
label(data$oatsquarestype)="Cold cereal: Oatmeal Squares, Oat Bran"
label(data$raisinbrantype)="Cold cereal: Raisin Bran"
label(data$ricekrispiestype)="Cold cereal: Rice Krispies, puffed rice"
label(data$shreddedwheattype)="Cold cereal: Shredded Wheat"
label(data$specialkplaintype)="Cold cereal: Special K, plain"
label(data$specialkflavstype)="Cold cereal: Special K, flavors"
label(data$totaltype)="Cold cereal: Total"
label(data$wheatiestype)="Cold cereal: Wheaties"
label(data$othersweetcerealtype)="Cold cereal: Other sweet cereal"
label(data$otherunsweetcerealtype)="Cold cereal: Other unsweetened cereal"
label(data$otherwholegraincerealtype)="Cold cereal: Other whole grain cereal"
label(data$otherfibercerealtype)="Cold cereal: Other bran or fiber cereal"
label(data$donteatordontknowcerealtype)="Cold cereal: Dont know/eat"
label(data$cookingfatpamornone)="Cooking Fat: Non-stick spray or none"
label(data$cookingfatbutter)="Cooking Fat: Butter or ghee"
label(data$cookingfathalf)="Cooking Fat: Butter/margarine blend"
label(data$cookingfatstickmarg)="Cooking Fat: Stick margarine"
label(data$cookingfatsofttubmarg)="Cooking Fat: Soft tub margarine"
label(data$cookingfatlowfatmarg)="Cooking Fat: Low-fat margarine"
label(data$cookingfatolive)="Cooking Fat: Olive oil"
label(data$cookingfatcanola)="Cooking Fat: Canola or safflower oil"
label(data$cookingfatcorn)="Cooking Fat: Corn, vegetable oil, blends"
label(data$cookingfatpeanut)="Cooking Fat: Peanut oil"
label(data$cookingfatlard)="Cooking Fat: Lard, fatback, bacon fat"
label(data$cookingfatcrisco)="Cooking Fat: Vegetable shortening, Crisco"
label(data$cookingfatother)="Cooking Fat: Other oil"
label(data$cookingfatdontknow)="Cooking Fat: Dont know"
label(data$usevitsregularly)="Mark Yes, if you take vitamin pills at least once a month"
label(data$prenatalvitsamount)="How often Prenatal vitamins"
label(data$prenatalvitsyears)="Number of Years: Prenatal vitamins"
label(data$oneadayamount)="How often One-A-Day type"
label(data$oneadayyears)="Number of Years: One-A-Day"
label(data$bcomplextypevitsamount)="How often Stress-Tabs, B-complex type"
label(data$bcomplextypevitsyears)="Number of Years: Stress-Tabs/B-complex"
label(data$antioxidantcomboamount)="How often antioxidant combinations"
label(data$antioxidantcomboyears)="Number of Years: Antioxidants"
label(data$vitaminaamount)="How often Vitamin A"
label(data$vitaminayears)="Number of Years: Vitamin A"
label(data$vitaminb6amount)="How often Vitamin B6"
label(data$vitaminb6years)="Number of Years: Vitamin B6"
label(data$vitaminb12amount)="How often Vitamin B12?"
label(data$vitaminb12years)="Number of Years: Vitamin B12"
label(data$vitamincamount)="How often Vitamin C"
label(data$vitamincyears)="Number of Years: Vitamin C"
label(data$vitamindamount)="How often Vitamin D"
label(data$vitamindyears)="Number of Years: Vitamin D"
label(data$vitamineamount)="How often Vitamin E"
label(data$vitamineyears)="Number of Years: Vitamin E"
label(data$folicacidamount)="How often Folic Acid"
label(data$folicacidyears)="Number of Years: Folic Acid"
label(data$calciumamount)="How often Calcium"
label(data$calciumyears)="Number of Years: Calcium"
label(data$ironamount)="How often Iron"
label(data$ironyears)="Number of Years: Iron"
label(data$zincamount)="How often Zinc"
label(data$zincyears)="Number of Years: Zinc"
label(data$omegasuppfreq)="How often Omega 3 supplements?"
label(data$omegasuppyears)="Number of Years: Omega 3 supplements"
label(data$fibersuppamount)="How often Fiber supplements?"
label(data$fibersuppyears)="Number of Years: Fiber supplements"
label(data$mineralsyesorno)="Multi-Vitamins, Contain minerals Y/N"
label(data$vitamincquan)="How many mg of Vitamin C do you usually take?"
label(data$vitaminequan)="How many IUs of Vitamin E do you usually take?"
label(data$calciumquan)="How many milligrams of Calcium do you usually take?"
label(data$vitamindquan)="How many IUs of Vitamin D do you usually take?"
label(data$fishoiltype)="Omega 3 Type: Fish Oil"
label(data$flaxhempseedoiltype)="Omega 3 Type: Flax oil, hemp oil, other seed oil"
label(data$krilloiltype)="Omega 3 Type: Krill Oil"
label(data$algaeoiltype)="Omega 3 Type: Algae Oil"
label(data$omega3dontknowtype)="Omega 3 Type: Dont know type"
label(data$veggiesfreq)="How many vegetables eaten per day/week"
label(data$fruitsfreq)="How many fruits eaten per day or week"
label(data$fatoilfreq)="How often use fat/oil in cooking"
label(data$meals)="How many meals per day?"
label(data$snacks)="How many snacks per day?"
label(data$lighthousefreq)="Cooking, shopping, light cleaning, frequency"
label(data$lighthousetime)="Cooking, shopping, light cleaning, quantity"
label(data$slowwalkfreq)="Slow walking, frequency"
label(data$slowwalktime)="Slow walking, quantity"
label(data$jobstandfreq)="Work on the job involving standing or driving,frequency"
label(data$jobstandtime)="Work on the job involving standing or driving, quantity"
label(data$childcarefreq)="Childcare and moderate housework, frequency"
label(data$childcaretime)="Childcare and moderate housework, quantity"
label(data$weedyardfreq)="Weeding, raking, etc. frequency"
label(data$weedyardtime)="Weeding, raking, etc. quantity"
label(data$briskwalkfreq)="Brisk walking, dancing, etc. frequency"
label(data$briskwalktime)="Brisk walking, dancing, etc. quantity"
label(data$jobwalkfreq)="Factory, mechanic, restaurant, walking on job, frequency"
label(data$jobwalktime)="Factory, mechanic, restaurant, walking on job, quantity"
label(data$heavyworkfreq)="Construction, painting, etc. frequency"
label(data$heavyworktime)="Construction, painting, etc. quantity"
label(data$jobliftfreq)="Heavy work, moving boxes, digging, etc. frequency"
label(data$joblifttime)="Heavy work, moving boxes, digging, etc. quantity"
label(data$exergymfreq)="Exercise at the gym, aerobics, jogging, etc. frequency"
label(data$exergymtime)="Exercise at the gym, aerobics, jogging, etc. quantity"
label(data$bikeswimfreq)="Bicycling or swimming, frequency"
label(data$bikeswimtime)="Bicycling or swimming, quantity"
label(data$latino)="Latino"
label(data$white)="White"
label(data$black)="Black or African American"
label(data$asian)="Asian"
label(data$nativeamer)="American indian or Alaska Native"
label(data$hawaiian)="Native Hawaiian or other Pacific Islander"
label(data$notprovided)="Do not wish to provide this information "
label(data$group_solid_count)="Number of solid foods reported on FFQ Note: Does not include beverages and condiments "
label(data$group_solid_total_frequency)="Average daily frequency of solid foods (decimal fraction)"
label(data$group_solid_total_grams)="Grams of solid food average daily, gms"
label(data$group_alcoholicbeverages_total_kcal)="Alcoholic beverages: Average daily calories, kcal"
label(data$group_alcoholicbeverages_total_grams)="Alcoholic beverages: Average daily grams, gms"
label(data$group_alcoholicbeverages_total_frequency)="Alcoholic beverages: Average daily frequency (decimal fraction)"
label(data$group_sugarybevg_total_kcal)="Sugary beverages: Average daily calories, kcal"
label(data$group_sugarybevg_total_grams)="Sugary beverages: Average daily grams, gms"
label(data$group_sugarybevg_total_frequency)="Sugary beverages: Average daily frequency (decimal fraction)"
label(data$group_a_sugbev_total_kcal)="Sugary beverages including fruit juice: Average daily calories, kcal"
label(data$group_a_sugbev_total_frequency)="Sugary beverages including fruit juices: Average daily frequency (decimal fraction)"
label(data$group_sweets_total_kcal)="Sweets group: Average daily calories, kcal                                           Note: Includes sweet beverages (not juice), pancakes, pastries, desserts, protein and cereal bars, jelly, candy, chocolate or flavored milk, sweet coffee drinks, condensed milk and sugar (added to coffee or tea)"
label(data$group_sweets_total_grams)="Sweets group: Average daily grams, gms"
label(data$group_sweets_total_frequency)="Sweets group: Average daily frequency (decimal fraction)"
label(data$dt_kcal)="Food energy, kcals"
label(data$dt_prot)="Protein, gms"
label(data$dt_carb)="Carbohydrate, gms"
label(data$dt_tfat)="Fat, gms"
label(data$dt_alco)="Alcohol (ethanol), mg"
label(data$dt_sug_t)="Sugars, total, gms"
label(data$dt_fibe)="Dietary fiber, gms"
label(data$dt_mois)="Water (moisture) in foods, gms"
label(data$dt_sfat)="Saturated fatty acids (saturated fat), gms"
label(data$dt_mfat)="Monounsaturated fatty acids, gms"
label(data$dt_pfat)="Polyunsaturated fatty acids, gms"
label(data$dt_chol)="Cholesterol, mg"
label(data$dt_s040)="Saturated fat, 4:0, gms (butanoic, butyric)"
label(data$dt_s060)="Saturated fat, 6:0, gms (hexanoic, caproic)"
label(data$dt_s080)="Saturated fat, 8:0, gms (octanoic, caprylic)"
label(data$dt_s100)="Saturated fat, 10:0, gms (decanoic, capric)"
label(data$dt_s120)="Saturated fat, 12:0, gms (dodecanoic, lauric)"
label(data$dt_s140)="Saturated fat, 14:0, gms (tetradecanoic, myristic)"
label(data$dt_s160)="Saturated fat, 16:0, gms (hexadecanoic, palmitic)"
label(data$dt_s180)="Saturated fat, 18:0, gms (octadecanoic, stearic)"
label(data$dt_m161)="Mono-unsaturated fat,16:1 undifferentiated. gms (hexadecenoic, palmitoleic)"
label(data$dt_m181)="Mono-unsaturated fat,18:1 undifferentiated, gms (octadecenoic,oleic)"
label(data$dt_m201)="Mono-unsaturated fat,20:1, gms (eicosenoic, gadoleic)"
label(data$dt_m221)="Mono-unsaturated fat,22:1 undifferentiated, gms (docosenoic, erucic)"
label(data$dt_p182)="Poly-unsaturated fat,18:2 undifferentiated, gms (octadecadienoic, linoleic)"
label(data$dt_p183)="Poly-unsaturated fat,18:3 undifferentiated, gms (octadecatrienoic, linolenic)"
label(data$dt_p184)="Poly-unsaturated fat,18:4, gms (octadecatetraenoic, parinaric)"
label(data$dt_p204)="Poly-unsaturated fat, 20:4 undifferentiated, gms (eicosatetraenoic)"
label(data$dt_p205)="Poly-unsaturated fat, 20:5 (EPA), gms (eicosapentaenoic, timnodonic)"
label(data$dt_p225)="Poly-unsaturated fat, 22:5, gms (docosapentaenoic, clupanodonic)"
label(data$dt_p226)="Poly-unsaturated fat, 22:6 (DHA), gms (docosahexenoic)"
label(data$dt_varae)="Vitamin A (RAE), mcg"
label(data$dt_ret)="Retinol, mcg"
label(data$dt_acaro)="Alpha-carotene, mcg"
label(data$dt_bcaro)="Beta-carotene, mcg"
label(data$dt_crypt)="Cryptoxanthin, beta, mcg"
label(data$dt_lyco)="Lycopene, mcg"
label(data$dt_lutze)="Lutein + Zeaxanthin, mcg"
label(data$dt_atoc)="Vitamin E as alpha-tocopherol, mg"
label(data$dt_atad)="Vitamin E, added (as fortification or enrichment), mg"
label(data$dt_vitd)="Vitamin D (D2 + D3), mcg"
label(data$dt_vitk)="Vitamin K as phylloquinone, mcg"
label(data$dt_vitc)="Vitamin C, mg"
label(data$dt_thia)="Thiamin (Vitamin B1), mg"
label(data$dt_ribo)="Riboflavin (Vitamin B2), mg"
label(data$dt_niac)="Niacin , mg"
label(data$dt_vitb6)="Vitamin B6, mg"
label(data$dt_tfol)="Total folate, mcg"
label(data$fol_dfe)="Folate DFE, mcg"
label(data$dt_folac)="Folic acid, mcg"
label(data$dt_folfd)="Food folate, mcg"
label(data$dt_vb12)="Vitamin B12, mcg"
label(data$dt_b12ad)="Vitamin B12, added (fortification or enrichment), mcg"
label(data$dt_chln)="Total choline, mg"
label(data$dt_calc)="Calcium, mg"
label(data$dt_iron)="Iron, mg"
label(data$dt_magn)="Magnesium, mg"
label(data$dt_phos)="Phosphorus, mg"
label(data$dt_pota)="Potassium, mg"
label(data$dt_sodi)="Sodium, mg"
label(data$dt_zinc)="Zinc,  total, mg"
label(data$dt_copp)="Copper, mg"
label(data$dt_sel)="Selenium, mcg"
label(data$dt_caffn)="Caffeine, mg"
label(data$dt_theo)="Theobromine, mg"
label(data$cyad)="Cyanidin, mg"
label(data$delph)="Delphinidin, mg"
label(data$malvidin)="Malvidin, mg"
label(data$pelargdin)="Pelargonidin, mg"
label(data$peonidin)="Peonidin, mg"
label(data$petunidin)="Petunidin, mg"
label(data$epicatec)="(-)-Epicatechin, mg"
label(data$epicatecg3)="(-)-Epicatechin 3-gallate, mg"
label(data$epicategc)="(-)-Epigallocatechin, mg"
label(data$epicategc3g)="(-)-Epigallocatechin 3-gallate, mg"
label(data$catechin)="(+)-Catechin, mg"
label(data$galcategc)="(+)-Gallocatechin, mg"
label(data$theaflavin)="Theaflavin, mg"
label(data$theaflv33d)="Theaflavin-3,3-digallate, mg"
label(data$theaflv3pg)="Theaflavin-3-gallate, mg"
label(data$theaflv3g)="Theaflavin-3-gallate, mg"
label(data$thearbign)="Thearubigins, mg"
label(data$eriodictyl)="Eriodictyol, mg"
label(data$hespt)="Hesperetin, mg"
label(data$naring)="Naringenin, mg"
label(data$apigen)="Apigenin, mg"
label(data$luteol)="Luteolin, mg"
label(data$isorhmntn)="Isorhamnetin, mg"
label(data$kaemf)="Kaempferol, mg"
label(data$myric)="Myricetin, mg"
label(data$querce)="Quercetin, mg"
label(data$ddzein)="Daiszein, mg"
label(data$gnstein)="Genistein, mg"
label(data$glyctein)="Glycitein, mg"
label(data$t_anthocyadns)="Total anthocyanidins, mg SUM(Cyad, Petunidin, Delph, Malvidin, Pelargdin, Peonidin) "
label(data$t_flavan3ols)="Total flavan-3-ols, mg SUM(Catechin, Epicategc, Epicatec, Epicatecg3, Epicategc3g,    Theaflavin, Thearbign, Theaflv33d, Theaflv3pg, Theaflv3g, Galcategc) "
label(data$t_flavanones)="Total flavanones, mg SUM(Eriodictyl,Hespt, Naring) "
label(data$t_flavones)="Total flavones, mg SUM(Apigen, Luteol) "
label(data$t_flavonols)="Total flavonols, mg SUM(Isorhmntn, Kaemf, Myric, Querce) "
label(data$t_isoflavones)="Total isoflavones, mg SUM(Ddzein, Gnstein, Glyctein) "
label(data$t_flavonoids)="Total flavonoids, mg SUM(Ddzein, Gnstein, Glyctein,  Cyad, Petunidin, Delph, Malvidin, Pelargdin, Peonidin, Catechin, Epicategc, Epicatec, Epicatecg3, Epicategc3g, Theaflavin, Thearbign, Theaflv33d, Theaflv3pg, Theaflv3g, Galcategc, Eriodictyl,Hespt, Naring, Apigen, Luteol, Isorhmntn, Kaemf, Myric, Querce) "
label(data$gi)="Glycemic Index (glucose), average daily"
label(data$gl)="Glycemic Load (glucose), average daily"
label(data$f_total)="Fruit: Total fruit, cup equivalents (cup eq.)   Total intact fruits (whole or cut) and fruit juices Note: include fruit and juice in drinks and recipes "
label(data$f_citmlb)="Fruit: Citrus, melons, berries (not juice), cup eq. Intact fruits (whole or cut) of citrus, melons, and berries "
label(data$f_other)="Fruit: Other (not juice), cup eq. Intact fruits (whole or cut); excluding citrus, melons, and berries "
label(data$f_juice)="Fruit juice, cup eq. Fruit juices, citrus and non citrus Note: include juice in drinks and recipes "
label(data$f_whole)="Fruit: Whole fruit (not juice), cup eq. (F_TOTAL  F_JUICE) "
label(data$v_total)="Vegetables: Total vegetables, cup equivalents (cup eq.) Total dark green, red and orange, starchy, and other vegetables; excludes legumes "
label(data$v_drkgr)="Vegetables: Dark green, cup eq. Dark green vegetables "
label(data$v_redor_total)="Vegetables: Red-orange, total, cup eq. Total red and orange vegetables (tomatoes and tomato products + other red and orange vegetables) "
label(data$v_redor_tomato)="Vegetables: Red-orange, tomato, cup eq. Tomatoes and tomato products "
label(data$v_redor_other)="Vegetables: Red-orange, other, cup eq. Other red and orange vegetables, excluding tomatoes and tomato products "
label(data$v_starchy_total)="Vegetables: Starchy, total, cup eq. Total starchy vegetables (white potatoes + other starchy vegetables) Note: Includes potatoes and other starchy vegetables "
label(data$v_starchy_potato)="Vegetables: Starchy, potato, cup eq. White potatoes "
label(data$v_starchy_other)="Vegetables: Starchy, other, cup eq. Other starchy vegetables, excluding white potatoes "
label(data$v_other)="Vegetables: Other vegetables, cup eq. Other vegetables not in the vegetable components listed above "
label(data$v_legumes)="Vegetables: Legumes, cup eq. Beans and peas (legumes) computed as vegetables Beans and soy products are also given in Protein foods units (ounce-equivalents) "
label(data$g_total)="Grain: Total grain, ounce-equivalents (oz. eq.) Total whole and refined grains Note: Includes whole and refined grain, including grains and flours, in baked goods and recipes. "
label(data$g_whole)="Grain: Whole grains, oz. eq. Grains defined as whole grains and contain the entire grain kernel ? the bran, germ, and endosperm "
label(data$g_refined)="Grains: Refined grains, oz. eq. Refined grains that do not contain all of the components of the entire grain kernel "
label(data$pf_total)="Protein foods: Total protein foods, ounce-equivalents Note: an ounce-equivalent of protein foods approximates the protein in one ounce of very lean meat. Total meat, poultry, organ meat, cured meat, seafood, eggs, soy, and nuts and seeds; excludes legumes. "
label(data$pf_mps_total)="Protein foods: Total meat, poultry, seafood, oz. eq. Total of meat, poultry, seafood, organ meat, and cured meat "
label(data$pf_meat)="Protein foods: Meat (red meat), oz. eq. Beef, veal, pork, lamb, and game meat; excludes organ meat and cured meat "
label(data$pf_curedmeat)="Protein foods: Cured meats, oz. eq. Frankfurters, sausages, corned beef, and luncheon meat that are made from beef, pork, or poultry "
label(data$pf_organ)="Protein foods: Organ meats, oz.eq. Organ meat from beef, veal, pork, lamb, game, and poultry (oz. eq.) "
label(data$pf_poult)="Protein foods: Poultry, oz.eq. Chicken, turkey, Cornish hens, duck, goose, quail, and pheasant (game birds); excludes organ meat and cured meat "
label(data$pf_seafd_hi)="Protein foods: Seafood (finfish, shellfish, and other seafood) high in n-3 fatty acids, oz. eq"
label(data$pf_seafd_low)="Protein foods: Seafood (finfish, shellfish, and other seafood) low in n-3 fatty acids, oz.eq."
label(data$pf_eggs)="Protein foods: Eggs, oz. eq.  (1 egg is an oz.eq.) Eggs (chicken, duck, goose, quail) and egg substitutes "
label(data$pf_soy)="Protein foods: Soy products, oz.eq. Soy products, excluding calcium fortified soy milk and mature soybeans "
label(data$pf_nutsds)="Protein foods: Nuts and seeds, oz.eq. Peanuts, tree nuts, and seeds; excludes coconut "
label(data$pf_legumes)="Protein foods: Legumes, oz.eq. Beans and Peas (legumes) computed as protein foods "
label(data$d_total)="Dairy: Total dairy, cup equivalents (cup eq.) Total milk, yogurt, cheese, and whey. For some foods, the total dairy values could be higher than the sum of D_MILK, D_YOGURT, and D_CHEESE because Miscellaneous dairy component composed of whey which is not included in FPED as a separate variable. "
label(data$d_milk)="Dairy: Milk, cup eq. Fluid milk, buttermilk, evaporated milk, dry milk, and calcium fortified soy milk "
label(data$d_yogurt)="Dairy: Yogurt, cup eq."
label(data$d_cheese)="Dairy: Cheese, cup eq."
label(data$oils)="Oils, grams Fats naturally present in nuts, seeds, and seafood; unhydrogentated vegetable oils, except palm oil, palm kernel oil, and coconut oils; fat present in avocado and olives above the allowable amount; 50% of fat present in stick and tub margarines and margarine spreads "
label(data$solid_fats)="Solid fats, grams Fats naturally present in meat, poultry, eggs, and dairy (lard, tallow, and butter); hydrogenated or partially hydrogenated oils; shortening, palm, palm kernel and coconut oils; fats naturally present in coconut meat and cocoa butter; and 50% of fat present in stick and tub margarines and margarine spreads "
label(data$add_sugars)="Added sugars, teaspoon equivalents (tsp.eq.)"
label(data$a_drinks)="Alcoholic drinks, number of drinks Alcoholic beverages and alcohol (ethanol) added to foods after cooking "
label(data$d_soy)="HEI precursor: Milk substitutes (calcium fortified) made from soy, cup eq."
label(data$m_soy_nd)="HEI precursor: Soy products without (calcium fortified) milk substitutes made from soy, oz. eq."
label(data$adsug_na)="HEI precursor: Added sugars, excluding sugars in alcoholic beverages, tsp. eq."
label(data$oils_m)="Modified USDA FPED food group for HEI-2010: Oils (modified), grams where all fats in margarines are categorized as solid fats and thus counted toward empty calories component."
label(data$solid_fats_m)="Modified USDA FPED food group for HEI-2010: Solid fats (modified), grams where all fats in margarines are categorized as solid fats and thus counted toward empty calories component."
label(data$a_bev_kc)="HEI precursor: Alcoholic beverages calories, kcal"
label(data$aa_bev)="AHEI-2010 precursor: Alcoholic beverages, servings (svgs.)"
label(data$av_tot_s)="AHEI-2010 precursor: Average daily vegetables, svgs."
label(data$af_tot_s)="AHEI-2010 precursor: Average daily fruit (not juice), svgs."
label(data$group_a_sugbev_total_grams)="AHEI-2010 precursor: Average daily sugary beverages (including juice), grams (to be converted to servings)"
label(data$r_meat_s)="AHEI-2010 precursor: Average daily red meats, svgs."
label(data$a_nut_s)="AHEI-2010 precursor: Average daily nuts, svgs."
label(data$a_bean_s)="AHEI-2010 precursor: Average daily beans, svgs."
label(data$dha_epa)="AHEI-2010 precursor: Average daily intake of DHA and EPA, grams"
label(data$adj_pufa)="AHEI-2010 precursor: Average daily intake of remaining poly-unsaturated fatty acids, without DHA and EPA, grams"
label(data$dt_trfat)="AHEI-2010 precursor: Trans fat, total, grams"
label(data$pctalch)="Percent of calories from alcoholic beverages"
label(data$psgtot)="My Plate: Grain (total), oz. eq."
label(data$psgwhl)="My Plate: Grain, whole, oz. eq."
label(data$psvegnbp)="My Plate: Vegetables _Total (no legumes or potatoes), cup eq."
label(data$psvegdkg)="My Plate: Vegetables - Dark green, cup eq."
label(data$psvegorn)="My Plate: Vegetables - Orange, cup eq."
label(data$psvegoth)="My Plate: Vegetables, other, cup eq."
label(data$psvegpot)="My Plate: Vegetables, potatoes, cup eq."
label(data$psfruit)="My Plate: Fruit  Total including juices, cup eq."
label(data$psdairy)="My Plate: Dairy - Total milk, cheese, yogurt, cup eq."
label(data$psmfp)="My Plate: Protein foods - Meat, fish (seafood), poultry, oz. eq."
label(data$psnutsd)="My Plate: Protein foods - Nuts and seeds, oz. eq."
label(data$pseggs)="My Plate: Protein foods - Eggs, oz. eq."
label(data$pslegsoy)="My Plate: Protein foods  Legumes and soy, oz. eq."
label(data$psoils)="My Plate: Beneficial oils, tsp. eq. Note: Includes oils and fats in dressings, fish, nuts, avocados "
label(data$sup_vita)="From supplements: Vitamin A RAE, mcg"
label(data$sup_lz)="From supplements: Lutein + Zeaxanthin, mcg"
label(data$sup_vitd)="From supplements: Vitamin D, mcg"
label(data$sup_vite)="From supplements: Vitamin E, mg"
label(data$sup_vk)="From supplements: Vitamin K, mcg"
label(data$sup_b1)="From supplements: Vitamin B-1 (Thiamin), mg"
label(data$sup_b2)="From supplements: Vitamin B-2 (Riboflavin), mg"
label(data$sup_niac)="From supplements: Vitamin B-3 (Niacin), mg"
label(data$sup_b6)="From supplements: Vitamin B-6 (Pyridoxine), mg"
label(data$sup_fol)="From supplements: Folate, mcg"
label(data$sup_b12)="From supplements: Vitamin B-12 (Cobalamin), mcg"
label(data$sup_vitc)="From supplements: Vitamin C, mg"
label(data$sup_ca)="From supplements: Calcium, mg"
label(data$sup_cu)="From supplements: Copper, mg"
label(data$sup_iron)="From supplements: Iron, mg"
label(data$sup_mg)="From supplements: Magnesium, mg"
label(data$sup_se)="From supplements: Selenium, mcg"
label(data$sup_zinc)="From supplements: Zinc, mg"
label(data$sup_ala)="From supplements: Alpha-linolenic acid (ALA), mg"
label(data$sup_oleic)="From supplements: Oleic acid, mg"
label(data$sup_om_3)="From supplements: Omega-3 fatty acids, mg Note: Includes ALA, DHA and EPA "
label(data$sup_om_6)="From supplements: Omega-6 fatty acids, mg"
label(data$sup_epa)="From supplements: Eicosapentaenoic acid (EPA), mg"
label(data$sup_dha)="From supplements: Docosahexaenoic acid (DHA), mg"
label(data$sup_fiber)="From supplements: Fiber, grams"
label(data$kcal_expenditure_all)="Estimated energy expenditure (EE), all activities, kcals."
label(data$kcal_expenditure_recr)="Estimated recreational EE, excluding chores and work, kcals."
label(data$lowmins)="Estimated light activity minutes per day"
label(data$modmins)="Estimated moderate activity minutes per day"
label(data$vigmins)="Estimated vigorous activity minutes per day"
label(data$recrmins)="Estimated minutes per day, excluding chores and job: NOTE: Recreational activities in this screener include Brisk walking, Exercise, and Biking, swimming, etc. "
label(data$metmins)="Estimated per day, MET minutes from all activities on questionnaire "
label(data$metminrecr)="Estimated per day, MET minutes from recreational activities on questionnaire"
label(data$group_breakfast_sandwich_with_egg_or_meat_total_grams)="Breakfast sandwiches or breakfast burritos with eggs or meat (g/day)"
label(data$group_other_eggs_or_omelets_total_grams)="Other eggs like scrambled or boiled, or quiche (not egg substitutes) (g/day)"
label(data$group_yogurt_total_grams)="Yogurt (not frozen yogurt), population mix (g/day)"
label(data$group_yogurt_plain_low_fat_total_grams)="Yogurt, Plain, Low-fat (g/day)  "
label(data$group_yogurt_plain_non_fat_total_grams)="Yogurt, Plain, Non-fat (g/day)"
label(data$group_yogurt_plain_full_fat_total_grams)="Yogurt, Plain, Full-fat (whole milk) (g/day)"
label(data$group_yogurt_sweet_low_fat_total_grams)="Yogurt, Fruit or flavors, Low-fat (g/day)"
label(data$group_yogurt_sweet_non_fat_total_grams)="Yogurt, Fruit or flavors, Non-fat (g/day)"
label(data$group_yogurt_sweet_full_fat_total_grams)="Yogurt, Fruit or flavors, Full-fat (whole milk) (g/day)"
label(data$group_cottage_cheese_ricotta_total_grams)="Cottage cheese, ricotta cheese (g/day)"
label(data$group_cream_cheese_sour_cream_dip_total_grams)="Cream cheese, sour cream, dips (g/day)"
label(data$group_cheese_total_grams)="Cheese, sliced cheese, cheese spread, including in sandwiches and quesadillas, population mix (g/day)"
label(data$group_cheese_low_fat_total_grams)="Cheese, sliced cheese, cheese spread, Low-fat (g/day)"
label(data$group_cheese_full_fat_total_grams)="Cheese, sliced cheese, cheese spread, Regular-fat (g/day)"
label(data$group_all_bran_original_total_grams)="Cold cereal: All Bran Original (g/day)"
label(data$group_all_bran_complete_complete_total_grams)="Cold cereal: All Bran Complete, Complete (g/day)"
label(data$group_apple_jacks_cookie_crisp_total_grams)="Cold cereal: Apple Jacks, Cookie Crisp (g/day)"
label(data$group_bran_flakes_total_grams)="Cold cereal: Bran Flakes (g/day)"
label(data$group_cap_n_crunch_total_grams)="Cold cereal: Capn Crunch (g/day)"
label(data$group_cheerios_plain_or_multi_grain_total_grams)="Cold cereal: Cheerios, plain or Multi-Grain (g/day)"
label(data$group_cheerios_honey_nut_flavors_total_grams)="Cold cereal: Cheerios, Honey Nut, flavors (g/day)"
label(data$group_chex_wheat_total_grams)="Cold cereal: Chex, Wheat (g/day)"
label(data$group_chex_other_total_grams)="Cold cereal: Chex, other (g/day)"
label(data$group_cinnamon_toast_crunch_total_grams)="Cold cereal: Cinnamon Toast Crunch (g/day)"
label(data$group_cocoa_krispies_pebbles_puffs_total_grams)="Cold cereal: Cocoa Krispies, Pebbles, Puffs (g/day)"
label(data$group_corn_flakes_corn_puffs_total_grams)="Cold cereal: Corn Flakes, Corn Puffs (g/day)"
label(data$group_corn_pops_total_grams)="Cold cereal: Corn Pops (g/day)"
label(data$group_fiber_one_bran_buds_total_grams)="Cold cereal: Fiber One, Bran Buds (g/day)"
label(data$group_froot_loops_total_grams)="Cold cereal: Froot Loops (g/day)"
label(data$group_frosted_flakes_total_grams)="Cold cereal: Frosted flakes (g/day)"
label(data$group_frosted_mini_wheats_total_grams)="Cold cereal: Frosted Mini-Wheats (g/day)"
label(data$group_granola_total_grams)="Cold cereal: Granola (g/day)"
label(data$group_grape_nuts_total_grams)="Cold cereal: Grape Nuts (g/day)"
label(data$group_honey_bunches_of_oats_total_grams)="Cold cereal: Honey Bunches of Oats (g/day)"
label(data$group_kashi_golean_heart_2_heart_total_grams)="Cold cereal: Kashi GOLEAN, Heart to Heart (g/day)"
label(data$group_life_total_grams)="Cold cereal: Life (g/day)"
label(data$group_lucky_charms_fruity_pebbles_total_grams)="Cold cereal: Lucky Charms, Fruity Pebbles (g/day)"
label(data$group_oatmeal_squares_oat_bran_total_grams)="Cold cereal: Oatmeal Squares, Oat Bran (g/day)"
label(data$group_raisin_bran_total_grams)="Cold cereal: Raisin Bran (g/day)"
label(data$group_rice_krispies_puffed_rice_total_grams)="Cold cereal: Rice Krispies, puffed rice (g/day)"
label(data$group_shredded_wheat_total_grams)="Cold cereal: Shredded Wheat (g/day)"
label(data$group_special_k_plain_total_grams)="Cold cereal: Special K, plain (g/day)"
label(data$group_special_k_flavors_total_grams)="Cold cereal: Special K, flavors (g/day)"
label(data$group_total_product_19_total_grams)="Cold cereal: Total (g/day)"
label(data$group_wheaties_total_grams)="Cold cereal: Wheaties (g/day)"
label(data$group_other_not_wholegrain_sweet_cereal_total_grams)="Cold cereal: Other sweet cereal (not whole grain) (g/day)"
label(data$group_other_not_wholegrain_unsweetened_cereal_total_grams)="Cold cereal: Other unsweetened cereal, (not whole grain) (g/day)"
label(data$group_other_whole_grain_cereal_total_grams)="Cold cereal: Other whole grain cereal (g/day)"
label(data$group_other_fiber_cereal_total_grams)="Cold cereal: Other bran or fiber cereal (g/day)"
label(data$group_dont_eat_cereal_total_grams)="Cold cereal: Dont eat cereal (g/day)"
label(data$group_oatmeal_wholegrain_cooked_cereal_total_grams)="Oatmeal, or whole grain cereal like Wheatena or Ralston (g/day)"
label(data$group_grits_cream_of_wheat_mush_total_grams)="Grits, cream of wheat, cornmeal mush (g/day)"
label(data$group_brown_rice_dishes_with_total_grams)="Brown Rice, or dishes made with brown rice (g/day)"
label(data$group_white_rice_dishes_with_total_grams)="White rice, or dishes made with rice, like rice and beans (g/day)"
label(data$group_pancakes_waffles_french_toast_crepes_with_syrup_total_grams)="Pancakes, waffles, French toast, crepes (g/day)"
label(data$group_breakfast_pastry_muffins_quick_bread_total_grams)="Breakfast pastries, like muffins, scones, sweet rolls, Danish, Pop Tarts, pan dulce (g/day)"
label(data$group_biscuits_scones_croissants_total_grams)="Biscuits, not counting breakfast sandwiches (g/day)"
label(data$group_corn_bread_hush_puppies_total_grams)="Corn bread, corn muffins, hush puppies (g/day)"
label(data$group_sandwich_buns_total_grams)="Hamburger buns, hotdog buns, submarine or hoagie buns (g/day)"
label(data$group_sandwich_buns_white_total_grams)="Burger, hotdog, submarine buns: White (g/day)"
label(data$group_sandwich_buns_multigrain_total_grams)="Burger, hotdog, submarine buns: Multi-grain (g/day)"
label(data$group_sandwich_buns_wholegrain_total_grams)="Burger, hotdog, submarine buns: 100% whole wheat (g/day)"
label(data$group_sandwich_buns_mix_total_grams)="Burger, hotdog, submarine buns: Eat all kinds (g/day)"
label(data$group_bagels_eng_muffin_pita_total_grams)="Bagels or English muffins, dinner rolls, pita, naan (g/day)"
label(data$group_bagels_white_total_grams)="Bagels, English muffins, rolls: White (g/day)"
label(data$group_bagels_multigrain_total_grams)="Bagels, English muffins, rolls: Multi-grain (g/day)"
label(data$group_bagels_wholegrain_total_grams)="Bagels, English muffins, rolls: 100% whle wheat (g/day)"
label(data$group_bagels_mix_total_grams)="Bagels, English muffins, rolls: Eat all kinds (g/day)"
label(data$group_tortillas_total_grams)="Tortillas (not counting in tacos or burritos) (g/day)"
label(data$group_tortillas_corn_total_grams)="Tortillas: Corn tortillas (g/day)"
label(data$group_tortillas_flour_wheat_total_grams)="Tortillas: Flour tortillas (wheat) (g/day)"
label(data$group_tortilla_mix_total_grams)="Tortillas: Eat all kinds or dont know (g/day)"
label(data$group_bread_dinner_rolls_total_grams)="Any other bread or toast, including white, dark, whole wheat, and what you have in sandwiches (g/day)"
label(data$group_bread_white_total_grams)="Bread: White (not whole grain) (g/day)"
label(data$group_bread_multi_grain_total_grams)="Bread: Multi-grain, rye or other brown bread (g/day)"
label(data$group_bread_whole_grain_total_grams)="Bread: 100% whole wheat (g/day)"
label(data$group_bread_mix_total_grams)="Bread: Eat some of each (g/day)"
label(data$group_broccoli_brussel_sprouts_chinese_broccoli_total_grams)="Broccoli, Chinese broccoli, or Brussels sprouts (g/day)"
label(data$group_carrots_carrot_mixes_total_grams)="Carrots, and mixed vegetables  containing carrots (g/day)"
label(data$group_corn_total_grams)="Corn (g/day)"
label(data$group_green_beans_peas_total_grams)="Green beans, string beans, green peas (g/day)"
label(data$group_greens_spinach_cooked_total_grams)="Cooked greens like spinach, collards, turnip greens, kale, mustard greens (g/day)"
label(data$group_cabbage_slaw_chinese_cabbages_total_grams)="Cabbage, cole slaw, Chinese cabbage (g/day)"
label(data$group_green_salad_raw_spinach_total_grams)="Green salad with lettuce or raw spinach (g/day)"
label(data$group_raw_tomatoes_total_grams)="Raw tomatoes (g/day)"
label(data$group_salad_dressing_total_grams)="Salad dressing (g/day)"
label(data$group_salad_dressing_low_fat_total_grams)="Salad dressing: Low-fat, lite (g/day)"
label(data$group_salad_dressing_fat_free_total_grams)="Salad dressing: Fat-free (g/day)"
label(data$group_salad_dressing_regular_total_grams)="Salad dressing: Regular (g/day)"
label(data$group_salad_dressing_oil_and_vinegar_total_grams)="Salad dressing: Oil & vinegar (g/day)"
label(data$group_avocado_guacamole_total_grams)="Avocado, guacamole (g/day)"
label(data$group_sweet_potato_yam_total_grams)="Sweet potatoes, yams (g/day)"
label(data$group_french_fries_fried_potatoes_total_grams)="French fries, home fries, hash browns, tater tots (g/day)"
label(data$group_white_potatoes_not_fried_total_grams)="Potatoes not fried, like baked, boiled, mashed, or in stew or potato salad (g/day)"
label(data$group_any_other_vegetable_total_grams)="Any other vegetable like squash, cauliflower, peppers, okra, nopales (g/day)"
label(data$group_melons_in_season_total_grams)="Watermelon, cantaloupe, honeydew, other melons, in season (g/day)"
label(data$group_strawberries_berries_in_season_total_grams)="Strawberries or other berries, in season (g/day)"
label(data$group_bananas_total_grams)="Bananas (g/day)"
label(data$group_apples_pears_raw_total_grams)="Apples or pears (g/day)"
label(data$group_orange_tangerine_grapefruit_fresh_total_grams)="Oranges, tangerines, grapefruit (g/day)"
label(data$group_peach_nectarine_total_grams)="Peaches and nectarines (g/day)"
label(data$group_other_fresh_fruit_fruit_salad_total_grams)="Any other fresh fruit like grapes, plums, mango, fruit salad (g/day)"
label(data$group_raisins_dried_fruit_total_grams)="Raisins, dates, other dried fruit (g/day)"
label(data$group_canned_fruit_applesauce_with_canned_citrus_total_grams)="Canned fruit like applesauce, fruit cocktail, canned peaches or pineapple (g/day)"
label(data$group_refried_beans_hummus_total_grams)="Refried beans, bean burritos, or hummus (g/day)"
label(data$group_other_beans_lentil_chili_not_rice_and_beans_total_grams)="Pinto beans, black beans, kidney beans, baked beans, lentils (g/day)"
label(data$group_tofu_or_tempeh_total_grams)="Tofu or tempeh (g/day)"
label(data$group_meat_substitutes_vegetable_meats_total_grams)="Meat substitutes, like veggie burgers, veggie chicken, vegetarian hot dogs or vegetarian lunch meats (g/day)"
label(data$group_bean_split_pea_lentil_soup_total_grams)="Split pea, bean, or lentil soup (g/day)"
label(data$group_vegetable_soup_total_grams)="Vegetable soup, vegetable beef soup, or tomato soup (g/day)"
label(data$group_other_soup_total_grams)="Any other soup including chicken noodle, cream soups, Cup-A-Soup, ramen (g/day)"
label(data$group_pizza_pizza_pockets_total_grams)="Pizza or pizza pockets (g/day)"
label(data$group_mac_and_cheese_cheese_dishes_total_grams)="Macaroni and cheese (g/day)"
label(data$group_spaghetti_pasta_with_tomato_sauce_total_grams)="Spaghetti, lasagna, other pasta with tomato sauce (g/day)"
label(data$group_spaghetti_meatless_total_grams)="Spaghetti or lasagna: Meatless (g/day)"
label(data$group_spaghetti_with_meat_total_grams)="Spaghetti or lasagna: Withmeat sauce or meatballs (g/day)"
label(data$group_other_noodles_pasta_sopa_seca_total_grams)="Other noodles like plain pasta, pasta salad, sopa seca (g/day)"
label(data$group_other_noodles_white_pasta_total_grams)="Noodles, pasta: Rarely whole grain (g/day)"
label(data$group_other_noodles_pasta_mix_total_grams)="Noodles, pasta: Sometimes whole grain (g/day)"
label(data$group_other_noodles_whole_grain_total_grams)="Noodles, pasta: Usually whole grain (g/day)"
label(data$group_egg_rolls_wantons_dumplings_samosas_total_grams)="Egg rolls, won tons, samosas, empanadas (g/day)"
label(data$group_burgers_ground_meats_total_grams)="Hamburgers, cheeseburgers, turkey burger, at home or from a restaurant (g/day)"
label(data$group_hamburger_patty_total_grams)="Burgers: Hamburger patty (g/day)"
label(data$group_cheeseburger_meat_and_cheese_total_grams)="Burgers: Cheeseburger (meat and cheese) (g/day)"
label(data$group_turkey_burger_the_meat_total_grams)="Burgers: Turkey burger patty (g/day)"
label(data$group_hot_dog_dinner_sausage_total_grams)="Hot dogs, or dinner sausage like Polish, Italian, chicken apple (g/day)"
label(data$group_hot_dog_beef_or_pork_total_grams)="Hot dogs, dinner sausage:  Beef or pork (g/day)"
label(data$group_hot_dog_poultry_low_fat_total_grams)="Hot dogs, dinner sausage:  Chicken or turkey, low-fat (g/day)"
label(data$group_sausage_bacon_total_grams)="Bacon or breakfast sausage (g/day)"
label(data$group_lunch_meats_total_grams)="Lunch meats like bologna, sliced ham, sliced turkey, salami (g/day)"
label(data$group_lunch_meats_beef_or_pork_total_grams)="Lunch meats: Beef or pork (g/day)"
label(data$group_lunch_meats_poultry_low_fat_total_grams)="Lunch meats: Chicken or turkey, low-fat (g/day)"
label(data$group_meat_loaf_meat_balls_total_grams)="Meat loaf, meat balls (g/day)"
label(data$group_steak_roast_total_grams)="Steak, roast beef,  pot roast, including in frozen dinners or sandwiches (g/day)   Beef or pork (beef): Sometimes eat the fat (g/day) "
label(data$group_steak_roast_fat_off_total_grams)="Beef or pork (beef): Avoid eating the fat (g/day)"
label(data$group_steak_roast_fat_on_total_grams)="Beef or pork (beef): Often eat the fat (g/day)"
label(data$group_tacos_burritos_enchiladas_with_meat_total_grams)="Tacos, burritos, enchiladas, tamales, tostadas, with meat or chicken (g/day)"
label(data$group_ribs_spareribs_bbq_total_grams)="Ribs, spareribs (g/day)"
label(data$group_pork_chops_roast_ham_total_grams)="Pork chops, pork roasts, cooked ham  (including for breakfast) (g/day) Beef or pork (pork): Sometimes eat the fat (g/day)  (population mix) "
label(data$group_pork_fat_off_total_grams)="Beef or pork (pork): Avoid eating the fat (g/day)"
label(data$group_pork_fat_on_total_grams)="Beef or pork (pork): Often eat the fat (g/day)"
label(data$group_mixed_dish_with_beef_pork_total_grams)="Any other beef or pork dish like stew, pot pie, corned beef  hash, chili, Hamburger Helper, curry (g/day)"
label(data$group_liver_liverwurst_total_grams)="Liver, including chicken livers or liverwurst (g/day)"
label(data$group_feet_neck_tail_tongue_chitlins_total_grams)="Pigs feet, neck bones, oxtails, tongue, chitlins (g/day)"
label(data$group_veal_lamb_goat_game_total_grams)="Veal, lamb, goat, deer meat, other game (g/day)"
label(data$group_fried_or_coated_chicken_turkey_total_grams)="Fried chicken, including chicken fingers, chicken nuggets, wings, chicken patty (g/day)"
label(data$group_fried_or_coated_chix_no_skin_total_grams)="Chicken or turkey (fried or coated): Avoid eating the skin (g/day)"
label(data$group_fried_or_coated_chix_ate_skin_total_grams)="Chicken or turkey (fried or coated): Often eat the skin (g/day)"
label(data$group_fried_or_coated_chix_ate_skin_sometimes_total_grams)="Chicken or turkey (fried or coated): Sometimes eat the skin (g/day)"
label(data$group_poultry_uncoated_total_grams)="Roasted or broiled chicken or turkey (g/day)"
label(data$group_poultry_uncoated_no_skin_total_grams)="Chicken or turkey (uncoated): Avoid eating the skin (g/day)"
label(data$group_poultry_uncoated_ate_skin_total_grams)="Chicken or turkey (uncoated): Often eat the skin (g/day)"
label(data$group_poultry_uncoated_ate_skin_sometimes_total_grams)="Chicken or turkey (uncoated): Sometimes eat the skin (g/day)"
label(data$group_chicken_or_turkey_mixed_dish_total_grams)="Any other chicken or turkey dish, like chicken stew or curry, chicken salad, stir-fry, Chinese chicken dishes (g/day)"
label(data$group_oysters_total_grams)="Oysters (g/day)"
label(data$group_shellfish_except_oysters_total_grams)="Shellfish like shrimp, scallops, crab (g/day)"
label(data$group_tuna_tuna_in_dishes_total_grams)="Tuna, tuna salad, tuna casserole (g/day)"
label(data$group_high_omega3_fish_total_grams)="Salmon, mackerel, sea bass, trout, sardines, herring, without breading (g/day)"
label(data$group_fried_fish_fish_sticks_sandwich_breaded_fillets_total_grams)="Fried fish, fish sticks, fish sandwich, breaded fillets (g/day)"
label(data$group_other_fish_dishes_low_omega3_total_grams)="Any other fish  (g/day)"
label(data$group_peanut_butter_nut_butter_total_grams)="Peanut butter or other nut butters (g/day)"
label(data$group_walnuts_flax_seeds_total_grams)="Walnuts or flax seeds (dont count flax seed oil) (g/day)"
label(data$group_peanuts_other_nuts_seeds_total_grams)="Peanuts, sunflower seeds, other nuts or seeds (g/day)"
label(data$group_protein_energy_bars_total_grams)="Energy or protein bars like Power Bar, Clif, Balance, Luna, South Beach, Atkins (g/day) Energy or protein bars:  Some of each (g/day)(population mix) "
label(data$group_high_energy_bar_total_grams)="Energy or protein bars:  High energy (g/day)"
label(data$group_high_protein_bar_total_grams)="Energy or protein bars:  High protein (g/day)"
label(data$group_cereal_granola_bars_total_grams)="Breakfast bars, cereal bars, granola bars (not energy or protein bars) (g/day)"
label(data$group_popcorn_total_grams)="Popcorn (g/day)"
label(data$group_popcorn_air_pop_fat_free_total_grams)="Popcorn: Air popped, fat free (g/day)"
label(data$group_popcorn_low_fat_light_total_grams)="Popcorn: Low-fat or Light (g/day)"
label(data$group_popcorn_regular_total_grams)="Popcorn: Regular (g/day)"
label(data$group_popcorn_caramel_total_grams)="Popcorn: Caramel corn (g/day)"
label(data$group_whole_grain_crackers_total_grams)="Whole grain crackers, like Wheat Thins, RyeKrisp, Ryvita, Wasa (g/day)"
label(data$group_whole_grain_crackers_low_fat_total_grams)="Crackers, pretzels (whole grain): Low-fat, including Rye Crisp, rice cakes, or plain pretzels (g/day)"
label(data$group_whole_grain_crackers_regular_total_grams)="Crackers, pretzels (whole grain): Regular fat cracker or filled pretzels (g/day)"
label(data$group_other_crackers_pretzels_not_wholegrain_total_grams)="Any other crackers, like saltines, Ritz, Cheez-Its, cheese-filled pretzels"
label(data$group_other_crackers_pretzels_low_fat_total_grams)="Crackers, pretzels (not whole grain): Low-fat, including Rye Crisp, rice cakes, or plain pretzels (g/day)"
label(data$group_other_crackers_filled_pretzels_regular_total_grams)="Crackers, pretzels (not whole grain): Regular fat cracker or filled pretzels (g/day)"
label(data$group_tortilla_or_corn_chips_corn_nuts_total_grams)="Tortilla chips or corn chips, like Fritos, Doritos, corn nuts (g/day)"
label(data$group_corn_puffs_twists_soy_potato_chips_total_grams)="Any other snack chips, like potato chips, Cheetos, Chex mix (g/day)"
label(data$group_donuts_total_grams)="Donuts (g/day)"
label(data$group_cake_cupcakes_total_grams)="Cake or snack cakes like cupcakes, Twinkies, pound cake, banana bread (g/day)"
label(data$group_cake_low_sugar_total_grams)="Cakes, snack cakes, cupcakes: Low-sugar, low-carb (g/day)"
label(data$group_cake_low_fat_total_grams)="Cakes, snack cakes, cupcakes: Low-fat  (g/day)"
label(data$group_cake_regular_total_grams)="Cakes, snack cakes, cupcakes: Regular-fat (g/day)"
label(data$group_cookies_brownies_total_grams)="Cookies, brownies (g/day)"
label(data$group_cookies_low_sugar_total_grams)="Cookies, brownies: Low-sugar, low-carb (g/day)"
label(data$group_cookies_low_fat_total_grams)="Cookies, brownies: Low-fat (g/day)"
label(data$group_cookies_regular_total_grams)="Cookies, brownies: Regular-fat (g/day)"
label(data$group_pumpkin_sweet_potato_pie_total_grams)="Pumpkin pie, sweet potato pie (g/day)"
label(data$group_other_pie_or_cobbler_total_grams)="Any other pie or cobbler, including fast food pies, snack pies (g/day)"
label(data$group_ice_cream_frozen_yogurt_total_grams)="Ice cream, ice cream bars, frozen yogurt, fast food milkshakes (g/day)"
label(data$group_ice_cream_low_sugar_total_grams)="Ice cream, frozen yogurt: Low-sugar, low-carb (g/day)"
label(data$group_ice_cream_frozen_yogurt_low_fat_total_grams)="Ice cream, frozen yogurt: Low-fat or frozen yogurt (g/day)"
label(data$group_ice_cream_regular_total_grams)="Ice cream, frozen yogurt: Regular (g/day)"
label(data$group_pudding_custard_flan_total_grams)="Pudding, custard, rice pudding, flan (g/day)"
label(data$group_chocolate_sauce_toppings_total_grams)="Chocolate or other flavored sauces or syrup, on ice cream (g/day)"
label(data$group_popsicles_sherbet_ices_jello_total_grams)="Popsicles, Jello, frozen fruit bars, Slushies, sherbet (dont count sugar-free) (g/day)"
label(data$group_chocolate_candy_total_grams)="Chocolate candy, candy bars like Snickers, Hersheys,  M&Ms (g/day)"
label(data$group_candy_not_chocolate_total_grams)="Any other candy, not chocolate, like hard candy, Lifesavers, Skittles, Starburst, breath mints, chewing gum (NOT sugar free) (g/day)"
label(data$group_margarine_at_table_total_grams)="Margarine (not butter) on bread, rice, vegetables, or other foods (g/day)"
label(data$group_butter_at_table_total_grams)="Butter (not margarine) on bread, rice, vegetables, or other foods (g/day)"
label(data$group_mayo_sandwich_spread_total_grams)="Mayonnaise, sandwich spreads (g/day)"
label(data$group_mayo_light_low_fat_total_grams)="Mayonnaise or sandwich spreads: Low-fat, light (g/day)"
label(data$group_mayo_regular_total_grams)="Mayonnaise or sandwich spreads:  Regular (g/day)"
label(data$group_ketchup_salsa_total_grams)="Ketchup, salsa, chili sauce, chili peppers (g/day)"
label(data$group_mustard_bbq_sauce_soy_sauce_etc_total_grams)="Mustard, barbecue sauce, soy sauce (g/day)"
label(data$group_gravy_rich_sauce_peanut_sauce_mole_total_grams)="Gravy, or other rich sauces like Alfredo, white sauce, mole, peanut sauce (g/day)"
label(data$group_jam_jelly_marmalade_total_grams)="Jam, jelly, marmalade (g/day)"
label(data$group_pickles_sauerkraut_kimchi_total_grams)="Pickles, picked vegetables, sauerkraut, kimchi (g/day)"
label(data$group_table_salt_total_grams)="Salt, added to your food at the table (g/day)"
label(data$group_chocolate_milk_cocoa_hot_chocolate_total_grams)="Chocolate milk, cocoa, hot chocolate (g/day)"
label(data$group_milk_and_milk_substitutes_total_grams)="Glasses of milk or soy milk,  not counting on cereal, in coffee, or chocolate milk (g/day)"
label(data$group_whole_milk_4_pct_fat_total_grams)="Milk: Whole milk (g/day)"
label(data$group_reduced_fat_2_pct_milk_total_grams)="Milk: 2% milk (g/day)"
label(data$group_low_fat_1_pct_milk_total_grams)="Milk: 1% milk (g/day)"
label(data$group_non_fat_skim_milk_total_grams)="Milk: Skim milk (g/day)"
label(data$group_soy_milk_total_grams)="Milk: Soy milk (g/day)"
label(data$group_rice_milk_total_grams)="Milk: Rice milk (g/day)"
label(data$group_other_milk_almond_total_grams)="Milk: Almond milk, other (g/day)"
label(data$group_meal_drinks_protein_drinks_total_grams)="Meal replacement drinks like Slim Fast, Ensure, or high protein drinks or powders (g/day)"
label(data$group_slim_fast_type_regular_total_grams)="Slim Fast, Ensure, or high protein drinks: Slim Fast, Ensure, regular (g/day)"
label(data$group_slim_fast_type_low_carb_total_grams)="Slim Fast, Ensure, or high protein drinks: Slim Fast, Ensure, light or low-carb (g/day)"
label(data$group_high_protein_drinks_regular_total_grams)="Slim Fast, Ensure, or high protein drinks: High protein drinks, regular (g/day)"
label(data$group_high_protein_drinks_low_carb_total_grams)="Slim Fast, Ensure, or high protein drinks: High protein drinks, light or low-carb (g/day)"
label(data$group_tomato_vegetable_juice_total_grams)="Tomato juice, V8, other vegetable  juice (g/day)"
label(data$group_orange_grapefruit_juice_total_grams)="Real 100% orange juice or grapefruit juice. Dont count orange soda or Sunny Delight (g/day)  Real 100% orange juice or grapefruit juice: Dont know (if fortified (g/day)"
label(data$group_oj_calcium_fortified_total_grams)="Real 100% orange juice or grapefruit juice: Calcium-fortified (g/day)"
label(data$group_oj_grapefruit_juice_not_calcium_fortified_total_grams)="Real 100% orange juice or grapefruit juice: Not calcium-fortified (g/day)"
label(data$group_other_100_pct_juice_and_blends_total_grams)="Other 100% juices, like apple, grape, 100% fruit blends, or fruit smoothies (g/day)"
label(data$group_hi_c_cranberry_juice_tang_total_grams)="Hi-C, cranberry juice cocktail, Hawaiian Punch, Tang (g/day)"
label(data$group_drinks_with_some_juice_total_grams)="Drinks with some juice like Sunny Delight, Knudsen (g/day)"
label(data$group_iced_tea_all_kinds_total_grams)="Iced tea, homemade, instant or bottled, like Nestea, Lipton,  Snapple, Tazo (g/day)"
label(data$group_ice_tea_home_no_sugar_total_grams)="Iced tea: Homemade, no sugar (g/day)"
label(data$group_ice_tea_home_sugar_total_grams)="Iced tea: Homemade, regular (g/day)"
label(data$group_ice_tea_bottle_no_sugar_total_grams)="Iced tea: Bottled, no sugar (g/day) (g/day)"
label(data$group_ice_tea_bottle_sugar_total_grams)="Iced tea: Bottled, regular, (g/day)"
label(data$group_gatorade_powerade_total_grams)="Gatorade, PowerAde, or other sports drinks (g/day)"
label(data$group_energy_drinks_total_grams)="Energy drinks like Red Bull, Rockstar, Monster: Regular (g/day)"
label(data$group_energy_drinks_low_sugar_total_grams)="Energy drinks like Red Bull, Rockstar, Monster: Low- sugar (g/day)"
label(data$group_energy_drinks_sugar_total_grams)="Energy drinks: Regular (g/day)"
label(data$group_koolaid_horchata_total_grams)="Kool-Aid, lemonade, fruit-flavored drinks, like Crystal Light, atole, horchata (not iced tea) (g/day)"
label(data$group_low_cal_koolaid_total_grams)="Kool-Aid, lemonade: Low calorie (g/day)"
label(data$group_regular_koolaid_total_grams)="Kool-Aid, lemonade: Regular (g/day)"
label(data$group_soda_or_pop_total_grams)="Soft drinks, soda, pop, like cola, 7-Up, orange soda, regular or diet (g/day)"
label(data$group_soda_diet_no_caffeine_total_grams)="Soft drinks: decaf (g/day)"
label(data$group_soda_diet_caffeine_total_grams)="Soft drinks: diet, regular caffeine (g/day)"
label(data$group_soda_sugar_no_caffeine_total_grams)="Soft drinks: Sugar-free, decaf (g/day)"
label(data$group_soda_sugar_caffeine_total_grams)="Soft drinks: Regular (g/day)"
label(data$group_soda_diet_unsure_caffeine_total_grams)="Soft drinks: Diet, dont know caffeine type (g/day)"
label(data$group_soda_sugar_unsure_caffeine_total_grams)="Soft drink: Regular sugar, dont know caffeine type /day)"
label(data$group_soda_decaf_unsure_sugar_total_grams)="Soft drinks: Decaf, dont know sugar type. (g/day)"
label(data$group_soda_caffeine_unsure_sugar_total_grams)="Soft drinks: Regular caffeine, dont know sugar type. (g/day)"
label(data$group_beer_any_kind_total_grams)="Beer or non-alcoholic beer (g/day)"
label(data$group_beer_regular_total_grams)="Beer: Regular beer (g/day)"
label(data$group_beer_light_low_carb_total_grams)="Beer: Light or Low-carb(g/day)"
label(data$group_beer_non_alcoholic_total_grams)="Beer: Non-alcoholic (g/day)"
label(data$group_wine_wine_coolers_all_kinds_total_grams)="Wine or wine coolers (g/day)"
label(data$group_wine_red_total_grams)="Wine or wine coolers: Red (g/day)"
label(data$group_wine_white_total_grams)="Wine or wine coolers: White (g/day)"
label(data$group_both_red_and_white_wine_total_grams)="Wine: Both red and white wine (g/day)"
label(data$group_liquor_cocktails_total_grams)="Liquor or mixed drinks, cocktails (g/day)"
label(data$group_water_bottled_or_tap_total_grams)="Water, bottled or tap (g/day)"
label(data$group_milky_coffee_drink_any_kind_total_grams)="Milky coffee drinks like latte, mocha, cappuccino, Frappuccino (g/day)"
label(data$group_latte_cappuccino_1_pct_or_2_pct_milk_total_grams)="Latte or cappuccino: 1% or 2% milk (g/day)"
label(data$group_latte_cappuccino_whole_milk_total_grams)="Latte or cappuccino: Whole milk (g/day)"
label(data$group_latte_cappuccino_non_fat_milk_total_grams)="Latte or cappuccino: Non-fat milk (g/day)"
label(data$group_latte_cappuccino_soy_milk_total_grams)="Latte or cappuccino: Soy milk (g/day)"
label(data$group_latte_cappuccino_something_else_total_grams)="Latte or cappuccino: Something else type milk (g/day)"
label(data$group_cafe_leche_1_pct_or_2_pct_milk_total_grams)="Caf?on leche: 1% or 2% milk (g/day)"
label(data$group_cafe_leche_whole_milk_total_grams)="Caf?on leche: Whole milk (g/day)"
label(data$group_cafe_leche_non_fat_milk_total_grams)="Caf?on leche: Non-fat milk (g/day)"
label(data$group_cafe_leche_soy_milk_total_grams)="Caf?on leche: Soy milk (g/day)"
label(data$group_cafe_leche_something_else_total_grams)="Caf?on leche: Something else type milk (g/day)"
label(data$group_mocha_1_pct_or_2_pct_milk_total_grams)="Mocha: 1% or 2% milk (g/day)"
label(data$group_mocha_whole_milk_total_grams)="Mocha: Whole milk (g/day)"
label(data$group_mocha_non_fat_milk_total_grams)="Mocha: Non-fat milk (g/day)"
label(data$group_mocha_soy_milk_total_grams)="Mocha: Soy milk (g/day)"
label(data$group_mocha_something_else_total_grams)="Mocha: Something else type milk (g/day)"
label(data$group_frappuccino_1_pct_or_2_pct_milk_total_grams)="Frappuccino: 1% or 2% milk (g/day)"
label(data$group_frappuccino_whole_milk_total_grams)="Frappuccino: Whole milk (g/day)"
label(data$group_frappuccino_non_fat_milk_total_grams)="Frappuccino: Non-fat milk (g/day)"
label(data$group_frappuccino_soy_milk_total_grams)="Frappuccino: Soy milk (g/day)"
label(data$group_frappuccino_something_else_total_grams)="Frappuccino: Something else type milk (g/day)"
label(data$group_coffee_decaf_total_grams)="Coffee: Decaf (g/day)"
label(data$group_coffee_caffeine_total_grams)="Coffee: Regular (g/day)"
label(data$group_coffee_both_kinds_total_grams)="Coffee: Both kinds (decaf and regular) (g/day)"
label(data$group_coffee_dont_drink_total_grams)="Coffee: Dont drink (g/day)"
label(data$group_hot_tea_decaf_total_grams)="Hot tea: Decaf (g/day)"
label(data$group_hot_tea_caffeine_total_grams)="Hot tea: Regular (g/day)"
label(data$group_hot_tea_both_kinds_total_grams)="Hot tea: Both kinds (reg. or decaf) (g/day)"
label(data$group_hot_tea_dont_drink_total_grams)="Hot tea: Dont drink (g/day)"
label(data$group_cream_or_half_n_half_total_grams)="Added to coffee or tea: Cream or half-n-half (g/day)"
label(data$group_non_dairy_creamer_liquid_total_grams)="Added to coffee or tea: Non-dairy creamer (g/day)"
label(data$group_condensed_milk_total_grams)="Added to coffee or tea: Condensed milk (g/day)"
label(data$group_sugar_or_honey_total_grams)="Added coffee or tea: Sugar or honey,  (g/day)"
label(data$group_cooking_fat_pop_mix_total_grams)="Fats or oils are used most often for cooking or frying (not baking) in your home, population mix (g/day)"
label(data$group_non_stick_spray_sr27_total_grams)="Fats/oils used in cooking: Non-stick spray (g/day)"
label(data$group_cook_fat_butter_or_ghee_total_grams)="Fats/oils used in cooking: Butter or ghee (g/day)"
label(data$group_cook_fat_butter_margarine_blend_total_grams)="Fats/oils used in cooking: Butter/margarine blend (g/day)"
label(data$group_cook_fat_margarine_stick_total_grams)="Fats/oils used in cooking: Stick margarine (g/day)"
label(data$group_cook_fat_margarine_tub_total_grams)="Fats/oils used in cooking: Tub margarine (g/day)"
label(data$group_cook_fat_margarine_low_fat_total_grams)="Fats/oils used in cooking: Low-fat margarine (g/day)"
label(data$group_cook_fat_olive_oil_total_grams)="Fats/oils used in cooking: Olive oil g/day)"
label(data$group_cook_fat_canola_safflower_oils_total_grams)="Fats/oils used in cooking: Canola, safflower oil (g/day)"
label(data$group_cook_fat_corn_vegetable_oil_blends_total_grams)="Fats/oils used in cooking: Corn oil, vegetable blends (g/day)"
label(data$group_cook_fat_peanut_oil_total_grams)="Fats/oils used in cooking: Peanut oil used in (g/day)"
label(data$group_cook_fat_animal_fat_total_grams)="Fats/oils used in cooking: Lard, fatback, bacon fat (g/day)"
label(data$group_cook_fat_veg_shortening_crisco_total_grams)="Fats/oils used in cooking: Vegetable shortening, Crisco (g/day)"
label(data$group_cook_fat_other_oil_coconut_various_nfs_vegetable_oils_total_grams)="Fats/oils used in cooking: Other oil (g/day)"
label(data$ash)="Ash, grams"
label(data$sucs)="Sucrose, grams"
label(data$glus)="Glucose (dextrose), grams"
label(data$frus)="Fructose, grams"
label(data$lacs)="Lactose, grams"
label(data$mals)="Maltose, grams"
label(data$gals)="Galactose, grams"
label(data$starch)="Starch, grams"
label(data$mn)="Manganese, mg"
label(data$fld)="Fluoride, mcg"
label(data$niacin_equiv_ne)="Niacin equivalents, NE mg                                                           mg NE = mg Niacin + (1000 * g Tryptophan)/ 60"
label(data$pantac)="Pantothenic acid, mg"
label(data$b_carotene_equiv)="Beta_carotene equivalents, mcg                                                             mcg Bcar-eqiv = mcg beta-carotene + .5 (mcg alpha-carotene +  mcg beta-cryptoxantin)"
label(data$vita_iu)="Vitamin A, IU IU vitamin A = (mcg retinol/ .3) + (mcg beta-carotene equivalents/ .6) "
label(data$tocphb)="Tocopherol, beta, mg"
label(data$tocphg)="Tocopherol, gamma, mg"
label(data$tocphd)="Tocopherol, delta, mg"
label(data$toctra)="Tocotrienol, alpha, mg"
label(data$toctrb)="Tocotrienol, beta, mg"
label(data$toctrg)="Tocotrienol, gamma, mg"
label(data$toctrd)="Tocotrienol, delta, mg"
label(data$ergcal)="Vitamin D2 (ergocalciferol), mcg"
label(data$chocal)="Vitamin D3 (cholecalciferol), mcg"
label(data$vitd_iu)="Vitamin D, IU                                                                                                                  IU vitamin D = 40 * mcg vitamin D    [ 0.025 mcg vitamin D = 1 IU ]"
label(data$vitk1d)="Dihydrophylloquinone, mcg"
label(data$mk4)="Menaquinone-4, mcg"
label(data$f13d0)="Saturated fat, 13:0, grams (tridecanoic)"
label(data$f15d0)="Saturated fat, 15:0, grams (pentadecanoic)"
label(data$f17d0)="Saturated fat, 17:0, grams (heptadecanoic)"
label(data$f20d0)="Saturated fat, 20:0, grams (eicosanoic, arachidic)"
label(data$f22d0)="Saturated fat, 22:0, grams (docosanoic, behenic)"
label(data$f24d0)="Saturated fat, 24:0, grams (tetracosanoic, lignoceric)"
label(data$f14d1)="Mono-unsaturated fat, 14:1, grams (tetradecenoic, myristoleic)"
label(data$f15d1)="Mono-unsaturated fat, 15:1, grams (pentadecenoic)"
label(data$f16d1c)="Mono-unsaturated fat, 16:1 c, grams (cis-hexadecenoic)"
label(data$f17d1)="Mono-unsaturated fat, 17:1, grams (heptadecenoic)"
label(data$f18d1c)="Mono-unsaturated fat, 18:1 c, grams (cis-octadecenoic)"
label(data$f22d1c)="Mono-unsaturated fat, 22:1 c, grams (cis-docosenoic)"
label(data$f24d1c)="Mono-unsaturated fat, 24:1 c, grams (cis-tetracosenoic, nervonic)"
label(data$f18d2cn6)="Poly-unsaturated fat, 18:2 c N-6, grams (cisoctadecadienoic N-6)"
label(data$f18d2cla)="Poly-unsaturated fat, 18:2 CLA (conjugated linoleic acid), grams"
label(data$f18d2i)="Poly-unsaturated fat, 18:2 i, grams (mixed isomers)"
label(data$f18d3cn3)="Poly-unsaturated fat, 18:3 c N-3, grams (cis-cis-cis-octadecatrienoic N-3, alpha-linolenic)"
label(data$f18d3cn6)="Poly-unsaturated fat, 18:3 c N-6, grams (cis-cis-cis-octadecatrienoic N-6, gamma-linolenic)"
label(data$f18d3i)="Poly-unsaturated fat, 18:3 i, grams (mixed isomers)"
label(data$f20d2cn6)="Poly-unsaturated fat, 20:2 c N-6, grams (cic-cis-eicosadienoic)"
label(data$f20d3)="Poly-unsaturated fat, 20:3 undifferentiated, grams (eicosatrienoic)"
label(data$f20d3n3)="Poly-unsaturated fat, 20:3 N-3, grams (eicosatrienoic N-3)"
label(data$f20d3n6)="Poly-unsaturated fat, 20:3 N-6, grams (eicosatrienoic N-6)"
label(data$f20d4n6)="Poly-unsaturated fat, 20:4 N-6, grams (eicosatetraenoic N-6, arachidonic)"
label(data$f21d5)="Poly-unsaturated fat, 21:5, grams"
label(data$f22d4)="Poly-unsaturated fat, 22:4, grams"
label(data$f16d1t)="Trans fat, mono-unsaturated fat, 16:1 t, grams (trans-hexadecenoic)"
label(data$f18d1t)="Trans fat, mono-unsaturated fat, 18:1 t, grams (trans-octadecenoic)"
label(data$f18d1tn7)="Trans fat, mono-unsaturated fat, 18:1 t N-7, grams (trans-octadecenoic N-7)"
label(data$f22d1t)="Trans fat, mono-unsaturated fat, 22:1 t, grams (trans- docosenoic)"
label(data$f18d2tt)="Trans fat, poly-unsaturated fat, 18:2 tt, grams (trans-trans-octadecadienoic)"
label(data$f18d2t)="Trans fat, poly-unsaturated fat, 18:2 t, grams (trans- octadecadienoic)"
label(data$fatrnm)="Fatty acids, total trans-monoenoic, grams"
label(data$fatrnp)="Fatty acids, total trans-polyenoic, grams"
label(data$phystr)="Phytosterols, mg"
label(data$stid7)="Stigmasterol, mg"
label(data$camd5)="Campesterol, mg"
label(data$sitstr)="Beta-sitosterol, mg"
label(data$trp_g)="Tryptophan, grams"
label(data$thr_g)="Threonine, grams"
label(data$ile_g)="Isoleucine, grams"
label(data$leu_g)="Leucine, grams"
label(data$lys_g)="Lysine, grams"
label(data$met_g)="Methionine, grams"
label(data$cys_g)="Cystine, grams"
label(data$phe_g)="Phenylalanine, grams"
label(data$tyr_g)="Tyrosine, grams"
label(data$val_g)="Valine, grams"
label(data$arg_g)="Arginine, grams"
label(data$histn_g)="Histidine, grams"
label(data$ala_g)="Alanine, grams"
label(data$asp_g)="Aspartic acid, grams"
label(data$glu_g)="Glutamic acid, grams"
label(data$gly_g)="Glycine, grams"
label(data$pro_g)="Proline, grams"
label(data$ser_g)="Serine, grams"
label(data$hyp)="Hydroxyproline, grams"
label(data$pac_1)="Proanthocyanidin monomers, mg"
label(data$pac_2)="Proanthocyanidin dimers, mg"
label(data$pac_3)="Proanthocyanidin trimers, mg"
label(data$pac_4)="Proanthocyanidin 4-6mers, mg"
label(data$pac_7)="Proanthocyanidin 7-10mers, mg"
label(data$pac10)="Proanthocyanidin polymers (>10mers), mg"
label(data$betn_c)="Betaine, mg"
label(data$cholnfr)="Free Choline, mg"
label(data$cholngpc)="Choline from glycerophoshocholine, mg"
label(data$cholnpc)="Choline from phosphocholine, mg"
label(data$cholnptc)="Choline from phosphatidylcholine, mg"
label(data$cholnsm)="Choline from sphingomyelin, mg"
label(data$dt_fiber_insol)="Dietary fiber, insoluble, grams"
label(data$dt_fiber_sol)="Dietary fiber, soluble, grams"
label(data$dt_prot_animal)="Protein from animal sources, grams"
label(data$dt_prot_vegetable)="Protein from vegetable sources, grams"
label(data$dt_nitrogen)="Nitrogen, grams"
label(data$phytic_acid)="Phytic acid, mg"
label(data$oxalic_acid)="Oxalic acid, mg"
label(data$coumestrol)="Coumestrol, mg"
label(data$biochanin_a)="Biochanin A, mg"
label(data$formononetin)="Formononetin, mg"
label(data$ffq_comments)="Comments:"
label(data$never_responses)="Number of Never responses"
label(data$ffq_qaqc_kcalok)="Kcal Outlier Evaluation"
label(data$ffq_qaqc_foodsperday)="Assessment of Number of Solid Foods reported (excludes beverages and condiments) according to NutritionQuest recommended QAQC"
label(data$ffq_qc_passed)="FFQ Quality Check Passed?"
label(data$block_ffq_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$sex_ffq.factor = factor(data$sex_ffq,levels=c("1","2"))
data$pregnant.factor = factor(data$pregnant,levels=c("1","2","3","M"))
data$breakfastsandwichfreq.factor = factor(data$breakfastsandwichfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$breakfastsandwichquan.factor = factor(data$breakfastsandwichquan,levels=c("1","2","M"))
data$eggsfreq.factor = factor(data$eggsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$eggsquan.factor = factor(data$eggsquan,levels=c("1","2","3","4","M"))
data$yogurtfreq.factor = factor(data$yogurtfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$yogurtquan.factor = factor(data$yogurtquan,levels=c("2","3","4","M"))
data$cottagecheesefreq.factor = factor(data$cottagecheesefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cottagecheesequan.factor = factor(data$cottagecheesequan,levels=c("1","2","3","4","M"))
data$creamcheesefreq.factor = factor(data$creamcheesefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$creamcheesequan.factor = factor(data$creamcheesequan,levels=c("1","2","3","4","M"))
data$slicedcheesefreq.factor = factor(data$slicedcheesefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$slicedcheesequan.factor = factor(data$slicedcheesequan,levels=c("1","2","3","4","M"))
data$coldcerealfreq.factor = factor(data$coldcerealfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$coldcerealquan.factor = factor(data$coldcerealquan,levels=c("2","3","4","M"))
data$wholegraincerealfreq.factor = factor(data$wholegraincerealfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$wholegraincerealquan.factor = factor(data$wholegraincerealquan,levels=c("1","2","3","4","M"))
data$gritsfreq.factor = factor(data$gritsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$gritsquan.factor = factor(data$gritsquan,levels=c("1","2","3","4","M"))
data$milkoncerealfreq.factor = factor(data$milkoncerealfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$milkoncerealquan.factor = factor(data$milkoncerealquan,levels=c("2"))
data$brownricefreq.factor = factor(data$brownricefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$brownricequan.factor = factor(data$brownricequan,levels=c("2","3","4","M"))
data$whitericefreq.factor = factor(data$whitericefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$whitericequan.factor = factor(data$whitericequan,levels=c("2","3","4","M"))
data$pancakefreq.factor = factor(data$pancakefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$pancakequan.factor = factor(data$pancakequan,levels=c("1","2","3","4","M"))
data$pastriesfreq.factor = factor(data$pastriesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$pastriesquan.factor = factor(data$pastriesquan,levels=c("1","2","3","4","M"))
data$biscuitfreq.factor = factor(data$biscuitfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$biscuitquan.factor = factor(data$biscuitquan,levels=c("1","2","3","4","M"))
data$cornbreadfreq.factor = factor(data$cornbreadfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cornbreadquan.factor = factor(data$cornbreadquan,levels=c("1","2","3","4","M"))
data$bunsfreq.factor = factor(data$bunsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$bunsquan.factor = factor(data$bunsquan,levels=c("1","2","3","4","M"))
data$bagelfreq.factor = factor(data$bagelfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$bagelquan.factor = factor(data$bagelquan,levels=c("1","2","3","4","M"))
data$tortillasfreq.factor = factor(data$tortillasfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$tortillasquan.factor = factor(data$tortillasquan,levels=c("1","2","3","4","M"))
data$otherbreadsfreq.factor = factor(data$otherbreadsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$otherbreadsquan.factor = factor(data$otherbreadsquan,levels=c("1","2","3","4","M"))
data$broccolifreq.factor = factor(data$broccolifreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$broccoliquan.factor = factor(data$broccoliquan,levels=c("1","2","3","4","M"))
data$carrotsfreq.factor = factor(data$carrotsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$carrotsquan.factor = factor(data$carrotsquan,levels=c("1","2","3","4","M"))
data$cornfreq.factor = factor(data$cornfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cornquan.factor = factor(data$cornquan,levels=c("1","2","3","4","M"))
data$greenbeansfreq.factor = factor(data$greenbeansfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$greenbeansquan.factor = factor(data$greenbeansquan,levels=c("1","2","3","4","M"))
data$cookedgreensfreq.factor = factor(data$cookedgreensfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cookedgreensquan.factor = factor(data$cookedgreensquan,levels=c("1","2","3","4","M"))
data$cabbagefreq.factor = factor(data$cabbagefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cabbagequan.factor = factor(data$cabbagequan,levels=c("1","2","3","4","M"))
data$greensaladfreq.factor = factor(data$greensaladfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$greensaladquan.factor = factor(data$greensaladquan,levels=c("1","2","3","4","M"))
data$rawtomatoesfreq.factor = factor(data$rawtomatoesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$rawtomatoesquan.factor = factor(data$rawtomatoesquan,levels=c("1","2","3","4","M"))
data$saladdressingsfreq.factor = factor(data$saladdressingsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$saladdressingsquan.factor = factor(data$saladdressingsquan,levels=c("1","2","3","4","M"))
data$avocadofreq.factor = factor(data$avocadofreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$avocadoquan.factor = factor(data$avocadoquan,levels=c("1","2","3","4","M"))
data$sweetpotatoesfreq.factor = factor(data$sweetpotatoesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$sweetpotatoesquan.factor = factor(data$sweetpotatoesquan,levels=c("1","2","3","4","M"))
data$friesfreq.factor = factor(data$friesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$friesquan.factor = factor(data$friesquan,levels=c("1","2","3","4","M"))
data$potatoesfreq.factor = factor(data$potatoesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$potatoesquan.factor = factor(data$potatoesquan,levels=c("1","2","3","4","M"))
data$otherveggiesfreq.factor = factor(data$otherveggiesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$otherveggiesquan.factor = factor(data$otherveggiesquan,levels=c("1","2","3","4","M"))
data$melonsseasonalfreq.factor = factor(data$melonsseasonalfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$melonsseasonalquan.factor = factor(data$melonsseasonalquan,levels=c("1","2","3","4","M"))
data$berriesseasonalfreq.factor = factor(data$berriesseasonalfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$berriesseasonalquan.factor = factor(data$berriesseasonalquan,levels=c("1","2","3","4","M"))
data$bananasfreq.factor = factor(data$bananasfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$bananasquan.factor = factor(data$bananasquan,levels=c("1","2","3","M"))
data$applesfreq.factor = factor(data$applesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$applesquan.factor = factor(data$applesquan,levels=c("1","2","3","M"))
data$orangesfreq.factor = factor(data$orangesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$orangesquan.factor = factor(data$orangesquan,levels=c("1","2","3","M"))
data$peachesfreq.factor = factor(data$peachesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$peachesquan.factor = factor(data$peachesquan,levels=c("1","2","3","M"))
data$otherfreshfruitfreq.factor = factor(data$otherfreshfruitfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$otherfreshfruitquan.factor = factor(data$otherfreshfruitquan,levels=c("1","2","3","4","M"))
data$driedfruitfreq.factor = factor(data$driedfruitfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$driedfruitquan.factor = factor(data$driedfruitquan,levels=c("1","2","3","M"))
data$cannedfruitfreq.factor = factor(data$cannedfruitfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cannedfruitquan.factor = factor(data$cannedfruitquan,levels=c("1","2","3","4","M"))
data$refriedbeansfreq.factor = factor(data$refriedbeansfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$refriedbeansquan.factor = factor(data$refriedbeansquan,levels=c("1","2","3","4","M"))
data$beansfreq.factor = factor(data$beansfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$beansquan.factor = factor(data$beansquan,levels=c("1","2","3","4","M"))
data$tofufreq.factor = factor(data$tofufreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$tofuquan.factor = factor(data$tofuquan,levels=c("1","2","3","4","M"))
data$meatsubstitutesfreq.factor = factor(data$meatsubstitutesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$meatsubstitutesquan.factor = factor(data$meatsubstitutesquan,levels=c("1","2","3","4","M"))
data$lentilsoupfreq.factor = factor(data$lentilsoupfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$lentilsoupquan.factor = factor(data$lentilsoupquan,levels=c("2","3","4","M"))
data$vegetablesoupfreq.factor = factor(data$vegetablesoupfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$vegetablesoupquan.factor = factor(data$vegetablesoupquan,levels=c("2","3","4","M"))
data$othersoupfreq.factor = factor(data$othersoupfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$othersoupquan.factor = factor(data$othersoupquan,levels=c("2","3","4","M"))
data$pizzafreq.factor = factor(data$pizzafreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$pizzaquan.factor = factor(data$pizzaquan,levels=c("1","2","3","4","M"))
data$macandcheesefreq.factor = factor(data$macandcheesefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$macandcheesequan.factor = factor(data$macandcheesequan,levels=c("2","3","4","M"))
data$spaghettifreq.factor = factor(data$spaghettifreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$spaghettiquan.factor = factor(data$spaghettiquan,levels=c("2","3","4","M"))
data$othernoodlesfreq.factor = factor(data$othernoodlesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$othernoodlesquan.factor = factor(data$othernoodlesquan,levels=c("2","3","4","M"))
data$eggrollfreq.factor = factor(data$eggrollfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$eggrollquan.factor = factor(data$eggrollquan,levels=c("1","2","3","4","M"))
data$eatmeat.factor = factor(data$eatmeat,levels=c("1","2","M"))
data$hamburgerfreq.factor = factor(data$hamburgerfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$hamburgerquan.factor = factor(data$hamburgerquan,levels=c("1","2","3","4","M"))
data$hotdogfreq.factor = factor(data$hotdogfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$hotdogquan.factor = factor(data$hotdogquan,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$baconsausagefreq.factor = factor(data$baconsausagefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$baconsausagequan.factor = factor(data$baconsausagequan,levels=c("1","2","3","4","M"))
data$lunchmeatfreq.factor = factor(data$lunchmeatfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$lunchmeatquan.factor = factor(data$lunchmeatquan,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$meatballsfreq.factor = factor(data$meatballsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$meatballsquan.factor = factor(data$meatballsquan,levels=c("2","3","4","M"))
data$steakfreq.factor = factor(data$steakfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$steakquan.factor = factor(data$steakquan,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$tacofreq.factor = factor(data$tacofreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$tacoquan.factor = factor(data$tacoquan,levels=c("1","2","3","4","M"))
data$ribsfreq.factor = factor(data$ribsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$ribsquan.factor = factor(data$ribsquan,levels=c("1","2","3","4","M"))
data$porkchopsfreq.factor = factor(data$porkchopsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$porkchopsquan.factor = factor(data$porkchopsquan,levels=c("1","2","3","4","M"))
data$beefporkdishfreq.factor = factor(data$beefporkdishfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$beefporkdishquan.factor = factor(data$beefporkdishquan,levels=c("2","3","4","M"))
data$liverfreq.factor = factor(data$liverfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$liverquan.factor = factor(data$liverquan,levels=c("1","2","3","M"))
data$varietymeatfreq.factor = factor(data$varietymeatfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$varietymeatquan.factor = factor(data$varietymeatquan,levels=c("1","2","3","M"))
data$veallambgamefreq.factor = factor(data$veallambgamefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$veallambgamequan.factor = factor(data$veallambgamequan,levels=c("1","2","3","M"))
data$friedorbreadedchickenfreq.factor = factor(data$friedorbreadedchickenfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$friedorbreadedchickenquan.factor = factor(data$friedorbreadedchickenquan,levels=c("1","2","3","4","M"))
data$roastchickenfreq.factor = factor(data$roastchickenfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$roastchickenquan.factor = factor(data$roastchickenquan,levels=c("1","2","3","4","M"))
data$otherchickendishfreq.factor = factor(data$otherchickendishfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$otherchickendishquan.factor = factor(data$otherchickendishquan,levels=c("2","3","4","M"))
data$eatfish.factor = factor(data$eatfish,levels=c("1","2","M"))
data$oystersfreq.factor = factor(data$oystersfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$oystersquan.factor = factor(data$oystersquan,levels=c("1","2","3","M"))
data$shellfishfreq.factor = factor(data$shellfishfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$shellfishquan.factor = factor(data$shellfishquan,levels=c("1","2","3","4","M"))
data$tunafreq.factor = factor(data$tunafreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$tunaquan.factor = factor(data$tunaquan,levels=c("1","2","3","M"))
data$salmonfreq.factor = factor(data$salmonfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$salmonquan.factor = factor(data$salmonquan,levels=c("1","2","3","4","M"))
data$friedorbreadedfishfreq.factor = factor(data$friedorbreadedfishfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$friedorbreadedfishquan.factor = factor(data$friedorbreadedfishquan,levels=c("1","2","3","4","M"))
data$otherfishfreq.factor = factor(data$otherfishfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$otherfishquan.factor = factor(data$otherfishquan,levels=c("1","2","3","4","M"))
data$peanutbutterfreq.factor = factor(data$peanutbutterfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$peanutbutterquan.factor = factor(data$peanutbutterquan,levels=c("1","2","3","4","M"))
data$walnutsfreq.factor = factor(data$walnutsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$walnutsquan.factor = factor(data$walnutsquan,levels=c("1","2","3","4","M"))
data$othernutsfreq.factor = factor(data$othernutsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$othernutsquan.factor = factor(data$othernutsquan,levels=c("1","2","3","4","M"))
data$proteinbarsfreq.factor = factor(data$proteinbarsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$proteinbarsquan.factor = factor(data$proteinbarsquan,levels=c("1","2","3","M"))
data$cerealbarsfreq.factor = factor(data$cerealbarsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cerealbarsquan.factor = factor(data$cerealbarsquan,levels=c("1","2","3","M"))
data$popcornfreq.factor = factor(data$popcornfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$popcornquan.factor = factor(data$popcornquan,levels=c("1","2","3","4","M"))
data$wholegraincrackersfreq.factor = factor(data$wholegraincrackersfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$wholegraincrackersquan.factor = factor(data$wholegraincrackersquan,levels=c("1","2","3","4","M"))
data$othercrackersfreq.factor = factor(data$othercrackersfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$othercrackersquan.factor = factor(data$othercrackersquan,levels=c("1","2","3","4","M"))
data$cornchipsfreq.factor = factor(data$cornchipsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cornchipsquan.factor = factor(data$cornchipsquan,levels=c("1","2","3","4","M"))
data$otherchipsfreq.factor = factor(data$otherchipsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$otherchipsquan.factor = factor(data$otherchipsquan,levels=c("1","2","3","4","M"))
data$donutsfreq.factor = factor(data$donutsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$donutsquan.factor = factor(data$donutsquan,levels=c("1","2","3","4","M"))
data$cakesfreq.factor = factor(data$cakesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cakesquan.factor = factor(data$cakesquan,levels=c("1","2","3","4","M"))
data$cookiesfreq.factor = factor(data$cookiesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cookiesquan.factor = factor(data$cookiesquan,levels=c("1","2","3","4","M"))
data$pumpkinpiefreq.factor = factor(data$pumpkinpiefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$pumpkinpiequan.factor = factor(data$pumpkinpiequan,levels=c("1","2","3","4","M"))
data$otherpiesfreq.factor = factor(data$otherpiesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$otherpiesquan.factor = factor(data$otherpiesquan,levels=c("1","2","3","4","M"))
data$icecreamfreq.factor = factor(data$icecreamfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$icecreamquan.factor = factor(data$icecreamquan,levels=c("2","3","4","M"))
data$puddingfreq.factor = factor(data$puddingfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$puddingquan.factor = factor(data$puddingquan,levels=c("2","3","4","M"))
data$sauceicecreamfreq.factor = factor(data$sauceicecreamfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$sauceicecreamquan.factor = factor(data$sauceicecreamquan,levels=c("1","2","3","M"))
data$popsiclesfreq.factor = factor(data$popsiclesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$popsiclesquan.factor = factor(data$popsiclesquan,levels=c("1","2","3","4","M"))
data$chocolatecandyfreq.factor = factor(data$chocolatecandyfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$chocolatecandyquan.factor = factor(data$chocolatecandyquan,levels=c("1","2","3","4","M"))
data$othercandiesfreq.factor = factor(data$othercandiesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$othercandiesquan.factor = factor(data$othercandiesquan,levels=c("1","2","3","4","M"))
data$margarinefreq.factor = factor(data$margarinefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$margarinequan.factor = factor(data$margarinequan,levels=c("1","2","3","4","M"))
data$butterfreq.factor = factor(data$butterfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$butterquan.factor = factor(data$butterquan,levels=c("1","2","3","4","M"))
data$mayofreq.factor = factor(data$mayofreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$mayoquan.factor = factor(data$mayoquan,levels=c("1","2","3","4","M"))
data$salsafreq.factor = factor(data$salsafreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$salsaquan.factor = factor(data$salsaquan,levels=c("1","2","3","4","M"))
data$barbecuesaucefreq.factor = factor(data$barbecuesaucefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$barbecuesaucequan.factor = factor(data$barbecuesaucequan,levels=c("1","2","3","4","M"))
data$otherrichsaucesfreq.factor = factor(data$otherrichsaucesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$otherrichsaucesquan.factor = factor(data$otherrichsaucesquan,levels=c("1","2","3","M"))
data$jamfreq.factor = factor(data$jamfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$jamquan.factor = factor(data$jamquan,levels=c("1","2","3","4","M"))
data$picklesfreq.factor = factor(data$picklesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$picklesquan.factor = factor(data$picklesquan,levels=c("1","2","3","4","M"))
data$saltfreq.factor = factor(data$saltfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$saltquan.factor = factor(data$saltquan,levels=c("1","2","3","4","M"))
data$cocoafreq.factor = factor(data$cocoafreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cocoaquan.factor = factor(data$cocoaquan,levels=c("1","2","3","4","M"))
data$milkfreq.factor = factor(data$milkfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$milkquan.factor = factor(data$milkquan,levels=c("1","2","3","4","M"))
data$mealreplacementdrinksfreq.factor = factor(data$mealreplacementdrinksfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$mealreplacementdrinksquan.factor = factor(data$mealreplacementdrinksquan,levels=c("1","2","3","4","M"))
data$tomatojuicefreq.factor = factor(data$tomatojuicefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$tomatojuicequan.factor = factor(data$tomatojuicequan,levels=c("1","2","3","4","M"))
data$orangejuicefreq.factor = factor(data$orangejuicefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$orangejuicequan.factor = factor(data$orangejuicequan,levels=c("1","2","3","4","M"))
data$otherfruitjuicesfreq.factor = factor(data$otherfruitjuicesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$otherfruitjuicesquan.factor = factor(data$otherfruitjuicesquan,levels=c("1","2","3","4","M"))
data$hicfreq.factor = factor(data$hicfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$hicquan.factor = factor(data$hicquan,levels=c("1","2","3","4","M"))
data$somejuicefreq.factor = factor(data$somejuicefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$somejuicequan.factor = factor(data$somejuicequan,levels=c("1","2","3","4","M"))
data$icedteafreq.factor = factor(data$icedteafreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$icedteaquan.factor = factor(data$icedteaquan,levels=c("1","2","3","4","M"))
data$sportsdrinksfreq.factor = factor(data$sportsdrinksfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$sportsdrinksquan.factor = factor(data$sportsdrinksquan,levels=c("1","2","3","4","M"))
data$energydrinksfreq.factor = factor(data$energydrinksfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$energydrinksquan.factor = factor(data$energydrinksquan,levels=c("1","2","3","4","M"))
data$lemonadefreq.factor = factor(data$lemonadefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$lemonadequan.factor = factor(data$lemonadequan,levels=c("1","2","3","4","M"))
data$sodafreq.factor = factor(data$sodafreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$sodaquan.factor = factor(data$sodaquan,levels=c("1","2","3","4","M"))
data$beerfreq.factor = factor(data$beerfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$beerquan.factor = factor(data$beerquan,levels=c("1","2","3","4","M"))
data$winefreq.factor = factor(data$winefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$winequan.factor = factor(data$winequan,levels=c("1","2","3","4","M"))
data$cocktailsfreq.factor = factor(data$cocktailsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$cocktailsquan.factor = factor(data$cocktailsquan,levels=c("1","2","3","4","M"))
data$waterfreq.factor = factor(data$waterfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$waterquan.factor = factor(data$waterquan,levels=c("1","2","3","4","M"))
data$coffeedrinksfreq.factor = factor(data$coffeedrinksfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$coffeedrinksquan.factor = factor(data$coffeedrinksquan,levels=c("1","2","3","4","M"))
data$coffeefreq.factor = factor(data$coffeefreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$coffeequan.factor = factor(data$coffeequan,levels=c("1","2","3","4","M"))
data$hotteafreq.factor = factor(data$hotteafreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$hotteaquan.factor = factor(data$hotteaquan,levels=c("1","2","3","4","M"))
data$coffeedrinkskind.factor = factor(data$coffeedrinkskind,levels=c("1","2","3","4","5","6","M"))
data$coffeedrinkstype.factor = factor(data$coffeedrinkstype,levels=c("1","2","3","4","5","6","M"))
data$decafcoffeetype.factor = factor(data$decafcoffeetype,levels=c("1","0","M"))
data$regularcoffeetype.factor = factor(data$regularcoffeetype,levels=c("1","0","M"))
data$bothkindscoffeetype.factor = factor(data$bothkindscoffeetype,levels=c("1","0","M"))
data$dontdrinkcoffeetype.factor = factor(data$dontdrinkcoffeetype,levels=c("1","0","M"))
data$creamincoffee.factor = factor(data$creamincoffee,levels=c("1","2","3","4","5","M"))
data$sugarincoffee.factor = factor(data$sugarincoffee,levels=c("1","2","M"))
data$coffeesugarteaspoons.factor = factor(data$coffeesugarteaspoons,levels=c("1","2","3","4","M"))
data$decafhotteatype.factor = factor(data$decafhotteatype,levels=c("1","0","M"))
data$regularhotteatype.factor = factor(data$regularhotteatype,levels=c("1","0","M"))
data$bothkindshotteatype.factor = factor(data$bothkindshotteatype,levels=c("1","0","M"))
data$dontdrinkhotteatype.factor = factor(data$dontdrinkhotteatype,levels=c("1","0","M"))
data$creamintea.factor = factor(data$creamintea,levels=c("1","2","3","4","5","M"))
data$sugarintea.factor = factor(data$sugarintea,levels=c("1","2","M"))
data$teasugarteaspoons.factor = factor(data$teasugarteaspoons,levels=c("1","2","3","4","M"))
data$milktype.factor = factor(data$milktype,levels=c("1","2","3","4","5","6","7","8","M"))
data$mealreplacementdrinkstype.factor = factor(data$mealreplacementdrinkstype,levels=c("1","2","3","4","5","M"))
data$orangejuicetype.factor = factor(data$orangejuicetype,levels=c("1","2","3","4","M"))
data$icedteatype.factor = factor(data$icedteatype,levels=c("1","2","3","4","5","M"))
data$lemonadetype.factor = factor(data$lemonadetype,levels=c("1","2","3","M"))
data$energydrinkstype.factor = factor(data$energydrinkstype,levels=c("1","2","3","M"))
data$sodatype.factor = factor(data$sodatype,levels=c("1","2","3","M"))
data$sodacaffeine.factor = factor(data$sodacaffeine,levels=c("1","2","3","M"))
data$beertype.factor = factor(data$beertype,levels=c("1","2","3","4","M"))
data$winetype.factor = factor(data$winetype,levels=c("1","2","3","4","M"))
data$slicedcheesetype.factor = factor(data$slicedcheesetype,levels=c("1","2","3","M"))
data$yogurtkind.factor = factor(data$yogurtkind,levels=c("1","2","M"))
data$yogurttype.factor = factor(data$yogurttype,levels=c("1","2","3","4","M"))
data$saladdressingstype.factor = factor(data$saladdressingstype,levels=c("1","2","3","4","5","M"))
data$spaghettitype.factor = factor(data$spaghettitype,levels=c("1","2","3","M"))
data$othernoodlestype.factor = factor(data$othernoodlestype,levels=c("1","2","3","4","M"))
data$hamburgertype.factor = factor(data$hamburgertype,levels=c("1","2","3","4","M"))
data$fatonmeattype.factor = factor(data$fatonmeattype,levels=c("1","2","3","4","M"))
data$chickenskintype.factor = factor(data$chickenskintype,levels=c("1","2","3","4","M"))
data$hotdogtype.factor = factor(data$hotdogtype,levels=c("1","2","3","M"))
data$lunchmeattype.factor = factor(data$lunchmeattype,levels=c("1","2","3","M"))
data$cakestype.factor = factor(data$cakestype,levels=c("1","2","3","4","M"))
data$cookiestype.factor = factor(data$cookiestype,levels=c("1","2","3","4","M"))
data$icecreamtype.factor = factor(data$icecreamtype,levels=c("1","2","3","4","M"))
data$proteinbarstype.factor = factor(data$proteinbarstype,levels=c("1","2","3","4","5","M"))
data$bageltype.factor = factor(data$bageltype,levels=c("1","2","3","4","5","M"))
data$bunstype.factor = factor(data$bunstype,levels=c("1","2","3","4","5","M"))
data$otherbreadstype.factor = factor(data$otherbreadstype,levels=c("1","2","3","4","5","M"))
data$tortillastype.factor = factor(data$tortillastype,levels=c("1","2","3","4","M"))
data$popcorntype.factor = factor(data$popcorntype,levels=c("1","2","3","4","5","6","M"))
data$crackerstype.factor = factor(data$crackerstype,levels=c("1","2","3","4","M"))
data$mayotype.factor = factor(data$mayotype,levels=c("1","2","3","M"))
data$allbranorigtype.factor = factor(data$allbranorigtype,levels=c("1","0"))
data$allbrancomptype.factor = factor(data$allbrancomptype,levels=c("1","0"))
data$applejackstype.factor = factor(data$applejackstype,levels=c("1","0"))
data$branflakestype.factor = factor(data$branflakestype,levels=c("1","0"))
data$capncrunchtype.factor = factor(data$capncrunchtype,levels=c("1","0"))
data$cheeriosplaintype.factor = factor(data$cheeriosplaintype,levels=c("1","0"))
data$cheerioshonnuttype.factor = factor(data$cheerioshonnuttype,levels=c("1","0"))
data$chexwheattype.factor = factor(data$chexwheattype,levels=c("1","0"))
data$chexothertype.factor = factor(data$chexothertype,levels=c("1","0"))
data$cinntoastcrtype.factor = factor(data$cinntoastcrtype,levels=c("1","0"))
data$cocoakrispiestype.factor = factor(data$cocoakrispiestype,levels=c("1","0"))
data$cornflakestype.factor = factor(data$cornflakestype,levels=c("1","0"))
data$cornpopstype.factor = factor(data$cornpopstype,levels=c("1","0"))
data$fiberonetype.factor = factor(data$fiberonetype,levels=c("1","0"))
data$frootloopstype.factor = factor(data$frootloopstype,levels=c("1","0"))
data$frostedflakestype.factor = factor(data$frostedflakestype,levels=c("1","0"))
data$frostedminiwheatstype.factor = factor(data$frostedminiwheatstype,levels=c("1","0"))
data$granolatype.factor = factor(data$granolatype,levels=c("1","0"))
data$grapenutstype.factor = factor(data$grapenutstype,levels=c("1","0"))
data$honbunchoatstype.factor = factor(data$honbunchoatstype,levels=c("1","0"))
data$kashigolnorhr2hrtype.factor = factor(data$kashigolnorhr2hrtype,levels=c("1","0"))
data$lifetype.factor = factor(data$lifetype,levels=c("1","0"))
data$luckycharmstype.factor = factor(data$luckycharmstype,levels=c("1","0"))
data$oatsquarestype.factor = factor(data$oatsquarestype,levels=c("1","0"))
data$raisinbrantype.factor = factor(data$raisinbrantype,levels=c("1","0"))
data$ricekrispiestype.factor = factor(data$ricekrispiestype,levels=c("1","0"))
data$shreddedwheattype.factor = factor(data$shreddedwheattype,levels=c("1","0"))
data$specialkplaintype.factor = factor(data$specialkplaintype,levels=c("1","0"))
data$specialkflavstype.factor = factor(data$specialkflavstype,levels=c("1","0"))
data$totaltype.factor = factor(data$totaltype,levels=c("1","0"))
data$wheatiestype.factor = factor(data$wheatiestype,levels=c("1","0"))
data$othersweetcerealtype.factor = factor(data$othersweetcerealtype,levels=c("1","0"))
data$otherunsweetcerealtype.factor = factor(data$otherunsweetcerealtype,levels=c("1","0"))
data$otherwholegraincerealtype.factor = factor(data$otherwholegraincerealtype,levels=c("1","0"))
data$otherfibercerealtype.factor = factor(data$otherfibercerealtype,levels=c("1","0"))
data$donteatordontknowcerealtype.factor = factor(data$donteatordontknowcerealtype,levels=c("1","0"))
data$cookingfatpamornone.factor = factor(data$cookingfatpamornone,levels=c("1","0"))
data$cookingfatbutter.factor = factor(data$cookingfatbutter,levels=c("1","0"))
data$cookingfathalf.factor = factor(data$cookingfathalf,levels=c("1","0"))
data$cookingfatstickmarg.factor = factor(data$cookingfatstickmarg,levels=c("1","0"))
data$cookingfatsofttubmarg.factor = factor(data$cookingfatsofttubmarg,levels=c("1","0"))
data$cookingfatlowfatmarg.factor = factor(data$cookingfatlowfatmarg,levels=c("1","0"))
data$cookingfatolive.factor = factor(data$cookingfatolive,levels=c("1","0"))
data$cookingfatcanola.factor = factor(data$cookingfatcanola,levels=c("1","0"))
data$cookingfatcorn.factor = factor(data$cookingfatcorn,levels=c("1","0"))
data$cookingfatpeanut.factor = factor(data$cookingfatpeanut,levels=c("1","0"))
data$cookingfatlard.factor = factor(data$cookingfatlard,levels=c("1","0"))
data$cookingfatcrisco.factor = factor(data$cookingfatcrisco,levels=c("1","0"))
data$cookingfatother.factor = factor(data$cookingfatother,levels=c("1","0"))
data$cookingfatdontknow.factor = factor(data$cookingfatdontknow,levels=c("1","0"))
data$usevitsregularly.factor = factor(data$usevitsregularly,levels=c("1","2","M"))
data$prenatalvitsamount.factor = factor(data$prenatalvitsamount,levels=c("1","4","5","6","7","8","9","M"))
data$prenatalvitsyears.factor = factor(data$prenatalvitsyears,levels=c("1","2","3","4","M"))
data$oneadayamount.factor = factor(data$oneadayamount,levels=c("1","4","5","6","7","8","9","M"))
data$oneadayyears.factor = factor(data$oneadayyears,levels=c("1","2","3","4","M"))
data$bcomplextypevitsamount.factor = factor(data$bcomplextypevitsamount,levels=c("1","4","5","6","7","8","9","M"))
data$bcomplextypevitsyears.factor = factor(data$bcomplextypevitsyears,levels=c("1","2","3","4","M"))
data$antioxidantcomboamount.factor = factor(data$antioxidantcomboamount,levels=c("1","4","5","6","7","8","9","M"))
data$antioxidantcomboyears.factor = factor(data$antioxidantcomboyears,levels=c("1","2","3","4","M"))
data$vitaminaamount.factor = factor(data$vitaminaamount,levels=c("1","4","5","6","7","8","9","M"))
data$vitaminayears.factor = factor(data$vitaminayears,levels=c("1","2","3","4","M"))
data$vitaminb6amount.factor = factor(data$vitaminb6amount,levels=c("1","4","5","6","7","8","9","M"))
data$vitaminb6years.factor = factor(data$vitaminb6years,levels=c("1","2","3","4","M"))
data$vitaminb12amount.factor = factor(data$vitaminb12amount,levels=c("1","4","5","6","7","8","9","M"))
data$vitaminb12years.factor = factor(data$vitaminb12years,levels=c("1","2","3","4","M"))
data$vitamincamount.factor = factor(data$vitamincamount,levels=c("1","4","5","6","7","8","9","M"))
data$vitamincyears.factor = factor(data$vitamincyears,levels=c("1","2","3","4","M"))
data$vitamindamount.factor = factor(data$vitamindamount,levels=c("1","4","5","6","7","8","9","M"))
data$vitamindyears.factor = factor(data$vitamindyears,levels=c("1","2","3","4","M"))
data$vitamineamount.factor = factor(data$vitamineamount,levels=c("1","4","5","6","7","8","9","M"))
data$vitamineyears.factor = factor(data$vitamineyears,levels=c("1","2","3","4","M"))
data$folicacidamount.factor = factor(data$folicacidamount,levels=c("1","4","5","6","7","8","9","M"))
data$folicacidyears.factor = factor(data$folicacidyears,levels=c("1","2","3","4","M"))
data$calciumamount.factor = factor(data$calciumamount,levels=c("1","4","5","6","7","8","9","M"))
data$calciumyears.factor = factor(data$calciumyears,levels=c("1","2","3","4","M"))
data$ironamount.factor = factor(data$ironamount,levels=c("1","4","5","6","7","8","9","M"))
data$ironyears.factor = factor(data$ironyears,levels=c("1","2","3","4","M"))
data$zincamount.factor = factor(data$zincamount,levels=c("1","4","5","6","7","8","9","M"))
data$zincyears.factor = factor(data$zincyears,levels=c("1","2","3","4","M"))
data$omegasuppfreq.factor = factor(data$omegasuppfreq,levels=c("1","4","5","6","7","8","9","M"))
data$omegasuppyears.factor = factor(data$omegasuppyears,levels=c("1","2","3","4","M"))
data$fibersuppamount.factor = factor(data$fibersuppamount,levels=c("1","4","5","6","7","8","9","M"))
data$fibersuppyears.factor = factor(data$fibersuppyears,levels=c("1","2","3","4","M"))
data$mineralsyesorno.factor = factor(data$mineralsyesorno,levels=c("1","2","3","M"))
data$vitamincquan.factor = factor(data$vitamincquan,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$vitaminequan.factor = factor(data$vitaminequan,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$calciumquan.factor = factor(data$calciumquan,levels=c("1","2","3","4","5","M"))
data$vitamindquan.factor = factor(data$vitamindquan,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$fishoiltype.factor = factor(data$fishoiltype,levels=c("1","0"))
data$flaxhempseedoiltype.factor = factor(data$flaxhempseedoiltype,levels=c("1","0"))
data$krilloiltype.factor = factor(data$krilloiltype,levels=c("1","0"))
data$algaeoiltype.factor = factor(data$algaeoiltype,levels=c("1","0"))
data$omega3dontknowtype.factor = factor(data$omega3dontknowtype,levels=c("1","0"))
data$veggiesfreq.factor = factor(data$veggiesfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$fruitsfreq.factor = factor(data$fruitsfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$fatoilfreq.factor = factor(data$fatoilfreq,levels=c("1","2","3","4","5","6","7","8","9","M"))
data$meals.factor = factor(data$meals,levels=c("1","2","3","4","5","M"))
data$snacks.factor = factor(data$snacks,levels=c("1","2","3","4","5","M"))
data$lighthousefreq.factor = factor(data$lighthousefreq,levels=c("1","2","3","4","5","6","M"))
data$lighthousetime.factor = factor(data$lighthousetime,levels=c("1","2","3","4","M"))
data$slowwalkfreq.factor = factor(data$slowwalkfreq,levels=c("1","2","3","4","5","6","M"))
data$slowwalktime.factor = factor(data$slowwalktime,levels=c("1","2","3","4","M"))
data$jobstandfreq.factor = factor(data$jobstandfreq,levels=c("1","2","3","4","5","6","M"))
data$jobstandtime.factor = factor(data$jobstandtime,levels=c("1","2","3","4","M"))
data$childcarefreq.factor = factor(data$childcarefreq,levels=c("1","2","3","4","5","6","M"))
data$childcaretime.factor = factor(data$childcaretime,levels=c("1","2","3","4","M"))
data$weedyardfreq.factor = factor(data$weedyardfreq,levels=c("1","2","3","4","5","6","M"))
data$weedyardtime.factor = factor(data$weedyardtime,levels=c("1","2","3","4","M"))
data$briskwalkfreq.factor = factor(data$briskwalkfreq,levels=c("1","2","3","4","5","6","M"))
data$briskwalktime.factor = factor(data$briskwalktime,levels=c("1","2","3","4","M"))
data$jobwalkfreq.factor = factor(data$jobwalkfreq,levels=c("1","2","3","4","5","6","M"))
data$jobwalktime.factor = factor(data$jobwalktime,levels=c("1","2","3","4","M"))
data$heavyworkfreq.factor = factor(data$heavyworkfreq,levels=c("1","2","3","4","5","6","M"))
data$heavyworktime.factor = factor(data$heavyworktime,levels=c("1","2","3","4","M"))
data$jobliftfreq.factor = factor(data$jobliftfreq,levels=c("1","2","3","4","5","6","M"))
data$joblifttime.factor = factor(data$joblifttime,levels=c("1","2","3","4","M"))
data$exergymfreq.factor = factor(data$exergymfreq,levels=c("1","2","3","4","5","6","M"))
data$exergymtime.factor = factor(data$exergymtime,levels=c("1","2","3","4","M"))
data$bikeswimfreq.factor = factor(data$bikeswimfreq,levels=c("1","2","3","4","5","6","M"))
data$bikeswimtime.factor = factor(data$bikeswimtime,levels=c("1","2","3","4","M"))
data$latino.factor = factor(data$latino,levels=c("1","2","3"))
data$white.factor = factor(data$white,levels=c("1","0"))
data$black.factor = factor(data$black,levels=c("1","0"))
data$asian.factor = factor(data$asian,levels=c("1","0"))
data$nativeamer.factor = factor(data$nativeamer,levels=c("1","0"))
data$hawaiian.factor = factor(data$hawaiian,levels=c("1","0"))
data$notprovided.factor = factor(data$notprovided,levels=c("1","0"))
data$ffq_qaqc_kcalok.factor = factor(data$ffq_qaqc_kcalok,levels=c("1","2","3"))
data$ffq_qaqc_foodsperday.factor = factor(data$ffq_qaqc_foodsperday,levels=c("1","2","3"))
data$ffq_qc_passed.factor = factor(data$ffq_qc_passed,levels=c("1","0"))
data$block_ffq_complete.factor = factor(data$block_ffq_complete,levels=c("0","1","2"))

levels(data$sex_ffq.factor)=c("Male","Female")
levels(data$pregnant.factor)=c("No","Yes","Not Female","Missing")
levels(data$breakfastsandwichfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$breakfastsandwichquan.factor)=c("1 sandwich","2 sandwiches","Missing")
levels(data$eggsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$eggsquan.factor)=c("1","2","3","4","Missing")
levels(data$yogurtfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$yogurtquan.factor)=c("1/2 cup","1 cup","2 cups","Missing")
levels(data$cottagecheesefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cottagecheesequan.factor)=c("1/4 cup","1/2","1 cup","2 cups","Missing")
levels(data$creamcheesefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$creamcheesequan.factor)=c("1 tablespoon","2 tablespoons","3 tablespoons","4 tablespoons","Missing")
levels(data$slicedcheesefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$slicedcheesequan.factor)=c("1 slice","2 slices","3 slices","4 slices","Missing")
levels(data$coldcerealfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$coldcerealquan.factor)=c("1/2 cup","1 cup","2 cups","Missing")
levels(data$wholegraincerealfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$wholegraincerealquan.factor)=c("1/4 cup","1/2","1 cup","2 cups","Missing")
levels(data$gritsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$gritsquan.factor)=c("1/4 cup","1/2","1 cup","2 cups","Missing")
levels(data$milkoncerealfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$milkoncerealquan.factor)=c("N/A")
levels(data$brownricefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$brownricequan.factor)=c("1/2 cup","1 cup","2 cups","Missing")
levels(data$whitericefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$whitericequan.factor)=c("1/2 cup","1 cup","2 cups","Missing")
levels(data$pancakefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$pancakequan.factor)=c("One","Two","Three","Four","Missing")
levels(data$pastriesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$pastriesquan.factor)=c("1 small piece","1 medium piece","2 pieces","3 pieces","Missing")
levels(data$biscuitfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$biscuitquan.factor)=c("1 small piece","1 medium piece","2 pieces","3 pieces","Missing")
levels(data$cornbreadfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cornbreadquan.factor)=c("1/2 piece","1  piece","2 pieces","3 pieces","Missing")
levels(data$bunsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$bunsquan.factor)=c("1/2 bun","1  bun","2 buns","3 buns","Missing")
levels(data$bagelfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$bagelquan.factor)=c("1/2 bagel","1  bagel","2 bagels","3 bagels","Missing")
levels(data$tortillasfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$tortillasquan.factor)=c("One","1  Two","2 Three","3 Four","Missing")
levels(data$otherbreadsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$otherbreadsquan.factor)=c("One","1  Two","2 Three","3 Four","Missing")
levels(data$broccolifreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$broccoliquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$carrotsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$carrotsquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$cornfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cornquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$greenbeansfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$greenbeansquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$cookedgreensfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cookedgreensquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$cabbagefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cabbagequan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$greensaladfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$greensaladquan.factor)=c("1/2 cup","1 cup","2 cup","3+ cups","Missing")
levels(data$rawtomatoesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$rawtomatoesquan.factor)=c("1/4 tomato","1/2 tomato","1 tomato","2 tomatoes","Missing")
levels(data$saladdressingsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$saladdressingsquan.factor)=c("1 tablespoon","2 tablespoons","3 tablespoons","4 tablespoons","Missing")
levels(data$avocadofreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$avocadoquan.factor)=c("1 tablespoon","2 tablespoons","3 tablespoons","4 tablespoons","Missing")
levels(data$sweetpotatoesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$sweetpotatoesquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$friesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$friesquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$potatoesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$potatoesquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$otherveggiesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$otherveggiesquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$melonsseasonalfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$melonsseasonalquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$berriesseasonalfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$berriesseasonalquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$bananasfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$bananasquan.factor)=c("1/2 banana","1 banana","2 bananas","Missing")
levels(data$applesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$applesquan.factor)=c("1/2","1","2","Missing")
levels(data$orangesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$orangesquan.factor)=c("1/2","1","2","Missing")
levels(data$peachesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$peachesquan.factor)=c("1/2","1","2","Missing")
levels(data$otherfreshfruitfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$otherfreshfruitquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$driedfruitfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$driedfruitquan.factor)=c("1/4 cup","1/2  cup","1 cup","Missing")
levels(data$cannedfruitfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cannedfruitquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$refriedbeansfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$refriedbeansquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$beansfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$beansquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$tofufreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$tofuquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$meatsubstitutesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$meatsubstitutesquan.factor)=c("1/4 cup","1/2  cup, one patty or dog","1 cup","2 cups","Missing")
levels(data$lentilsoupfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$lentilsoupquan.factor)=c("1/2  cup","1 cup","2 cups","Missing")
levels(data$vegetablesoupfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$vegetablesoupquan.factor)=c("1/2  cup","1 cup","2 cups","Missing")
levels(data$othersoupfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$othersoupquan.factor)=c("1/2  cup","1 cup","2 cups","Missing")
levels(data$pizzafreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$pizzaquan.factor)=c("One","Two","Three","Four","Missing")
levels(data$macandcheesefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$macandcheesequan.factor)=c("1/2  cup","1 cup","2 cups","Missing")
levels(data$spaghettifreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$spaghettiquan.factor)=c("1/2  cup","1 cup","2 cups","Missing")
levels(data$othernoodlesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$othernoodlesquan.factor)=c("1/2  cup","1 cup","2 cups","Missing")
levels(data$eggrollfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$eggrollquan.factor)=c("One","Two","Three","Four","Missing")
levels(data$eatmeat.factor)=c("Yes","No","Missing")
levels(data$hamburgerfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$hamburgerquan.factor)=c("One small burger","One large burger","Two burgers","Three burgers","Missing")
levels(data$hotdogfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$hotdogquan.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$baconsausagefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$baconsausagequan.factor)=c("One","Two","Three","Four","Missing")
levels(data$lunchmeatfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$lunchmeatquan.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$meatballsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$meatballsquan.factor)=c("1/2  cup","1 cup","2 cups","Missing")
levels(data$steakfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$steakquan.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$tacofreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$tacoquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$ribsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$ribsquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$porkchopsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$porkchopsquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$beefporkdishfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$beefporkdishquan.factor)=c("1/2  cup","1 cup","2 cups","Missing")
levels(data$liverfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$liverquan.factor)=c("1/4 cup","1/2  cup","1 cup","Missing")
levels(data$varietymeatfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$varietymeatquan.factor)=c("1/4 cup","1/2  cup","1 cup","Missing")
levels(data$veallambgamefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$veallambgamequan.factor)=c("1/4 cup","1/2  cup","1 cup","Missing")
levels(data$friedorbreadedchickenfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$friedorbreadedchickenquan.factor)=c("one piece","2 pieces/6 nuggets","3 pieces","4 pieces","Missing")
levels(data$roastchickenfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$roastchickenquan.factor)=c("1/4 cup","1/2  cup or 1 medium piece","1 cup","2 cups or half chicken","Missing")
levels(data$otherchickendishfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$otherchickendishquan.factor)=c("1/2  cup","1 cup","2 cups","Missing")
levels(data$eatfish.factor)=c("Yes","No","Missing")
levels(data$oystersfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$oystersquan.factor)=c("1/4 cup","1/2  cup","1 cup","Missing")
levels(data$shellfishfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$shellfishquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$tunafreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$tunaquan.factor)=c("1/4 cup","1/2  cup","1 cup","Missing")
levels(data$salmonfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$salmonquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$friedorbreadedfishfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$friedorbreadedfishquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$otherfishfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$otherfishquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$peanutbutterfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$peanutbutterquan.factor)=c("1/2 tablespoon","1 tablespoons","2 tablespoons","3 tablespoons","Missing")
levels(data$walnutsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$walnutsquan.factor)=c("1 tablespoon","2 tablespoons","1/4 cup","1/2 cup","Missing")
levels(data$othernutsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$othernutsquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$proteinbarsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$proteinbarsquan.factor)=c("Small bar","Medium bar","Large bar","Missing")
levels(data$cerealbarsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cerealbarsquan.factor)=c("One bar","Two bars","Three bars","Missing")
levels(data$popcornfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$popcornquan.factor)=c("1-2 cups","3-6 cups","7-9 cups","10-12 cups","Missing")
levels(data$wholegraincrackersfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$wholegraincrackersquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$othercrackersfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$othercrackersquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$cornchipsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cornchipsquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$otherchipsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$otherchipsquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$donutsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$donutsquan.factor)=c("1 mini donut","1 medium donut","2 donuts","3 donuts","Missing")
levels(data$cakesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cakesquan.factor)=c("1 small","1 medium","2","3","Missing")
levels(data$cookiesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cookiesquan.factor)=c("1-2 cups","3-4 cups","5-6 cups","7+ cups","Missing")
levels(data$pumpkinpiefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$pumpkinpiequan.factor)=c("1/2","1","2","3","Missing")
levels(data$otherpiesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$otherpiesquan.factor)=c("1/2","1","2","3","Missing")
levels(data$icecreamfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$icecreamquan.factor)=c("1/2 cup","1 cup","2 cups","Missing")
levels(data$puddingfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$puddingquan.factor)=c("1/2 cup","1 cup","2 cups","Missing")
levels(data$sauceicecreamfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$sauceicecreamquan.factor)=c("1-2  tablespoons","3-4 tablespoons","1/2 cups","Missing")
levels(data$popsiclesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$popsiclesquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$chocolatecandyfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$chocolatecandyquan.factor)=c("1 mini size","1 medium size","1 large size","1 king size","Missing")
levels(data$othercandiesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$othercandiesquan.factor)=c("1-2 pieces","1/2 package","1 package","2 packages","Missing")
levels(data$margarinefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$margarinequan.factor)=c("One","Two","Three","Four","Missing")
levels(data$butterfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$butterquan.factor)=c("One","Two","Three","Four","Missing")
levels(data$mayofreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$mayoquan.factor)=c("1/2 tablespoon","1 tablespoon","2 tablespoons","3 tablespoons","Missing")
levels(data$salsafreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$salsaquan.factor)=c("1/2 tablespoon","1 tablespoon","2 tablespoons","3 tablespoons","Missing")
levels(data$barbecuesaucefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$barbecuesaucequan.factor)=c("1/2 tablespoon","1 tablespoon","2 tablespoons","3 tablespoons","Missing")
levels(data$otherrichsaucesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$otherrichsaucesquan.factor)=c("1/4 cup","1/2 cup","1 cup","Missing")
levels(data$jamfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$jamquan.factor)=c("1/2 tablespoon","1 tablespoon","2 tablespoons","3 tablespoons","Missing")
levels(data$picklesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$picklesquan.factor)=c("1/4 cup","1/2  cup","1 cup","2 cups","Missing")
levels(data$saltfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$saltquan.factor)=c("1-3","4-5","6-7","8+","Missing")
levels(data$cocoafreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cocoaquan.factor)=c("1/2 serving","1 servings","2 servings","3 servings","Missing")
levels(data$milkfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$milkquan.factor)=c("1 serving","2 servings","3 servings","4 servings","Missing")
levels(data$mealreplacementdrinksfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$mealreplacementdrinksquan.factor)=c("One","Two","Three","Four","Missing")
levels(data$tomatojuicefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$tomatojuicequan.factor)=c("1/2 serving","1 servings","2 servings","3 servings","Missing")
levels(data$orangejuicefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$orangejuicequan.factor)=c("1/2 serving","1 servings","2 servings","3 servings","Missing")
levels(data$otherfruitjuicesfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$otherfruitjuicesquan.factor)=c("1/2 serving","1 servings","2 servings","3 servings","Missing")
levels(data$hicfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$hicquan.factor)=c("1/2 serving","1 servings","2 servings","3 servings","Missing")
levels(data$somejuicefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$somejuicequan.factor)=c("1/2 serving","1 servings","2 servings","3 servings","Missing")
levels(data$icedteafreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$icedteaquan.factor)=c("1/2 serving","1 servings","2 servings","3 servings","Missing")
levels(data$sportsdrinksfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$sportsdrinksquan.factor)=c("16-oz bottle","1 20-oz bottle","2 16-oz bottles","2 20-oz bottles","Missing")
levels(data$energydrinksfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$energydrinksquan.factor)=c("8-oz can","1 12-16-oz can","1 20-oz can","24 oz or more","Missing")
levels(data$lemonadefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$lemonadequan.factor)=c("8-oz can","1 12-16-oz can","1 20-oz can","30 oz or more","Missing")
levels(data$sodafreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$sodaquan.factor)=c("1 can","20-oz bottle","2 cans","Big Gulp or 3 cans","Missing")
levels(data$beerfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$beerquan.factor)=c("1 can","2 cans","3-4 cans or small pitcher","5+ cans or large pitcher","Missing")
levels(data$winefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$winequan.factor)=c("1/2 glass","1 glass","2 glasses, 1/2 bottle","4+ glasses","Missing")
levels(data$cocktailsfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$cocktailsquan.factor)=c("One","Two","Three","Four","Missing")
levels(data$waterfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$waterquan.factor)=c("1","2","3-4","5+","Missing")
levels(data$coffeedrinksfreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$coffeedrinksquan.factor)=c("12 oz","16 oz","20 oz","24+ oz","Missing")
levels(data$coffeefreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$coffeequan.factor)=c("1","2","3","4+","Missing")
levels(data$hotteafreq.factor)=c("Never","A few times per year","Once per month","2-3 times per month","Once per week","2 times per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$hotteaquan.factor)=c("1","2","3","4+","Missing")
levels(data$coffeedrinkskind.factor)=c("Frappuccino","Mocha","Latte or cappuccino","Caf? con leche","Some of each (default)","Dont drink","Missing")
levels(data$coffeedrinkstype.factor)=c("Whole milk","1 or 2% milk (reduced fat)","Skim milk or non-fat","Soy milk","Something else","Dont drink","Missing")
levels(data$decafcoffeetype.factor)=c("Selected","Not Selected","Missing")
levels(data$regularcoffeetype.factor)=c("Selected","Not Selected","Missing")
levels(data$bothkindscoffeetype.factor)=c("Selected","Not Selected","Missing")
levels(data$dontdrinkcoffeetype.factor)=c("Selected","Not Selected","Missing")
levels(data$creamincoffee.factor)=c("Cream or half-n-half","CoffeeMate, non-dairy creamer","Condensed milk","Any other milk","None of these","Missing")
levels(data$sugarincoffee.factor)=c("No","Yes","Missing")
levels(data$coffeesugarteaspoons.factor)=c("One","Two","Three","Four","Missing")
levels(data$decafhotteatype.factor)=c("Selected","Not Selected","Missing")
levels(data$regularhotteatype.factor)=c("Selected","Not Selected","Missing")
levels(data$bothkindshotteatype.factor)=c("Selected","Not Selected","Missing")
levels(data$dontdrinkhotteatype.factor)=c("Selected","Not Selected","Missing")
levels(data$creamintea.factor)=c("Cream or half-n-half","CoffeeMate, non-dairy creamer","Condensed milk","Any other milk","None of these","Missing")
levels(data$sugarintea.factor)=c("No","Yes","Missing")
levels(data$teasugarteaspoons.factor)=c("One","Two","Three","Four","Missing")
levels(data$milktype.factor)=c("Whole milk","2% milk (default)","1% milk (low-fat)","Skim milk, non-fat","Soy milk","Rice milk","Almond milk, other","Dont drink","Missing")
levels(data$mealreplacementdrinkstype.factor)=c("Slim Fast, Ensure, regular","Slim Fast, Ensure, light or low-carb","High protein drinks, regular","High protein drinks, light or low-carb","Dont know/ Dont drink","Missing")
levels(data$orangejuicetype.factor)=c("Calcium fortified","Not calcium fortified","Dont know","Dont drink","Missing")
levels(data$icedteatype.factor)=c("Home-made, no sugar","Home-made, with sugar","Bottled, no-sugar","Bottled, pre-sweetened","Dont drink","Missing")
levels(data$lemonadetype.factor)=c("Sugar-free","Regular","Dont drink","Missing")
levels(data$energydrinkstype.factor)=c("Sugar-free","Regular","Dont drink","Missing")
levels(data$sodatype.factor)=c("Diet, low-calorie","Regular","Dont drink","Missing")
levels(data$sodacaffeine.factor)=c("Has caffeine","Does not have caffeine","Dont drink","Missing")
levels(data$beertype.factor)=c("Regular","Light","Non-alcoholic","Dont drink","Missing")
levels(data$winetype.factor)=c("Red wine","White wine","Both red and white wine","Dont drink","Missing")
levels(data$slicedcheesetype.factor)=c("Low-fat","Regular-fat","Dont eat","Missing")
levels(data$yogurtkind.factor)=c("Low-fat","With fruit or other flavors","Missing")
levels(data$yogurttype.factor)=c("Low-fat","Non-fat","Regular","Dont eat","Missing")
levels(data$saladdressingstype.factor)=c("Low-fat, lite","Fat free","Regular","Oil and vinegar","Dont use","Missing")
levels(data$spaghettitype.factor)=c("Meatless","With meat sauce or meatballs","Dont eat","Missing")
levels(data$othernoodlestype.factor)=c("Rarely while grain","Sometimes whole grain","usually whole grain","Dont eat/know","Missing")
levels(data$hamburgertype.factor)=c("Hamburger","Cheeseburger","Turkey burger","Dont eat","Missing")
levels(data$fatonmeattype.factor)=c("Avoid eating the fat","Sometimes eat the fat","Often eat the fat","Dont eay (beef or pork)","Missing")
levels(data$chickenskintype.factor)=c("Avoid eating the fat","Sometimes eat the fat","Often eat the fat","Dont eay ( Chicken or turkey)","Missing")
levels(data$hotdogtype.factor)=c("Beef or pork","chicken or turkey, low-fat","Dont eat","Missing")
levels(data$lunchmeattype.factor)=c("Beef or pork","chicken or turkey, low-fat","Dont eat","Missing")
levels(data$cakestype.factor)=c("Low-sugar, low-carb","Low-fat","Regular-fat","Dont eat","Missing")
levels(data$cookiestype.factor)=c("Low-sugar, low-carb","Low-fat","Regular-fat","Dont eat","Missing")
levels(data$icecreamtype.factor)=c("Low-sugar, low-carb ice cream","Low-fat ice cream or frozen yogurt","Regular ice cream","Dont eat ice cream or frozen yogurt","Missing")
levels(data$proteinbarstype.factor)=c("High energy","High protein","Some of each","Dont know","Dont eat","Missing")
levels(data$bageltype.factor)=c("White","Multi-grain","100% whole wheat","Eat all kinds","Dont eat","Missing")
levels(data$bunstype.factor)=c("White","Multi-grain","100% whole wheat","Eat all kinds","Dont eat","Missing")
levels(data$otherbreadstype.factor)=c("White","Multi-grain","100% whole wheat","Eat some of each","Dont eat","Missing")
levels(data$tortillastype.factor)=c("Corn tortillas","Flour tortillas (wheat)","Eat all kinds or dont know","Dont eat","Missing")
levels(data$popcorntype.factor)=c("Air popped, fat free","Low-fat or light","Regular","Caramel corn","Dont know","Dont eat","Missing")
levels(data$crackerstype.factor)=c("Low-fat, including RyeKrisp, rice cakes, or plain pretzels","Regular-fat crackers or filled pretzels","Dont know","Dont eat","Missing")
levels(data$mayotype.factor)=c("Low-fat, light","Regular","Dont eat","Missing")
levels(data$allbranorigtype.factor)=c("Selected","Not Selected")
levels(data$allbrancomptype.factor)=c("Selected","Not Selected")
levels(data$applejackstype.factor)=c("Selected","Not Selected")
levels(data$branflakestype.factor)=c("Selected","Not Selected")
levels(data$capncrunchtype.factor)=c("Selected","Not Selected")
levels(data$cheeriosplaintype.factor)=c("Selected","Not Selected")
levels(data$cheerioshonnuttype.factor)=c("Selected","Not Selected")
levels(data$chexwheattype.factor)=c("Selected","Not Selected")
levels(data$chexothertype.factor)=c("Selected","Not Selected")
levels(data$cinntoastcrtype.factor)=c("Selected","Not Selected")
levels(data$cocoakrispiestype.factor)=c("Selected","Not Selected")
levels(data$cornflakestype.factor)=c("Selected","Not Selected")
levels(data$cornpopstype.factor)=c("Selected","Not Selected")
levels(data$fiberonetype.factor)=c("Selected","Not Selected")
levels(data$frootloopstype.factor)=c("Selected","Not Selected")
levels(data$frostedflakestype.factor)=c("Selected","Not Selected")
levels(data$frostedminiwheatstype.factor)=c("Selected","Not Selected")
levels(data$granolatype.factor)=c("Selected","Not Selected")
levels(data$grapenutstype.factor)=c("Selected","Not Selected")
levels(data$honbunchoatstype.factor)=c("Selected","Not Selected")
levels(data$kashigolnorhr2hrtype.factor)=c("Selected","Not Selected")
levels(data$lifetype.factor)=c("Selected","Not Selected")
levels(data$luckycharmstype.factor)=c("Selected","Not Selected")
levels(data$oatsquarestype.factor)=c("Selected","Not Selected")
levels(data$raisinbrantype.factor)=c("Selected","Not Selected")
levels(data$ricekrispiestype.factor)=c("Selected","Not Selected")
levels(data$shreddedwheattype.factor)=c("Selected","Not Selected")
levels(data$specialkplaintype.factor)=c("Selected","Not Selected")
levels(data$specialkflavstype.factor)=c("Selected","Not Selected")
levels(data$totaltype.factor)=c("Selected","Not Selected")
levels(data$wheatiestype.factor)=c("Selected","Not Selected")
levels(data$othersweetcerealtype.factor)=c("Selected","Not Selected")
levels(data$otherunsweetcerealtype.factor)=c("Selected","Not Selected")
levels(data$otherwholegraincerealtype.factor)=c("Selected","Not Selected")
levels(data$otherfibercerealtype.factor)=c("Selected","Not Selected")
levels(data$donteatordontknowcerealtype.factor)=c("Selected","Not Selected")
levels(data$cookingfatpamornone.factor)=c("Selected","Not Selected")
levels(data$cookingfatbutter.factor)=c("Selected","Not Selected")
levels(data$cookingfathalf.factor)=c("Selected","Not Selected")
levels(data$cookingfatstickmarg.factor)=c("Selected","Not Selected")
levels(data$cookingfatsofttubmarg.factor)=c("Selected","Not Selected")
levels(data$cookingfatlowfatmarg.factor)=c("Selected","Not Selected")
levels(data$cookingfatolive.factor)=c("Selected","Not Selected")
levels(data$cookingfatcanola.factor)=c("Selected","Not Selected")
levels(data$cookingfatcorn.factor)=c("Selected","Not Selected")
levels(data$cookingfatpeanut.factor)=c("Selected","Not Selected")
levels(data$cookingfatlard.factor)=c("Selected","Not Selected")
levels(data$cookingfatcrisco.factor)=c("Selected","Not Selected")
levels(data$cookingfatother.factor)=c("Selected","Not Selected")
levels(data$cookingfatdontknow.factor)=c("Selected","Not Selected")
levels(data$usevitsregularly.factor)=c("No, not regularly","Yes, fairly regularly","Missing")
levels(data$prenatalvitsamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$prenatalvitsyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$oneadayamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$oneadayyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$bcomplextypevitsamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$bcomplextypevitsyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$antioxidantcomboamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$antioxidantcomboyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$vitaminaamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$vitaminayears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$vitaminb6amount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$vitaminb6years.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$vitaminb12amount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$vitaminb12years.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$vitamincamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$vitamincyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$vitamindamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$vitamindyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$vitamineamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$vitamineyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$folicacidamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$folicacidyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$calciumamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$calciumyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$ironamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$ironyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$zincamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$zincyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$omegasuppfreq.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$omegasuppyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$fibersuppamount.factor)=c("Didnt take","A few times per year","1 day per week","2 days per week","3-4 times per week","5-6 times per week","Every day","Missing")
levels(data$fibersuppyears.factor)=c("Less than 1 year","1-4 years","5-9 years","10 +","Missing")
levels(data$mineralsyesorno.factor)=c("Contain minerals, iron, zinc, etc","Do not contain minerals","Dont know","Missing")
levels(data$vitamincquan.factor)=c("100","250","500","750","1000","1500","2000","3000+","Dont know","Missing")
levels(data$vitaminequan.factor)=c("100","200","300","400","600","800","1000","2000+","Dont know","Missing")
levels(data$calciumquan.factor)=c("100","350","650","1250 +","Dont know","Missing")
levels(data$vitamindquan.factor)=c("400","600","800","1000","2000","3000","4000","5000+","Dont know","Missing")
levels(data$fishoiltype.factor)=c("Selected","Not Selected")
levels(data$flaxhempseedoiltype.factor)=c("Selected","Not Selected")
levels(data$krilloiltype.factor)=c("Selected","Not Selected")
levels(data$algaeoiltype.factor)=c("Selected","Not Selected")
levels(data$omega3dontknowtype.factor)=c("Selected","Not Selected")
levels(data$veggiesfreq.factor)=c("Rarely","1-2 per week","3-4 per week","5-6 per week","1 per day","1 1/2 per day","2 per day","3 per day","4+ per day","Missing")
levels(data$fruitsfreq.factor)=c("Rarely","1-2 per week","3-4 per week","5-6 per week","1 per day","1 1/2 per day","2 per day","3 per day","4+ per day","Missing")
levels(data$fatoilfreq.factor)=c("Rarely","1-2 per week","3-4 per week","5-6 per week","1 per day","1 1/2 per day","2 per day","3 per day","4+ per day","Missing")
levels(data$meals.factor)=c("1","2","3","4","5","Missing")
levels(data$snacks.factor)=c("1","2","3","4","6","Missing")
levels(data$lighthousefreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$lighthousetime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$slowwalkfreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$slowwalktime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$jobstandfreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$jobstandtime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$childcarefreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$childcaretime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$weedyardfreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$weedyardtime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$briskwalkfreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$briskwalktime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$jobwalkfreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$jobwalktime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$heavyworkfreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$heavyworktime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$jobliftfreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$joblifttime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$exergymfreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$exergymtime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$bikeswimfreq.factor)=c("Rarely or Never","A few times a month","Once or twice a week","3-4 times a week","5-6 times a week","Almost every day","Missing")
levels(data$bikeswimtime.factor)=c("Less than 30 minutes","30-60 minutes","1-2 hours","3 or more hours","Missing")
levels(data$latino.factor)=c("Hispanic or Latino","Not Hispanic or Latino","Do not wish to provide this information")
levels(data$white.factor)=c("Selected","Not Selected")
levels(data$black.factor)=c("Selected","Not Selected")
levels(data$asian.factor)=c("Selected","Not Selected")
levels(data$nativeamer.factor)=c("Selected","Not Selected")
levels(data$hawaiian.factor)=c("Selected","Not Selected")
levels(data$notprovided.factor)=c("Selected","Not Selected")
levels(data$ffq_qaqc_kcalok.factor)=c("Within NHANES 5th/95th percentiles (>600/650 and < 4400/5700 for females/males, respectively)","Below NHANES 5th percentile (< 600/650 for females/males, respectively)","Above NHANES 95th percentile (>4400/5700 for females/males, respectively)")
levels(data$ffq_qaqc_foodsperday.factor)=c("Adequate number of foods per day reported: >6 foods per day males/>5 per day females","Warning: < 6 foods per day for males/< 5 foods per day for females reported","Error: < 5 foods per day for males/< 4 foods per day for females reported")
levels(data$ffq_qc_passed.factor)=c("Yes","No")
levels(data$block_ffq_complete.factor)=c("Incomplete","Unverified","Complete")

# write.csv(data, "data/cleaned-data/clean_FL100_FFQ.csv")

View(data)