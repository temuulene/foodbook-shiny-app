# Foodbook Variable Reconciliation Report

**Generated:** 2026-01-08

## Summary

| Metric | Count |
|--------|-------|
| Expected exposures (from Excel) | 346 |
| Current exposures (in App) | 233 |
| FB1 microdata columns | 473 |
| FB2 microdata columns | 276 |
| Labels in labeling.do | 411 |

## Breakdown

| Category | Count | Action Required |
|----------|-------|----------------|
| Exact matches | 213 | None |
| Close matches (naming diff) | 18 | Standardize naming |
| Currently excluded | 17 | Review with Megan |
| Missing in labeling.do | 85 | Add to labeling file |
| In labeling but no microdata | 13 | Not available in Foodbook |
| Unknown (needs investigation) | 0 | Debug app loading |

---

## 1. Close Matches (Naming Standardization Needed)

These variables exist in the app but with slightly different names.

| Excel Name | App Name | French |
|------------|----------|--------|
| Other onions | Other onion | Autres oignons |
| Fresh mango | Fresh mangos | Mangues fraîches |
| Frozen mango | Frozen mangos | Mangues surgelées |
| Citrus fruits* | Citrus fruit* | Agrumes* |
| Frozen berries* | Frozen raspberries | Petits fruits congelés* |
| Tahini, including in home-made hummus | Tahini, incuding in home-made hummus | Tahini (pâte de sésame), y compris dans du houmous maison |
| Any beef (not including deli-meat) | Any chicken (not including deli-meats) | Bœuf (excluant les charcuteries) |
| Any pork (not including deli-meat) | Any chicken (not including deli-meats) | Tout porc (sauf les charcuteries) |
| Ham (not including deli-meat)* | Turkey (not including deli-meat) | Jambon (excluant les charcuteries)* |
| Any chicken (not including deli-meat) | Any chicken (not including deli-meats) | Poulet (sauf les charcuteries) |
| Chicken sausage (excluding dried sauage) | Chicken sausage (excluding dried) | Saucisse de poulet (sauf les saucissons) |
| Turkey pieces and parts | Turkey pieces or parts | Dinde en morceaux ou en pièces |
| Other deli-meat | Other deli meat | Autres charcuteries |
| Shrimp/prawns | Shrimp/ prawns | Crevettes/écrevisses |
| Any food or drinks containing cannabis as an ingredient | Any food or drink containing cannabis as an ingredient | Aliments ou boissons contenant du cannabis |
| Frozen pot pie* | Frozen potpie* | Tourtes surgelées* |
| Eggs from farmers market or farm | Egg from farmers market or farm | Consommation d’œufs provenant d’un marché agricole |
| Swim in any natural water | Swim in natural water | Nage dans de l’eau naturelle |

---

## 2. Currently Excluded (Review with Megan)

These variables are in the exclusion list but Megan expects them in the app.

| English | French |
|---------|--------|
| Cherry or grape tomatoes | Tomates cerises ou tomates raisins |
| Mesclun greens | Mesclun (mélange printanier) |
| Cashews | Noix de cajou (sauf le beurre ou la tartinade de cajou) |
| Ground beef consumed raw or undercooked | Bœuf haché consommé cru ou pas assez cuit |
| Steak | Bifteck |
| Bacon* | Bacon* |
| Store-bought breaded chicken | Poulet pané du commerce |
| Turkey bacon* | Bacon de dinde* |
| Smoked fish | Poisson fumé |
| Raw fish | Poisson consommé cru |
| Raw oysters | Huîtres consommées crues |
| Powder* | Préparations pour nourrissons en poudre* |
| Dog | Chiens |
| Cat | Chats |
| Bird | Oiseaux |
| Poultry | Volaille |
| Pig | Cochon |

---

## 3. Missing from Labeling File

These variables are expected but not defined in `foodbook variable labeling.do`.
They may be Toolkit-specific additions or errors in the Excel file.

| English | French |
|---------|--------|
| Tomatoes on a sandwich, burger or taco at restaurant or fast food establishment* | Tomates dans un sandwich, un hamburger ou un taco provenant d'un restaurant ou d'un restaurant-minute* |
| Roma/plum tomatoes* | Tomates Roma/italiennes* |
| Hothouse tomatoes* | Tomates de Serre* |
| Beefsteak tomatoes* | Tomates Cœur de bœuf* |
| Lettuce or leafy greens on a sandwich, burger or taco at restaurant or fast  food establishment* | Laitue ou légumes-feuilles dans un sandwich, un hamburger ou un taco provenant d'un restaurant ou d'un restaurant-minute* |
| Cabbage (includes coleslaw) | Chou (y compris sous forme de salade de chou) |
| Peas (shelled or in pods)* | Pois (écossés ou non)* |
| Any carrots* | Tout type de carottes* |
| Any fresh herbs | Herbes fraîches |
| Fresh tarragon* | Estragon frais* |
| Any spices* | Tout type d'épices* |
| Any store-bought prepared salads?* | Tout type de salades préparées du commerce* |
| Pasta salad (store-bought prepared)* | Salade de pâtes* |
| Dried mango | Mangues séchées |
| Any frozen fruit* | Tout type de fruits congelés* |
| Frozen fruit (not including berries)* | Fruits congelés (excluant les petits fruits)* |
| Unpasteurized fruit juice* | Jus de fruits non pasteurisé* |
| Peanuts (not including peanut butter) | Arachides (sauf le beurre ou la tartinade d’arachide) |
| Almonds (excluding almond butter) | Amandes (sauf le beurre ou la tartinade d’amandes) |
| Hazelnuts (Filberts) | Noisettes (sauf le beurre ou la tartinade de noisette) |
| Other nut paste, butter or spread* | Autres pâtes, beurres ou tartinades de noix* |
| Any seeds | Tout type de graines |
| Tahini, halva or other products made from sesame seeds* | Tahini, halva ou autres produits faits à partir de graines de sésame* |
| Any other ground beef* | Autre bœuf haché* |
| Any hamburgers | Tout type d'hamburgers |
| Store-bought frozen beef patties | Galettes de bœuf achetées congelées |
| Home-made hamburgers* | Hamburgers faites à la maison* |
| Hamburgers from a restaurant or fast food establishment* | Hamburgers provenant d'un restaurant ou d'un restaurant-minute* |
| Chicken from a restaurant or fast food establishment* | Poulet provenant d'un restaurant ou d'un restaurant-minute* |
| Turkey sausage | Saucisses de dinde |
| Any deli-meat/cold cuts | Charcuteries/viandes froides |
| Chicken deli-meat | Charcuterie de poulet |
| Turkey deli-meat | Charcuterie de dinde |
| Ham deli-meat | Charcuterie de jambon |
| Beef deli-meat | Charcuterie de bœuf |
| Any organ meats | Abats |
| Kielbasa* | Saucisse Kielbasa* |
| Shawarma or donair | Shawarma ou viande à donair* |
| Any shellfish | Mollusques |
| Any eggs | Œufs |
| Unpasteurized dairy milk (not including cheese) | Lait non pasteurisé (cru), sauf le fromage |
| Feta | Feta |
| Cheese made from goats milk | Fromage fait de lait de chèvre |
| Cheddar* | Cheddar* |
| Parmesan* | Parmesan* |
| Other cheeses sold as blocks/wheels* | Fromage généralement vendu en bloc /meule* |
| Processed cheese* | Fromage transformé* |
| Blue-veined cheese* | Fromages à pâte persillée* |
| Goat/sheep milk cheese* | Fromage de lait de chèvre/brebis* |
| Any wheat flour | Farine de blé |
| Hummus (excluding home-made) | Houmous (sauf le houmous fait maison) |
| Any baby formula* | Tout type de préparations pour nourrissons* |
| Store-bought pureed baby food* | Aliments en purées pour nourrissons du commerce* |
| Always or sometimes eats organic produce | Consomme toujours ou parfois des produits biologiques |
| Vegan diet | Régime végétalien |
| Vegetarian diet | Régime végétarien |
| Kosher diet | Régime casher |
| Halal diet | Régime halal |
| Drank water supplied to residence | Consommation de l’eau potable fournie à la résidence |
| Consumed water from a private well | Consommation d’eau : puits privé |
| Consumed trucked-in water | Consommation d’eau : eau approvisionnée par des camions-citernes |
| Consumed water from a cistern | Consommation d’eau : citerne |
| Consumed any untreated lake, spring or river water | Consommation d’eau : eau de lac, de source ou de rivière non traitée |
| Other water source | Consommation d’eau : autre source d’eau |
| Raw water consumption* | Avoir bu de l'eau non traitée* |
| Swim or go into any water* | Retrouvé dans un plan d'eau pour nager* |
| Any contact with animals, animal waste, habitat or food* | Contact avec un animal ou avec des excréments, l'habitat ou la nourriture d'un animal* |
| Companion animals present in the home | Animaux de compagnie à la maison |
| Reptile* | Reptile* |
| Amphibian* | Amphibien* |
| Fish or aquarium* | Poisson ou aquarium* |
| Handle pet waste or clean litter box/pet enclosure | Manipulation d’excréments d’animaux de compagnie ou nettoyage de l’enclos pour animaux |
| Contact with pet with diarrhea | Contact avec un animal de compagnie souffrant de diarrhée |
| Handled any dry pet food | Aliments secs pour animaux de compagnie |
| Handled any raw pet food (store-bought or home-made) | Aliments crus pour animaux de compagnie |
| Handled any raw treats derived from animal parts | Produits crus dérivés de parties d’animaux |
| Handled any processed animal treats | Friandises transformées pour animaux |
| Handled any rodents or insects for reptiles* | Manipulation de rongeurs ou d'insectes pour reptiles* |
| Horse (contact with)* | Cheval* |
| Live on farm or country property | Vivre sur une ferme ou une propriété de campagne |
| Visit or work on a farm, petting zoo or fair | Visite ou travail à la ferme, au zoo pour enfants ou à la foire |
| Visited any petting zoo* | Visite d'un zoo pour enfants* |
| Visited any farm or barn* | Visite d'une ferme ou d'une grange* |
| Visited any agricultural fair* | Visite d'une foire agricole* |
| Visited any pet store* | Visite d'une animalerie* |

---

## 4. In Labeling File but No Microdata

These variables have labels defined but the corresponding columns are not in the Foodbook microdata files.
This typically means the variable was planned but not collected.

| English | French | Code |
|---------|--------|------|
| Pâté/meat spread | Pâté/viande à tartiner | `pate` |
| Bologna* | Bologne* | `bologna` |
| Salami* | Salami* | `salami` |
| Pepperoni* | Pepperoni* | `pepperoni` |
| Scallops* | Pétoncles* | `scallops` |
| Crab* | Crabe* | `crab` |
| Lobster* | Homard* | `lobster` |
| Pasteurized dairy milk* | Lait pasteurisé* | `milkpas` |
| Powdered milk product* | Lait en poudre* | `pwdmilk` |
| Whipped/whipping cream* | Crème à fouetter/fouettée* | `whipcream` |
| Sour cream* | Crème sure* | `sourcream` |
| Mozzarella* | Mozzarella* | `mozz` |
| Cottage, ricotta or other fresh cheese* | Cottage, ricotta ou autre fromage frais* | `fcheese` |
