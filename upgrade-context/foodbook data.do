/*Import Foodbook 1 data*/

use foodbook.dta

/*keep variables that are not in Foodbook 2*/

keep QINTRO3 AGE gender month_dv EXPWEIGHT_CMA2 ///
Q81_A_dv Q36_E_dv Q81_B_dv Q90 Q102_A Q102_C_dv Q102_D_dv Q102_E_dv Q102_F_dv Q105_A_dv Q105_B_dv Q105_C_dv Q105_H_dv Q105_I_dv  ///
Q105_F_dv Q105_K_dv Q36_C_dv Q36_D_dv Q19 Q20 Q22 Q23 Q24 Q25 Q26 Q27 Q29 Q106_A Q32 Q36_E_dv Q_38 Q_39 Q_42 Q_43 Q_44 Q_45 Q_46 Q_47 Q_50  ///
Q_52 Q_54 Q37A Q71 Q73 Q106_D Q106_E Q106_F Q106_G Q107_A Q107_B Q107_C Q107_D Q107_E Q107_F Q133_B_dv Q89_B


rename QINTRO3 PT
rename AGE age
rename gender Gender
rename month_dv Month

rename Q19 celery
rename Q20 carrot
rename Q22 pea
rename Q23 gybeans
rename Q24 broccoli
rename Q25 cauliflo
rename Q26 leek
rename Q27 garlic
rename Q29 zucchini
rename Q32 vegjuice
rename Q36_C_dv psaladstor
rename Q36_D pastasal
rename Q36_E_dv fruitsal
rename Q37A salsa
rename Q_38 apple
rename Q_39 pear
rename Q_42 apricot
rename Q_43 plum
rename Q_44 citrus 
rename Q_45 cherry
rename Q_46 grape
rename Q_47 banana
rename Q_50 kiwi
rename Q_52 pineapple
rename Q_54 olive
rename Q71 hambhome
rename Q73 hambrest
rename Q81_A ham
rename Q81_B bacon
rename Q90 shawarma
rename Q89_B sausage
rename Q102_A milkpas
rename Q102_C pwdmilk
rename Q102_D whipcream
rename Q102_E sourcream
rename Q105_A cheddar
rename Q105_B mozz
rename Q105_C parm
rename Q105_F hrdcheese
rename Q105_H bvcheese
rename Q105_I fcheese
rename Q105_K procheese
rename Q106_A vegez
rename Q106_D pizza
rename Q106_E potpie
rename Q106_F frozenm
rename Q106_G frozens
rename Q107_A dryfruit
rename Q107_B granola
rename Q107_C chips
rename Q107_D choc
rename Q107_E coldbc
rename Q107_F hotbc
rename Q133_B_dv animfdwet

drop Q*

/*append Foodbook 2 data file*/

append using "foodbook2v2.dta"

*Vegetables and herbs
rename Q4_dv anytom
rename Q4A_dv tomcherry
rename Q5_dv anylettuce
rename Q5A_dv iceberg
rename Q5B_dv romaine
rename Q5C_dv spinach
rename Q5D_dv mesclun
rename Q5E_dv kale
rename Q5F_dv arugula
rename Q5G_dv olettuce
rename Q6 prepacklg
rename Q7 saladkit
rename Q8_dv cabbage
rename Q8A_dv coleslaw
rename Q9 greensal
rename Q10 minicar
rename Q11_dv sprouts /*FB2 includes microgreens*/
rename Q11A_dv microgreens
rename Q11B_dv alfalfa
rename Q11C_dv beansprouts
rename Q11D_dv osprout
rename Q12 cucumbers
rename Q13 bpeppers
rename Q14 hotpeppers
rename Q15 snappea
rename Q16 mushroom
rename Q17_dv onions
rename Q17B_dv redonion
rename Q17A_dv grnonion
rename Q17C_dv oonion
rename Q18 anybasil
rename Q19 cilantro
rename Q20 parsley
rename Q21 ofherb

*Fruits
rename Q22_dv mango
rename Q22A_dv mango_f
rename Q22B_dv mango_z
rename Q22C_dv mangod
rename Q23_dv papaya
rename Q23A_dv papaya_f
rename Q23B_dv papaya_z
rename Q23C_dv papaya_d
rename Q24_dv pomegra
rename Q24A_dv pomegra_f
rename Q24B_dv pomegra_z
rename Q25_dv avocado
rename Q25A_dv avocado_f
rename Q25B_dv avocado_z
rename melon_dv melon
rename Q26_dv cantaloup
rename Q26A_dv cantaloup_f
rename Q26B_dv cantaloup_z
rename Q27_dv honeydew
rename Q27A_dv honeydew_f
rename Q27B_dv honeydew_z
rename Q28_dv watermel
rename Q28A_dv watermel_f
rename Q28B_dv watermel_z
rename Q29_dv peach
rename Q29A_dv peach_f
rename Q29B_dv peach_z
rename Q30_dv nectarine
rename Q30A_dv nectarine_f
rename Q30B_dv nectarine_z
rename berries_dv anyberry
rename Q31_dv strawb
rename Q31A_dv strawb_f
rename Q31B_dv strawb_z
rename Q31C_dv strawb_d
rename Q32_dv raspb
rename Q32A_dv raspb_f
rename Q32B_dv raspb_z
rename Q32C_dv raspb_d
rename Q33_dv blueb
rename Q33A_dv blueb_f
rename Q33B_dv blueb_z
rename Q33C_dv blueb_d
rename Q34_dv blackb
rename Q34A_dv blackb_f
rename Q34B_dv blackb_z
rename Q34C_dv blackb_d
rename Q35_dv coconut
rename Q35A_dv coconut_f
rename Q35B_dv coconut_z
rename Q35C_dv coconut_d
rename Q36 fruitz /*bag of mixed frozen fruit or berries*/
rename Q38 unpjuice /*unpasteurized apple cider or fruit juice*/
rename Q37 fruitsm

rename Q39_dv anynuts
rename Q39A_dv peanut
rename Q39B_dv pnbutter
rename Q39C_dv almond
rename Q39D_dv almondbutter
rename Q39E_dv walnut
rename Q39F_dv hazelnut
rename Q39G_dv hazelnutbutter
rename Q39H_dv cashew
rename Q39I_dv cbutter
rename Q39J_dv pecan
rename Q39K_dv onut
rename Q40 sunflwsd
rename Q41 sunbutter
rename Q42 sesamesd
rename Q43 tahini
rename Q44 chia
rename Q45 flax
rename Q46 oseed


rename Q47_dv anybeef 
rename Q47A_dv anygroundbeef
rename Q47AI_dv bgroundraw
rename Q47B_dv hamb
rename Q47BI_dv zbeefpatties
rename Q47C_dv rawb
*rename Q47D_dv steak
rename Q47D_dv bwhole
rename Q47E_dv stewingb
rename Q47F_dv veal
rename Q47G_dv sausage_beef
*rename Q47H_dv bwhole
rename Q47H_dv obwhole
*generate bwhole = 1 if obwhole == 1 | steak == 1 | stewingb == 1
*replace bwhole = 2 if obwhole == 2 & steak == 2 & stewingb == 2

rename Q48_dv anypork
rename Q48A_dv pground
rename Q48B_dv sausage_pork
rename Q48C_dv porkparts
rename Q48D_dv rpork


rename Q49_dv anychick
rename Q49A_dv cbreaded
rename Q49AI_dv zcbreaded
rename Q49AB_dv cstuffed
rename Q49ABI_dv zcstuffed
rename Q49B_dv cground
rename Q49C_dv wholec
rename Q49D_dv rawwholec
rename Q49E_dv chickparts
rename Q49F_dv sausage_chick
rename Q50_dv anyturk
rename Q50A_dv tground
rename Q50B_dv sausage_turk
rename Q50C_dv wholet
rename Q50D_dv turkparts
rename Q51 poultryo

rename Q52_dv deliany
rename Q52A_dv chickdeli
rename Q52B_dv turkdeli
rename Q52C_dv hamdeli
rename Q52D_dv beefdeli
rename Q52E_dv odeli
rename Q53 hotdog
rename Q54 pate
rename Q55_dv organ
rename Q55A_dv vliver
rename Q55AI_dv rawliver
rename Q55B_dv oorgan
rename Q56 goat
rename Q57 lamb
rename Q58 horse
rename Q59 driedmeat
rename Q67 deer
rename Q68 huntedmeats 

rename Q69_dv seafood
rename Q69A_dv anyfish
rename Q69AI_dv fishsmoke
rename Q69AII_dv fishraw
rename Q69B_dv shellfish
rename Q69BI_dv mussels
rename Q69BII_dv clams
rename Q69BIII_dv shrimp /*shrimp/prawns*/
rename Q69BIV_dv oysters
rename Q69BIV_1_dv oysters_r

rename Q70_dv egg
rename Q70A_dv eggraw

rename Q71 icecream
rename Q72 milkdess
rename Q73 yogurt
rename Q74 milkunpas
rename Q75_dv anycheese
rename Q75A_dv gouda
rename Q75B_dv feta
rename Q75C_dv scheese 
rename Q75D_dv gcheese /*FB 2 JUST GOAT*/ 
rename Q75E_dv cheeseunpas
rename Q76 milko
rename Q77 nondairyo

rename Q78 flour
rename Q79 dough
rename Q80 oflour

rename Q81_dv pbmeat
rename Q81A_dv tofu
rename Q81B_dv soyprod
rename Q82 hummus

rename Q83_dv supp
rename Q83A_dv mealrepbev
rename Q83B_dv protpowder
rename Q83C_dv probiotics
rename Q84 cannabis

rename Q85 butcher
rename Q86_dv farmfood
rename Q86A_dv farmproduce
rename Q86B_dv farmegg
rename Q86C_dv farmmeat
rename Q86D_dv farmdairy
rename Q87_dv mealkit
rename Q87A_dv goodfood
rename Q87B_dv hellofresh
rename Q87C_dv chefsplate
rename Q87D_dv omealkit

rename Q91 swim
 
rename Q93_dv domanim 
rename Q97_dv domanim_cat
rename Q96_dv domanim_dog
rename Q102_dv domanim_bird
rename Q98_dv domanim_reptile
rename Q99_dv domanim_rodent
rename Q100_dv domanim_rabbit
rename Q101_dv domanim_hedgehog
rename Q108_dv ndomanim
rename Q109_dv cattle 
rename Q112_dv goat_sheep
rename Q111_dv pig 
rename Q110_dv poultry
rename Q103A_dv animfd
rename Q103_dv animfddry
rename Q104_dv animfdraw
rename Q105_dv animfdparts
rename Q106_dv animfdproc
rename Q107_dv animfdrodent

rename Q90_C4 water_bottled
rename Q90_C1 water_municipal

/*Merge weight variables from Foodbook 1 and 2*/

gen weight = EXPWEIGHT_CMA2
replace weight = proj_weight_non_traveller if weight == .

*do "foodbook OMD variable labeling.do"
