storm_data <- read.csv('repdata_data_StormData.csv')
storm_data

#1)  Across the United States, which types of events (as indicated in the EVTYPE 
#variable) are most harmful with respect to population health?

#a) aggregating EVTYPE wrt injuries
library(dplyr)
total_injuries <- aggregate(INJURIES~EVTYPE, storm_data, sum)
total_injuries <- arrange(total_injuries, desc(INJURIES))
total_injuries <- total_injuries[1:20, ]
total_injuries

#b) aggregating EVTYPE wrt fatalities
total_fatalities <- aggregate(FATALITIES~EVTYPE,storm_data, sum)
total_fatalities <- arrange(total_fatalities, desc(FATALITIES))
total_fatalities <- total_fatalities[1:20, ]
total_fatalities

# Plot of Top 10 Fatalities & Injuries for Weather Event Types ( Population Health Impact )
par(mfrow=c(1,2),mar=c(10,3,3,2))
barplot(total_fatalities$FATALITIES,names.arg=total_fatalities$EVTYPE,las=2,col="maroon",ylab="fatalities",main="Top 10 fatalities")
barplot(total_injuries$INJURIES,names.arg=total_injuries$EVTYPE,las=2,col="maroon",ylab="injuries",main="Top 10 Injuries")


#2. Across the United States, which types of events have the greatest economic 
#consequences?

# we have property Damage and crop damage 
# Aggregate Data for Property Damage

property_damage <- aggregate(PROPDMG ~ EVTYPE, data = storm_data, FUN = sum)
property_damage <- property_damage[order(property_damage$PROPDMG, decreasing = TRUE), ]
# 10 most harmful causes of injuries
property_damageMax <- property_damage[1:10, ]
print(property_damageMax)

#Aggregate Data for Crop Damage
crop_damage <- aggregate(CROPDMG ~ EVTYPE, data = storm_data, FUN = sum)
crop_damage <- crop_damage[order(crop_damage$CROPDMG, decreasing = TRUE), ]
# 10 most harmful causes of injuries
crop_damageMax <- crop_damage[1:10, ]
print(crop_damageMax)


##plot the graph showing the top 10 property and crop damages

par(mfrow=c(1,2),mar=c(11,3,3,2))
barplot(property_damageMax$PROPDMG/(10^5),names.arg=property_damageMax$EVTYPE,las=3,col="green",ylab="Property damage(billions)",main="Top10 Property Damages")
barplot(crop_damageMax$CROPDMG/(10^5),names.arg=crop_damageMax$EVTYPE,las=3,col="green",ylab="Crop damage(billions)",main="Top10 Crop Damages")


