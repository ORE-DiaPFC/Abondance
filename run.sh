# ! /bin/sh
#$ -S /bin/sh

#SITE=Scorff # Nivelle Oir Bresle
YEAR=2017
CHAINS=2
BURNIN=1000 # Number of steps to "burn-in" the samplers.
ITER=5000 # Total number of steps in chains to save.
THIN=1 # Number of steps to "thin" (1=keep every step).

# Change repos here:
REPbase="/home/mbuoro/Documents/RESEARCH/PROJECTS/ORE-DiaPFC/Abundance"
#"/media/ORE/Abundance" 

COUNTER=0

for SITE in   Nivelle #Oir #Scorff  Bresle  
do
     
cd $REPbase/$SITE
     echo $SITE
     
     for STADE in adult tacon smolt   
     do
      if [ -d "$STADE" ]; then # if directory exists...
  # Control will enter here if $DIRECTORY exists.
     echo "|_$STADE"

    cp $STADE/data/data_"$STADE".R $STADE/data/data_"$STADE"_TMP.R
    #sed 's|Rep|'"$REPbase"'|g' -i $STADE/data/data_"$STADE"_TMP.R
    sed 's|SITE|'"$SITE"'|g' -i $STADE/data/data_"$STADE"_TMP.R
    sed 's|STADE|'"$STADE"'|g' -i $STADE/data/data_"$STADE"_TMP.R
    sed 's|YEAR|'"$YEAR"'|g' -i $STADE/data/data_"$STADE"_TMP.R
    
    
    cp $REPbase/analyse.R $STADE/analyse_"$STADE".R
    sed 's|Rep|'"$REPbase"'|g' -i $STADE/analyse_"$STADE".R
    sed 's|SITE|'"$SITE"'|g' -i $STADE/analyse_"$STADE".R
    sed 's|STADE|'"$STADE"'|g' -i $STADE/analyse_"$STADE".R
    sed 's|YEAR|'"$YEAR"'|g' -i $STADE/analyse_"$STADE".R
    sed 's|CHAINS|'"$CHAINS"'|g' -i $STADE/analyse_"$STADE".R
    sed 's|BURNIN|'"$BURNIN"'|g' -i $STADE/analyse_"$STADE".R
    sed 's|ITER|'"$ITER"'|g' -i $STADE/analyse_"$STADE".R
    sed 's|THIN|'"$THIN"'|g' -i $STADE/analyse_"$STADE".R
     

   R CMD BATCH --no-save --no-restore $STADE/analyse_"$STADE".R & # analyse dans R
    
# Save PIDs of processes
#COUNTER=$[COUNTER + 1]
#echo $COUNTER

# CLEANING
    #rm -f $STADE/CODAindex.txt
    #rm -f analyse_"$STADE".R
    #rm -f analyse_"$STADE".Rout
    #rm -f  $STADE/data/data_"$STADE"_TMP.R
    
    fi
    done
 done   
    


