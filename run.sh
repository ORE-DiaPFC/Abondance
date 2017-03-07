# ! /bin/sh
#$ -S /bin/sh

#SITE=Scorff # Nivelle Oir Bresle
YEAR=2016
BURNIN=5000 # Number of steps to "burn-in" the samplers.
ITER=50000 # Total number of steps in chains to save.
THIN=1 # Number of steps to "thin" (1=keep every step).


REPbase="/home/basp-meco88/Documents/RESEARCH/PROJECTS/ORE/Abundance"
#"/media/ORE/Abundance" 

for SITE in Bresle #Oir Nivelle Scorff
do
     
cd $REPbase/$SITE
     echo $SITE
     
     for STADE in tacon smolt adult
     do
      if [ -d "$STADE" ]; then # if directory exists...
  # Control will enter here if $DIRECTORY exists.
     echo "|_$STADE"

    cp $REPbase/analyse.R $STADE/analyse_"$STADE".R
    sed 's|Rep|'"$REPbase"'|g' -i $STADE/analyse_"$STADE".R
     sed 's|SITE|'"$SITE"'|g' -i $STADE/analyse_"$STADE".R
     sed 's|STADE|'"$STADE"'|g' -i $STADE/analyse_"$STADE".R
     sed 's|YEAR|'"$YEAR"'|g' -i $STADE/analyse_"$STADE".R
     sed 's|BURNIN|'"$BURNIN"'|g' -i $STADE/analyse_"$STADE".R
     sed 's|ITER|'"$ITER"'|g' -i $STADE/analyse_"$STADE".R
     sed 's|THIN|'"$THIN"'|g' -i $STADE/analyse_"$STADE".R

     #R CMD BATCH --no-save --no-restore $STADE/analyse_"$STADE".R & # analyse dans R
    
    rm -f $STADE/CODAindex.txt
    #rm -f analyse_"$STADE".R
    #rm -f analyse_"$STADE".Rout  
    fi
    done
 done   
    
