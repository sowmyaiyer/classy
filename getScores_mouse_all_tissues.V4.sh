#!/bin/bash
# This script creates a master table for classification for each tissue. Rows are tested enhancers from enhancer.lbl.gov. Columns are histone mod signals. Last column is tissue-specific label (for example, for heart, heart+ enhancers will have 1, all others will have 0

#for tissue in {"forebrain","neuraltube","heart","limb","hindbrain","midbrain"}
for tissue in {"forebrain","heart"}
#for tissue in {"forebrain","neuraltube","heart","limb"}
do
	echo $tissue
	echo chr        start   end     id      label   species tissues > ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.new.V4.bed
	sort -k4,4V /home/si14w/nearline/enhancer_predictions/mouse_${tissue}_positive_and_negative_sorted.V4.bed  | sed 's/negative/0/' | sed 's/positive/1/' >> ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.new.V4.bed
	for age in {"embryonic_11.5day","embryonic_14.5day","postnatal_0day"}
	#for age in {"embryonic_11.5day","embryonic_14.5day"}
	do
		for peakFile in `ls ~/nearline/enhancer_predictions/new_encode_data/*_mm_${age}_*${tissue}*_VS_*_mm_${age}_*${tissue}*_*_Control.macs2_output_peaks.narrowPeak`
		do
                       featurename=`basename $peakFile | cut -d"_" -f2,3,4,5,7`
                        echo $featurename
                        bwFile=`basename $peakFile | sed 's/output_peaks\.narrowPeak/output_logLR\.bw/'`

                        # Intersect training intervals with peak file.
			echo "bedtools intersect -b ~/nearline/enhancer_predictions/mouse_${tissue}_positive_and_negative_sorted.V4.bed -a $peakFile -wb"
                        bedtools intersect -b ~/nearline/enhancer_predictions/mouse_${tissue}_positive_and_negative_sorted.V4.bed -a $peakFile -wb  | awk '{ print $1"\t"$2"\t"$3"\t"$7"\t"$11"\t"$12"\t"$13"\t"$14"\t"$15"\t"$16 }' > ~/nearline/enhancer_predictions/new_encode_data/${featurename}_intersect_mouse_${tissue}_training.new.V4.bed
                        # For intervals that intersect a peak, the score will be a weighted sum (score of peak * number of intersecting bases)
                        if [[ -s  ~/nearline/enhancer_predictions/new_encode_data/${featurename}_intersect_mouse_${tissue}_training.new.V4.bed ]] ; then
                                Rscript getWeightedScores.R $HOME/nearline/enhancer_predictions/new_encode_data/${featurename}_intersect_mouse_${tissue}_training.new.V4.bed ${featurename} /home/si14w/nearline/enhancer_predictions/new_encode_data/${featurename}.new.V4.weightedscore
                        else
                                echo zero sized file ~/nearline/enhancer_predictions/new_encode_data/${featurename}_intersect_mouse_${tissue}_training.new.V4.bed
                                cat /dev/null > /home/si14w/nearline/enhancer_predictions/new_encode_data/${featurename}.new.V4.weightedscore
                        fi
                        # For intervals that don't intersect with a peak, the score will be cumulative signal in region (not average)
                        bedtools intersect -a ~/nearline/enhancer_predictions/mouse_${tissue}_positive_and_negative_sorted.V4.bed -b $peakFile -v | awk '{ printf("%s\t%d\t%d\t%s\n",$1,$2,$3,$4 ) }' >  ~/nearline/enhancer_predictions/new_encode_data/${featurename}_no_intersect_mouse_${tissue}_training.new.V4.bed
                        bigWigAverageOverBed ~/nearline/enhancer_predictions/new_encode_data/${bwFile} ~/nearline/enhancer_predictions/new_encode_data/${featurename}_no_intersect_mouse_${tissue}_training.new.V4.bed stdout | cut -f1,4  > ~/nearline/enhancer_predictions/new_encode_data/${featurename}_no_intersect_mouse_${tissue}_training.new.V4.bed.score
                        echo ${featurename} > ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores_${featurename}.new.V4.bed
                        # concatenate rows with peak intersections and rows with no pean intersections, sort by element name
                        cat ~/nearline/enhancer_predictions/new_encode_data/${featurename}_no_intersect_mouse_${tissue}_training.new.V4.bed.score ~/nearline/enhancer_predictions/new_encode_data/${featurename}.new.V4.weightedscore | sort -k1,1V | awk '{ print $2}' >> ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores_${featurename}.new.V4.bed
                        paste ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.new.V4.bed ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores_${featurename}.new.V4.bed > ~/nearline/enhancer_predictions/new_encode_data/temp
                        mv ~/nearline/enhancer_predictions/new_encode_data/temp ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.new.V4.bed		
		done
	done
	awk '{ printf("%s\t",$4); for (e = 8; e < NF; e ++) { printf("%s\t", $e) } printf ("%s\n", $5) }'  ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.new.V4.bed > ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.new.temp
	head -1 ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.new.temp > header.${tissue}
	awk '{ if (NR > 1) print }' ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.new.temp | sort -k1,1V > data.${tissue}
	cat header.${tissue} data.${tissue} >  ~/nearline/enhancer_predictions/new_encode_data/${tissue}_master_table_training.V4.new.tab
	rm ~/nearline/enhancer_predictions/new_encode_data/${tissue}_feature_scores.new.temp header.${tissue} data.${tissue}
	echo done master for ${tissue}
done

#for tissue in {"forebrain","neuraltube","heart","limb","hindbrain","midbrain"}
#do
#	echo -e 'glm\tsvm\tgbm\tgam\tnb\trandomForest\tglm_noreg\tensemble' >  $HOME/TR/classy/${tissue}.V4.new.allages
#	for i in {1..30}
#	do
#		echo $i of 30
#		Rscript $HOME/TR/classy/super.R $HOME/nearline/enhancer_predictions/new_encode_data/${tissue}_master_table_training.V4.tab raw $HOME/TR/classy/${tissue}.V4.new.allages
#	done
#done
