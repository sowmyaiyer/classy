sed -i 's/^<pre>//g' enhancer_lbl.V4.fa
grep "^>" enhancer_lbl.V4.fa | awk -F"|" '{print}' | sed 's/ //g' | sed 's/>//g' | sed 's/element//g'  | sed 's/\[[0-9]*[0-9]\/[0-9]*[0-9]*\]//g' | awk -F"|" '
{
        if ($1 == "Human")
                sp="hs"
        else if ($1 == "Mouse")
                sp="mm"
        printf("%s%s\t%s\t%s\t",sp,$3,$4,$1);
        if ($4 == "positive")
        {
                for (i=5; i<= NF; ++i)
                {
                        printf("%s",$i)
                        if (i < NF)
                        { printf(",") }
                }
        }
        else {
                printf(".")
        }
        printf("\n")
}' > id_tissues.txt
sort -k1,1 lbl_data.txt > lbl_data.sorted.txt
sort -k1,1 id_tissues.txt > id_tissues.sorted.txt

join lbl_data.sorted.txt id_tissues.sorted.txt  | sed 's/ /\t/g' | sed 's/\:/\t/g' | sed 's/\-/\t/g' | awk '{ printf("%s\t%s\t%s\t%s\t%s\t%s\t%s\n",$2,$3,$4,$1,$5,$7,$8)}' | sort -k1,1 -k2,2n > enhancer_lbl_formatted.V4.bed


grep "negative" enhancer_lbl_formatted.V4.bed > mouse_enhancer_negative.4.bed
grep "positive" enhancer_lbl_formatted.V4.bed > mouse_enhancer_positive.4.bed
cat mouse_enhancer_positive.4.bed mouse_enhancer_negative.4.bed | sort -k1,1 -k2,2n > mouse_enhancer_positive_and_negative_sorted.V4.bed

grep "heart" enhancer_lbl_formatted.V4.bed > mouse_heart_positive.4.bed
grep -v "heart" enhancer_lbl_formatted.V4.bed | sed 's/positive/negative/g' > mouse_heart_negative.4.bed
cat mouse_heart_positive.4.bed mouse_heart_negative.4.bed | sort -k1,1 -k2,2n  > mouse_heart_positive_and_negative_sorted.V4.bed

grep "brain" enhancer_lbl_formatted.V4.bed > mouse_brain_positive.4.bed
grep -v "brain" enhancer_lbl_formatted.V4.bed | sed 's/positive/negative/g' > mouse_brain_negative.4.bed
cat mouse_brain_positive.4.bed mouse_brain_negative.4.bed | sort -k1,1 -k2,2n  > mouse_brain_positive_and_negative_sorted.V4.bed

grep "forebrain" enhancer_lbl_formatted.V4.bed > mouse_forebrain_positive.4.bed
grep -v "forebrain" enhancer_lbl_formatted.V4.bed | sed 's/positive/negative/g' > mouse_forebrain_negative.4.bed
cat mouse_forebrain_positive.4.bed mouse_forebrain_negative.4.bed | sort -k1,1 -k2,2n > mouse_forebrain_positive_and_negative_sorted.V4.bed

grep "hindbrain" enhancer_lbl_formatted.V4.bed > mouse_hindbrain_positive.4.bed
grep -v "hindbrain" enhancer_lbl_formatted.V4.bed | sed 's/positive/negative/g' > mouse_hindbrain_negative.4.bed
cat mouse_hindbrain_positive.4.bed mouse_hindbrain_negative.4.bed | sort -k1,1 -k2,2n > mouse_hindbrain_positive_and_negative_sorted.V4.bed

grep "midbrain" enhancer_lbl_formatted.V4.bed > mouse_midbrain_positive.4.bed
grep -v "midbrain" enhancer_lbl_formatted.V4.bed | sed 's/positive/negative/g' > mouse_midbrain_negative.4.bed
cat mouse_midbrain_positive.4.bed mouse_midbrain_negative.4.bed | sort -k1,1 -k2,2n > mouse_midbrain_positive_and_negative_sorted.V4.bed

grep "neuraltube" enhancer_lbl_formatted.V4.bed > mouse_neuraltube_positive.4.bed
grep -v "neuraltube" enhancer_lbl_formatted.V4.bed | sed 's/positive/negative/g' > mouse_neuraltube_negative.4.bed
cat mouse_neuraltube_positive.4.bed mouse_neuraltube_negative.4.bed | sort -k1,1 -k2,2n > mouse_neuraltube_positive_and_negative_sorted.V4.bed

grep "limb" enhancer_lbl_formatted.V4.bed > mouse_limb_positive.4.bed
grep -v "limb" enhancer_lbl_formatted.V4.bed | sed 's/positive/negative/g' > mouse_limb_negative.4.bed
cat mouse_limb_positive.4.bed mouse_limb_negative.4.bed | sort -k1,1 -k2,2n > mouse_limb_positive_and_negative_sorted.V4.bed

grep "kidney" enhancer_lbl_formatted.V4.bed > mouse_kidney_positive.4.bed
grep -v "kidney" enhancer_lbl_formatted.V4.bed | sed 's/positive/negative/g' > mouse_kidney_negative.4.bed
cat mouse_kidney_positive.4.bed mouse_kidney_negative.4.bed | sort -k1,1 -k2,2n > mouse_kidney_positive_and_negative_sorted.V4.bed

for othertissue in {"eye","nose","branchialarch","nose","cranialnerve","facialmesenchyme","trigeminalV(ganglion,cranial)","dorsalrootganglion"}
do
	grep ${othertissue}  enhancer_lbl_formatted.V4.bed > mouse_${othertissue}_positive.4.bed
	grep -v ${othertissue} enhancer_lbl_formatted.V4.bed | sed 's/positive/negative/g' > mouse_${othertissue}_negative.4.bed
	cat mouse_${othertissue}_positive.4.bed mouse_${othertissue}_negative.4.bed | sort -k1,1 -k2,2n > mouse_${othertissue}_positive_and_negative_sorted.V4.bed
done

