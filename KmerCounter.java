package utils;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;


public class KmerCounter
{
	private static final Map<String, String> REVERSE_COMPLEMENTS_NT;
//	private static final Map<String, String> REVERSE_COMPLEMENTS_KMERS = new LinkedHashMap<String, String>();
    static
    {
    	REVERSE_COMPLEMENTS_NT = new HashMap<String, String>();
    	REVERSE_COMPLEMENTS_NT.put("A", "T");
    	REVERSE_COMPLEMENTS_NT.put("G", "C");
    	REVERSE_COMPLEMENTS_NT.put("C", "G");
    	REVERSE_COMPLEMENTS_NT.put("T", "A");
    	REVERSE_COMPLEMENTS_NT.put("N", "N");
    }
    
    private static String getReverseComplement (String str)
    {
    	char[] str_charr = str.toCharArray();
    	char[] rev_complement = new char[str_charr.length];
    	for (int i = str_charr.length-1; i >= 0; i --)
    	{
    		rev_complement[str_charr.length-1-i] =  REVERSE_COMPLEMENTS_NT.get(Character.toString(str_charr[i])).charAt(0);
    	}
    	
    	return String.valueOf(rev_complement);
    }
	
	/**
	 * @param args
	 */
	public static void main(String[] args) throws Exception 
	{
		
		//String kmerFile = "/home/si14w/my/kmers.txt";
		//String kmerFile = "/Users/sowmyaiyer/Downloads/kmers.txt";
		String sequenceFile = args[0];
		String outFile = args[1];
		String kmerFile = args[2];
		int k = new Integer(args[3]).intValue();
		
		Map<String, Double>KMER_COUNTS_HASHMAP = new LinkedHashMap<String, Double>();
		
		Scanner sc_kmer = null;
    	try
    	{
    		sc_kmer = new Scanner(new File(kmerFile));
    		while (sc_kmer.hasNextLine())
    		{
    			String kmer = sc_kmer.nextLine();
  //  			REVERSE_COMPLEMENTS_KMERS.put(kmer, getReverseComplement(kmer));
    			// Initialize kmer counts hashmap to populate sequence kmer counts
    			KMER_COUNTS_HASHMAP.put(kmer, 0.0);
    		}
    		sc_kmer.close();
    	} catch (FileNotFoundException fe)
    	{
    		fe.printStackTrace();
    	}
		
		
		// Iterate through each sequence in sequence text file, then for each sequence string slide k-sized window across each sequence
		// update master list as you go along
		FileWriter fw = new FileWriter(outFile);
		fw.write("id\t");
		Iterator<String> it_kmer = KMER_COUNTS_HASHMAP.keySet().iterator();
		StringBuffer sb_kmer_title = new StringBuffer();
		while (it_kmer.hasNext())
		{
			sb_kmer_title.append(it_kmer.next()).append("\t");
		}
		fw.write(sb_kmer_title.append("\n").toString());
		
		Scanner sc_seq = new Scanner(new File(sequenceFile));
		Integer sequence_id = new Integer(1);
		while (sc_seq.hasNextLine())
		{
			String thisSeq = sc_seq.nextLine();
			double seqLen = thisSeq.length();
			StringBuffer kmerCountsString = new StringBuffer(); 
		
			kmerCountsString.append("seq_").append(sequence_id.intValue()).append("\t");
			//Map<String, Double> kmerMap_for_this_seq = sequence_kmer_dist.get("seq_"+sequence_id);
			Map<String, Double> kmerMap_for_this_seq = new LinkedHashMap<String, Double>(KMER_COUNTS_HASHMAP); //initialize to 0 counts for new sequence
			
			for (int i = 0; i < seqLen-k+1; i ++)
			{
				String strInWindow = thisSeq.substring(i, i+k).toUpperCase();
				double count_in_seq = 0.0;
				//boolean kmer_exists_in_table = sequence_kmer_dist.get("seq_"+sequence_id).containsKey(strInWindow);
				boolean kmer_exists_in_table = kmerMap_for_this_seq.containsKey(strInWindow);
				if (kmer_exists_in_table)
				{
					count_in_seq = kmerMap_for_this_seq.get(strInWindow).doubleValue();
					count_in_seq = count_in_seq + (1/seqLen); // divide by seqLen to normalize by sequence length
					kmerMap_for_this_seq.put(strInWindow, count_in_seq);
					
				}
				else
				{
					count_in_seq = kmerMap_for_this_seq.get(getReverseComplement(strInWindow)).doubleValue();
					count_in_seq = count_in_seq + (1/seqLen); // divide by seqLen to normalize by sequence length
					kmerMap_for_this_seq.put(getReverseComplement(strInWindow), count_in_seq);
				}
				
				
			}
			//sequence_kmer_dist.put("seq_"+sequence_id, kmerMap_for_this_seq);
			Iterator<String> kmer_counts_it = KMER_COUNTS_HASHMAP.keySet().iterator();
			while (kmer_counts_it.hasNext())
			{
				String kmer = kmer_counts_it.next();
				kmerCountsString.append(kmerMap_for_this_seq.get(kmer)).append("\t"); 
			}
			
			fw.write(new StringBuffer().append(kmerCountsString).append("\n").toString());
			sequence_id ++;
		}
		fw.close();
		sc_seq.close();
		
	}
}
