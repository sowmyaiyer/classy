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
	private static final Map<String, String> REVERSE_COMPLEMENTS_KMERS = new HashMap<String, String>();
    static
    {
    	REVERSE_COMPLEMENTS_NT = new HashMap<String, String>();
    	REVERSE_COMPLEMENTS_NT.put("A", "T");
    	REVERSE_COMPLEMENTS_NT.put("G", "C");
    	REVERSE_COMPLEMENTS_NT.put("C", "G");
    	REVERSE_COMPLEMENTS_NT.put("T", "A");
    	REVERSE_COMPLEMENTS_NT.put("N", "N");
    	String kmerFile = "/home/si14w/scratch/kmers_6.txt";
    	//String kmerFile = "/Users/sowmyaiyer/Downloads/kmers.txt";
    	Scanner sc_kmer = null;
    	try
    	{
    		sc_kmer = new Scanner(new File(kmerFile));
    		while (sc_kmer.hasNextLine())
    		{
    			String kmer = sc_kmer.nextLine();
    			REVERSE_COMPLEMENTS_KMERS.put(kmer, getReverseComplement(kmer));
    		}
    		sc_kmer.close();
    	} catch (FileNotFoundException fe)
    	{
    		fe.printStackTrace();
    	}
		
		
    	
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
		
		final List<String> kmers = new ArrayList<String>();
		List<String> sequences = new ArrayList<String>();
		
		Map<String, StringBuffer> kmer_dist = new LinkedHashMap<String, StringBuffer>(); 
		
		Scanner sc_kmer = new Scanner(new File(kmerFile));
		while (sc_kmer.hasNextLine())
		{
			String kmer = sc_kmer.nextLine();
			kmer_dist.put(kmer, new StringBuffer());
			kmers.add(kmer);
		}
		sc_kmer.close();
		
		Scanner sc_seq = new Scanner(new File(sequenceFile));
		while (sc_seq.hasNextLine())
		{
			sequences.add(sc_seq.nextLine());
		}
		sc_seq.close();
		
		// Iterate through each kmer in kmer file, then for each sequence string slide k-sized window across each sequence
		// update master list as you go along
		FileWriter fw = new FileWriter(outFile);
		Iterator<String> it = kmer_dist.keySet().iterator();
		while (it.hasNext())
		{
			String thisKmer = it.next();
			
			StringBuffer thisKmerDist = kmer_dist.get(thisKmer);
			int windowSize = thisKmer.length();

			Iterator<String> it_seq = sequences.iterator();
			while (it_seq.hasNext())
			{
				String thisSeq = it_seq.next();
				double seqLen = thisSeq.length();
				double count_in_seq = 0;

				for (int i = 0; i < seqLen-windowSize+1; i ++)
				{
					String strInWindow = thisSeq.substring(i, i+windowSize).toUpperCase();
					if (strInWindow.equals(thisKmer) || strInWindow.equals(REVERSE_COMPLEMENTS_KMERS.get(thisKmer)))
					{
						count_in_seq ++;
					}
				}
				//System.out.println(seqLen);
				//System.out.println(count_in_seq);
				thisKmerDist.append(count_in_seq/seqLen).append("\t"); //normalize number of occurrences by length of sequence
			}
			fw.write(new StringBuffer().append(thisKmer).append("\t").append(thisKmerDist).append("\n").toString());
		}
		fw.close();
		
	}

}
