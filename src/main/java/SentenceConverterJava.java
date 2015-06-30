
import bioc.BioCAnnotation;
import bioc.BioCPassage;
import bioc.util.CopyConverter;

public class SentenceConverterJava extends CopyConverter {
	@Override
	//CopyConverter.javadaki getPassage methodu override edildi. 
	//getMatchingSentence methodunda dondurulen BioCAnnotation objesi out'a eklenir.
	public BioCPassage getPassage(BioCPassage in) {
		BioCPassage out = new BioCPassage();
		out.setOffset(in.getOffset());
		out.setInfons(in.getInfons());
		out.setText(in.getText());
		int countAnnSentence=0;
		BioCAnnotation newAnnotation = null;
		String text = in.getText();
		int current = 0;
		int period = text.indexOf(". ", current);
		while (period > -1) {
			String sentence;
			sentence = text.substring(current, period + 1);
			//newAnnotation = getMatchingSentences(sentence);

			if (newAnnotation != null){
				
				if(countAnnSentence==0)
					out.addAnnotation(newAnnotation);
				else
				{
					out.getAnnotation(0).setText(out.getAnnotation(0).getText().concat(newAnnotation.getText()));
				}
				
				countAnnSentence++;

			}

			current = period + 2; // advance to next sentence
			while (current < text.length() && text.charAt(current) == ' ') {
				++current; // skip extra spaces
			}
			if (current >= text.length()) {
				current = -1; // flag for fell off end
				period = -1;
			} else {
				period = text.indexOf(". ", current);
			}
			

		}
		
	
		return out;
	}
}

