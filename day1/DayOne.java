import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class DayOne {
	static String calibration(String line) {
		String result = "";
		for (char letter: line.toCharArray()) {
			try {
				String str = String.valueOf(letter);
				Integer.parseInt(str);
				result = result + str;
			} catch (NumberFormatException e) {
			}
		}
		if (result.length() > 2) {
			return(result.substring(0,1)+result.substring(result.length() - 1));
		} else if (result.length() < 2){
			return(result + result);
		} else {
			return(result);
		}
	}

	public static void main(String[] args) {
		int total = 0;
		try {
			File file = new File("day1/input.txt");
			Scanner fileReader = new Scanner(file);
			while (fileReader.hasNextLine()) {
				String data = fileReader.nextLine();
				data = calibration(data);
				total = total + Integer.parseInt(data);
			}
			fileReader.close();
			} catch (FileNotFoundException err) {
				err.printStackTrace();
			}
			System.out.println(total);
		}
}
