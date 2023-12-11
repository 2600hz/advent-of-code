import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class DayOne2 {
	static String maybeReplaceLetter(String line) {
		String[] letters = {"one","two","three","four","five","six", "seven", "eight", "nine"};
		for (String s: letters) {
			if (line.contains(s)) {
				switch (s) {
					case "one":
						return "1";
					case "two":
						return "2";
					case "three":
						return "3";
					case "four":
						return "4";
					case "five":
						return "5";
					case "six":
						return "6";
					case "seven":
						return "7";
					case "eight":
						return "8";
					case "nine":
						return "9";
					default:
						break;
				}
			}
		}
		return line;
	}

	static String calibration(String line) {
		String result = "";
		String onlyLetters = "";
		for (char letter: line.toCharArray()) {
			String str = String.valueOf(letter);
			try {
				Integer.parseInt(str);
				result = result + str;
			} catch (NumberFormatException e) {
				onlyLetters = onlyLetters + str;
				String s = maybeReplaceLetter(onlyLetters);
				if (s != onlyLetters) {
					result = result + s;
					onlyLetters = onlyLetters.substring(onlyLetters.length()-2);
				}
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
