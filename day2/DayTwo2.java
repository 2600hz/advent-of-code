import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.HashMap;

public class DayTwo2 {

	static Integer castToInt(String line) {
		String number = "";
		for (char c: line.toCharArray()) {
			try {
				String tmp = String.valueOf(c);
				Integer.parseInt(tmp);
				number = number + tmp;
			} catch (NumberFormatException e) {
			}
		}
		try {
			return Integer.parseInt(number);
		} catch (NumberFormatException e) {
			return 0;
		}
	}

	static HashMap<String, Integer> countGames(String[] game) {
		HashMap<String, Integer> scores = new HashMap<String, Integer>();
		scores.put("blue", 1);
		scores.put("red", 1);
		scores.put("green", 1);
		for (String currentGame: game) {
			String s = "";
			int i = 0;
			if (currentGame.contains("blue")) {
				s = currentGame.split("blue")[0];
				i = castToInt(s.trim());
				scores.put("blue", i);
			} else if (currentGame.contains("red")) {
				s = currentGame.split("red")[0];
				i = castToInt(s.trim());
				scores.put("red", i);
			} else if (currentGame.contains("green")) {
				s = currentGame.split("green")[0];
				i = castToInt(s.trim());
				scores.put("green", i);
			}

		}
		return scores;
	}
	
	static Integer splipSetOfCubes(String line) {
		HashMap<String, Integer> scoresT = new HashMap<String, Integer>();
		scoresT.put("blue", 1);
		scoresT.put("red", 1);
		scoresT.put("green", 1);

		for (String b: line.split(";")) {
			HashMap<String, Integer> scoresG = countGames(b.split(","));
			if (scoresT.get("blue") < scoresG.get("blue")) {
					scoresT.put("blue", scoresG.get("blue"));
			}
			if (scoresT.get("red") < scoresG.get("red")) {
				scoresT.put("red", scoresG.get("red"));
			}  
			if (scoresT.get("green") < scoresG.get("green")) {
				scoresT.put("green", scoresG.get("green"));
			}
		}
		return scoresT.get("blue") * scoresT.get("red") * scoresT.get("green");
	}

	static Integer loadCubes(String line) {
		String[] gameStr = line.split(":", 2);
		int result = splipSetOfCubes(gameStr[1]);
		return result;
	}

	public static void main(String[] args) {
		int total = 0;
		try {
			File file = new File("data/input.txt");
			Scanner fileReader = new Scanner(file);
			while (fileReader.hasNextLine()) {
				String data = fileReader.nextLine();
				int i = loadCubes(data);
				total = total + i;
			}
			fileReader.close();
			} catch (FileNotFoundException err) {
				err.printStackTrace();
			}
			System.out.println(total);
	}

}
