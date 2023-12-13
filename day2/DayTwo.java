import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class DayTwo {
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
		return Integer.parseInt(number);
	}

	static Integer countGames(String game) {
		for (String c: game.split(",")) {
			int i = castToInt(c);
			if (c.contains("blue") && i > 14) {
				return 0;
			} else if(c.contains("red") && i > 12) {
				return 0;
			} else if (c.contains("green") && i > 13) {
				return 0;
			}
		}
		return 1;
	}
	
	static Integer splipSetOfCubes(String line) {
		String[] games = line.split(";");
		for (String g: games) {
			if (countGames(g) == 0) {
				return 0;
			}
		}
		return 1;
	}

	static Integer loadCubes(String line) {
		String[] gameStr = line.split(":", 2);
		int result = splipSetOfCubes(gameStr[1]);
		if (result == 1) {
			return castToInt(gameStr[0]);
		} else {
			return 0;
		}
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
