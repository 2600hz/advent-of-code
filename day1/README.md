## Puzzle #1

#### How to run the code?

You need to have php installed. Then simply go to this folder, and run `php puzzle1.php`. The result will show in the standard output.

#### What does the code do?

 - Loads the file into a string
 - Trims any leading/trailing whitespaces
 - Split that string into an array of strings, using line returns as delimiter
 - Loop through that array: 
    - Perform the operation (divide by 3, round down, minus 2) on each element (php automagically casts those strings into numbers)
    - Add the result of the operation to the total result
 - Print out the final result to the standard output

 ## Puzzle #2

#### How to run the code?

Same as Puzzle #1, just run `php puzzle2.php` instead.

#### What does the code do?

Same thing as Puzzle #1, but instead of performing the operation directly in the loop, it calls a function `calculateFuel($mass)`.  
This function takes a mass as a parameter, calculates the fuel for that mass, then recursively calls itself for the mass of the fuel it just calculated, until it reaches negative or zero fuel requirement.
