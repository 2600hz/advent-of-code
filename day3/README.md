# Advent of Code Day 3

#### How to run the code?

You need to have php installed. Then simply go to this folder, and run `php puzzle1.php` for the Puzzle #1 or `php puzzle2.php` for Puzzle #2. The result will show in the standard output.

#### Puzzle #1

Alright, my implementation for this one feels a bit dirty...  

- I first load the file as a string, split it in an array of two lines (using line breaks) and assign those two lines to the `$wire1` and `$wire2` variables.  
- Next, for each of those wires, I walk the path and store each individual location the wire goes in an array. That location is saved as a string representing the coordinate using the start of the wires as the point of origin, with the format `"x,y"`. I was originally planning on storing it as an array of two integer instead `[x,y]`, however I could not get the php function `array_uintersect` working on arrays of arrays, so I went with `array_intersect` which works on arrays of strings.
- Once I have all the locations of both wires, I use `array_intersect` to only get the locations present in both, and use `array_unique` on the result to remove duplicated.
- Then loop through all the common locations, calculate the distance by adding the absolute values of `x` and `y`, and return the shortest distance.

#### Puzzle #2

The process for this is very similar to Puzzle #1, the main differences are:  

- Instead of storing the locations as `"x,y"` strings, they are stored as an associative array `["x,y" => n]` where `n` is the total number of steps walked to reach this location. In order to only store each location reached with the fewest steps (in case the wire crosses itself), the location is only added to the array of locations if the key `x,y` isn't already present.
- Then, the function `array_intersect` is used on the _keys_ of both wire location arrays, instead of the arrays themselves.
- Finally, instead of adding up the absolute values of the location coordinates, we add up the total of steps walked (`n`) for both wires for each common location, and return the smallest number.