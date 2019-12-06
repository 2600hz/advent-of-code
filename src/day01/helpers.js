import { pipe, parseLines, listOfNumbers } from '../utils.js';

// :: String -> [Numbers]
export const normalizeData = pipe(
  parseLines,
  listOfNumbers
);

// :: Number -> Number
export const solveFuelForMass = a => Math.floor(a / 3) - 2;
