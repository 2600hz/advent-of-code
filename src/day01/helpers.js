import { pipe, parseLines, lineToList, listOfNumbers } from '../utils.js';

// :: String -> [Numbers]
export const normalizeData = pipe(
  parseLines,
  lineToList,
  listOfNumbers
);

// :: Number -> Number
export const solveFuelForMass = a => Math.floor(a / 3) - 2;
