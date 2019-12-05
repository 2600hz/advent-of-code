import { pipe, lineToList, listOfNumbers } from '../utils.js';

// :: String -> [Number]
export const normalizeData = pipe(
  lineToList,
  listOfNumbers
);

// :: a -> ([b] -> [b])
export const intcodeToStateRestorer = a => b => b.map((c, d) => a[d] || c);
