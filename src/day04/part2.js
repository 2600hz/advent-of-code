import {
  pipePromise,
  length,
  strictFunctionsEnforcer,
  primitiveMatching
} from '../utils.js';
import { getRange, criteriaMatcher } from './helpers.js';

/**
 * --- Part Two ---
 * An Elf just remembered one more important detail: the two adjacent matching digits are not part
 * of a larger group of matching digits.
 *
 * Given this additional criterion, but still ignoring the range rule, the following are now true:
 *
 * - 112233 meets these criteria because the digits never decrease and all repeated digits are
 *   exactly two digits long.
 * - 123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
 * - 111122 meets the criteria (even though 1 is repeated more than twice, it still contains a
 *   double 22).
 *
 * How many different passwords within the range given in your puzzle input meet all of the
 * criteria?
 */

// :: [Number] -> Boolean
const noDecreasingDigit = digits => {
  let isValid = true;
  while (isValid) {
    const [a, b, ...rest] = digits;
    if (digits.length < 2) break;
    isValid = !(a > b);
    if (!isValid) break;
    digits = [b, ...rest];
  }
  return isValid;
};

const matches = primitiveMatching;

// :: [Number] -> Boolean
const hasTwoIdenticallyAdjascentNonGroupedDigits = digits => {
  let isValid = false;
  while (!isValid) {
    const [a, ...rest] = digits;
    const [b, c, d, e, f] = rest;
    // no adjascent
    if (
      matches(a,b,c,d,e,f)
      || matches(a,b,c,d,e)
      || matches(b,c,d,e,f)
      || matches(b,c,d,e)
    ) break;
    // [a,b] | [e,f] adjascent
    if (
      matches(a,b,c,d) && matches(e,f)
      || matches(a,b) && matches(c,d,e,f)
    ) isValid = true;
    // possible [d,e] | [e,f] adjascent
    if (matches(a,b,c)) {
      digits = [d, e, f];
      continue;
    }

    if (digits.length < 2) break;
    if (matches(a,b)) isValid = true;
    digits = [...rest];
  }
  return isValid;
};

const enforceCriteria = strictFunctionsEnforcer(
  noDecreasingDigit,
  hasTwoIdenticallyAdjascentNonGroupedDigits
);

const meetsCriteria = criteriaMatcher(enforceCriteria);

// :: [Number] -> [Number]
const generatePasswordsWithinRange = ([candidate, limit]) => {
  const passwords = [];
  while (candidate <= limit) {
    if (meetsCriteria(candidate)) passwords.push(candidate);
    candidate += 1;
  }
  return passwords;
};

// :: String -> Promise(Number)
const secureContainer = pipePromise(
  getRange,
  generatePasswordsWithinRange,
  length,
  console.log
);

secureContainer(import.meta.url).catch(console.log);
