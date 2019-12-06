import {
  pipe,
  pipePromise,
  getDataFromInput,
  length,
  primitiveToArray,
  strictFunctionsEnforcer,
  listOfNumbers
} from '../utils.js';

/**
 * --- Day 4: Secure Container ---
 * You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had
 * written the password on a sticky note, but someone threw it out.
 *
 * However, they do remember a few key facts about the password:
 *
 * - It is a six-digit number.
 * - The value is within the range given in your puzzle input.
 * - Two adjacent digits are the same (like 22 in 122345).
 * - Going from left to right, the digits never decrease; they only ever increase or stay the same
 *   (like 111123 or 135679).
 *
 * Other than the range rule, the following are true:
 *
 * - 111111 meets these criteria (double 11, never decreases).
 * - 223450 does not meet these criteria (decreasing pair of digits 50).
 * - 123789 does not meet these criteria (no double).
 *
 * How many different passwords within the range given in your puzzle input meet these criteria?
 */

// :: String -> Promise([Number])
const getRange = pipePromise(
  getDataFromInput,
  data => data.split('-').map(Number)
);

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

// :: [Number] -> Boolean
const hasTwoIdenticallyAdjascentDigits = digits => {
  let isValid = true;
  while (isValid) {
    const [a, b, ...rest] = digits;
    isValid = digits.length > 1;
    if (a === b) break;
    digits = [b, ...rest];
  }
  return isValid;
};

const enforceCriteria = strictFunctionsEnforcer(
  noDecreasingDigit,
  hasTwoIdenticallyAdjascentDigits
);

// :: Number -> Boolean
const meetsCriteria = pipe(
  primitiveToArray,
  listOfNumbers,
  enforceCriteria
);

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
