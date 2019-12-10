import { readFile } from 'fs';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import path from 'path';
import readline from 'readline';

// :: (String, String | Object) -> Promise
const readFilePromise = (a, b = 'utf-8') => new Promise((resolve, reject) =>
  readFile(a, b, (err, data) =>
    err ? reject(err) : resolve(data)));

// :: (String, String | Object) -> Promise
export const getDataFromFile = async (a, b) => await readFilePromise(a, b);

// :: (...Function) -> (a -> b)
export const pipe = (...a) => b => a.reduce((c, d) => d(c), b);

// :: (...Function) -> (a -> Promise(b))
export const pipePromise = (...a) => b => a.reduce((c, d) => c.then(d), Promise.resolve(b));

// :: String -> (String -> String)
const buildPathToFileName = a => b => path.join(b, a);

// :: URL | String -> String
const resolvePathToDir = pipePromise(
  fileURLToPath,
  dirname
);

// :: String -> String
const buildPathToInput = buildPathToFileName('input.txt');

// :: URL | String -> String
export const resolvePathToInputFile = pipePromise(
  resolvePathToDir,
  buildPathToInput
);

export const getDataFromInput = pipePromise(
  resolvePathToInputFile,
  getDataFromFile
);

// :: String -> [String]
export const parseLines = a => a.split(/\n/gm);

// :: String -> [String]
export const lineToList = a => a.split(',');

// :: [a] -> [Number]
export const listOfNumbers = a => a.map(Number);

// :: (Number, Number) -> Number
export const sum = (a, b) => a + b;

// :: [Number] -> Number
export const sumUp = (...a) => a.reduce(sum);

// :: (Number, Number) -> Number
export const multiply = (a, b) => a * b;

// :: [a] -> a
export const head = ([a]) => a;

// :: Object -> (a -> b)
export const objectPropGetter = a => b => ({...a})[b];

// :: ([Number], [Number]) -> Number
export const manathanDistance = ([a, b], [c, d]) => Math.abs(a - c) + Math.abs(b - d);

// :: Array -> Number
export const length = a => a.length;

// :: a -> [String]
export const primitiveToArray = a => [...('' + a)];

// :: ...Function -> (a -> Boolean)
export const strictFunctionsEnforcer = (...a) => b => a.every(c => c(b));

// :: ...a -> Boolean
export const primitiveMatching = (...a) => a.every((b, c, [d]) => b === d);

// :: readline.Interface => (String -> Promise(String))
const questionPrompter = rlInterface =>  prompt => new Promise((resolve, reject) => {
  try {
    rlInterface.question(prompt, answer => {
      rlInterface.close();
      resolve(answer);
    });
  } catch (e) {
    reject(e);
  }
});

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

// :: String -> Promise(String)
export const promptQuestion = questionPrompter(rl);

export const unary = a => b => a(b);
