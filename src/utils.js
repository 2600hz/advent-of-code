import { readFile } from 'fs';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import path from 'path';

// :: (String, String | Object) -> Promise
const readFilePromise = (a, b = 'utf-8') => new Promise((resolve, reject) =>
  readFile(a, b, (err, data) =>
    err ? reject(err) : resolve(data)));

// :: (String, String | Object) -> Promise
export const getDataFromFile = async (a, b) => await readFilePromise(a, b);

// :: (...Function) -> (a -> b)
export const pipe = (...a) => b => a.reduce((c, d) => c.then(d), Promise.resolve(b));

// :: String -> (String -> String)
const buildPathToFileName = a => b => path.join(b, a);

// :: URL | String -> String
const resolvePathToDir = pipe(
  fileURLToPath,
  dirname
);

// :: String -> String
const buildPathToInput = buildPathToFileName('input.txt');

// :: URL | String -> String
export const resolvePathToInputFile = pipe(
  resolvePathToDir,
  buildPathToInput
);

export const getDataFromInput = pipe(
  resolvePathToInputFile,
  getDataFromFile
);

// :: String -> String
export const parseLines = a => a.replace(/\n/gm, ',')

// :: String -> String
export const lineToList = a => a.split(',');

// :: [a] -> [Number]
export const listOfNumbers = a => a.map(Number);

// :: (Number, Number) -> Number
export const sum = (a, b) => a + b;

// :: [Number] -> Number
export const sumUp = a => a.reduce(sum);

// :: (Number, Number) -> Number
export const multiply = (a, b) => a * b;

// :: [a] -> a
export const head = a => a[0];

// :: Object -> (a -> b)
export const objectPropGetter = a => b => ({...a})[b];
