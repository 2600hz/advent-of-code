import { pipe, pipePromise, getDataFromInput, primitiveToArray, listOfNumbers } from '../utils.js';

// :: String -> Promise([Number])
export const getRange = pipePromise(
  getDataFromInput,
  data => data.split('-').map(Number)
);

// :: Function -> (a -> b)
export const criteriaMatcher = a => pipe(primitiveToArray, listOfNumbers, a);
