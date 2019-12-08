import { pipe, pipePromise, lineToList, listOfNumbers, getDataFromInput } from '../utils.js';

const normalizeData = pipe(
  lineToList,
  listOfNumbers
);

export const getIntcode = pipePromise(
  getDataFromInput,
  normalizeData
);
