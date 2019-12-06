import {
  pipePromise,
  lineToList,
  listOfNumbers,
  sum,
  multiply,
  objectPropGetter
} from '../utils.js';

// :: String -> [Number]
export const normalizeData = pipePromise(
  lineToList,
  listOfNumbers
);

// :: a -> ([b] -> [b])
export const intcodeToStateRestorer = a => b => b.map((c, d) => a[d] || c);

// :: a => a
const extractInstructionParameters = ({intcode, index}) => ({
  intcode,
  index,
  first: intcode[index + 1],
  second: intcode[index + 2],
  third: intcode[index + 3]
});

// :: a => a
const resolveInstructionValues = ({intcode, index, first, second, third}) => ({
  opcode: intcode[index],
  left: intcode[first],
  right: intcode[second],
  store: third
});

// :: Object => b
const getOperationByOpcode = objectPropGetter({ 1: sum, 2: multiply });

// :: a => a
const performOperation = ({opcode, left, right}) => getOperationByOpcode(opcode)(left, right);

// :: a => a
const processOpcode = ({store, ...rest}) => ({ store, result: performOperation(rest) });

// :: a => Promise(a)
const computeInstruction = pipePromise(
  extractInstructionParameters,
  resolveInstructionValues,
  processOpcode
);

// :: [Number] -> Promise([Number])
export const interpretIntcode = intcode => new Promise(async (resolve, reject) => {
  const newIntcode = [...intcode];
  let i = 0;
  let store;
  let result;

  while (newIntcode[i] !== 99) {
    try {
      ({store, result} = await computeInstruction({ intcode: newIntcode, index: i }));
    } catch (e) {
      return reject(new Error(`Unknown opcode ${newIntcode[i]} at index ${i}`));
    }
    newIntcode.splice(store, 1, result);
    i += 4;
  }

  return resolve(newIntcode);
});
