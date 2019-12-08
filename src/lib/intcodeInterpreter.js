import {
  sum,
  multiply,
  objectPropGetter,
  pipe,
  pipePromise,
  promptQuestion,
  unary
} from '../utils.js';

const getOpcode = pipe(
  String,
  a => a.slice(-2),
  Number
);

const getModes = pipe(
  String,
  a => a.length === 1 ? '' : a.slice(0, -2),
  a => a.split('').map(Number).reverse(),
  a => a.length === 0
    ? [0,0,0]
    : a.length === 1
      ? [...a, 0, 0]
      : a.length === 2
        ? [...a, 0]
        : a
);

const getValueForMode = ({intcode, mode, value}) => ({
  0: intcode[value],
  1: value
})[mode];

const getParameters = ({intcode, pointer, modes}) => modes.map((mode, index) => getValueForMode({
  intcode,
  mode,
  value: intcode[pointer + (index + 1)]
}));

const getStore = ({intcode, pointer, opcode}) => ({
  1: intcode[pointer + 3],
  2: intcode[pointer + 3],
  3: intcode[pointer + 1]
})[opcode];

const getAnswer = pipePromise(
  promptQuestion,
  Number
);

const operationsByOpcode = { 1: sum, 2: multiply, 3: getAnswer, 4: unary(console.log) };

const getOperationByOpcode = objectPropGetter(operationsByOpcode);

const getIncrementByOpcode = objectPropGetter({ 1: 4, 2: 4, 3: 2, 4: 2 });

const resolveParametersForOpcode = ({opcode, parameters}) => ({
  1: parameters,
  2: parameters,
  3: ['Enter input > '],
  4: parameters
})[opcode];

const performOperation = ({opcode, parameters}) => new Promise(async (resolve, reject) =>
  typeof operationsByOpcode[opcode] === 'function'
    ? resolve(getOperationByOpcode(opcode)(...resolveParametersForOpcode({ opcode, parameters })))
    : reject(new Error(`Unknown opcode ${opcode} `)));

const parseInstruction = ({intcode, pointer}) => {
  const lead = intcode[pointer];
  const opcode = getOpcode(lead);
  const modes = getModes(lead);
  const parameters = getParameters({ intcode, pointer, modes });
  const store = getStore({ intcode, pointer, opcode });
  return { opcode, parameters, store };
};

// :: [Number] -> Promise([Number])
export default async originalIntcode => {
  const intcode = [...originalIntcode];
  let pointer = 0;

  do {
    const {opcode, parameters, store} = parseInstruction({ intcode, pointer });
    const result = await performOperation({ opcode, parameters });
    if (Number.isInteger(result)) intcode[store] = result;
    pointer += getIncrementByOpcode(opcode);
  } while (getOpcode(intcode[pointer]) !== 99);

  return intcode;
};
