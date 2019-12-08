import { sum, multiply, pipe, pipePromise, promptQuestion, unary } from '../utils.js';

const promptForInput = pipePromise(
  promptQuestion,
  Number
);

const parseOpcode = pipe(
  String,
  a => a.slice(-2),
  Number
);

// :: Number -> Number
const getOpcode = ({lead, spec}) => new Promise((resolve, reject) => {
  const opcode = parseOpcode(lead);
  return spec[opcode] ? resolve(opcode) : reject(new Error(`Unknown opcode ${opcode}`));
});

const getStoreByOpcode = ({intcode, pointer, opcode, spec}) => {
  const {store} = spec[opcode];
  return store ? intcode[sum(pointer, store)] : undefined;
};

const getOperationByOpcode = ({opcode, spec}) => {
  const {operation} = spec[opcode];
  return operation || (() => undefined);
};

const getNextPointerByOpcode = ({opcode, pointer, parameters, spec}) => {
  const {inc} = spec[opcode];
  if (typeof inc === 'number') return sum(pointer, inc);
  if (typeof inc === 'function') return inc(pointer, ...parameters);
  return pointer;
};

// :: Number -> [Number]
const getModes = pipe(
  String,
  s => s.slice(0, -2),
  s => [...s].reverse().join(''),
  s => s.padEnd(3, '0'),
  s => [...s].map(Number)
);

const getValueForMode = ({intcode, mode, current}) => ({
  0: intcode[current],
  1: current
})[mode];

// :: Object -> [Number]
const getParameters = ({intcode, pointer, modes}) => modes.map((mode, index) => getValueForMode({
  intcode,
  mode,
  current: intcode[sum(pointer, sum(index, 1))]
}));

const performOperation = ({opcode, parameters, spec}) => new Promise(async (resolve, reject) => {
  const operation = getOperationByOpcode({ opcode, spec });
  let result;
  try {
    result = await operation(...parameters);
  } catch (e) {
    return reject(e);
  }
  return resolve(result);
});

const parseInstruction = async ({intcode, pointer, spec}) => {
  const lead = intcode[pointer];
  const opcode = await getOpcode({ lead, spec });
  const modes = getModes(lead);
  const parameters = getParameters({ intcode, pointer, modes });
  const store = getStoreByOpcode({ intcode, pointer, opcode, spec });
  const nextPointer = getNextPointerByOpcode({ opcode, pointer, parameters, spec });
  return { opcode, parameters, store, nextPointer };
};

// :: [Number] -> Promise([Number])
export default async originalIntcode => {
  const spec = {
    1: {
      store: 3,
      operation: sum,
      inc: 4
    },
    2: {
      store: 3,
      operation: multiply,
      inc: 4
    },
    3: {
      store: 1,
      operation: async () => await promptForInput('Enter input > '),
      inc: 2
    },
    4: {
      operation: unary(console.log),
      inc: 2
    },
    5: {
      inc: (pointer, first, second) => first !== 0 ? second : sum(pointer, 3)
    },
    6: {
      inc: (pointer, first, second) => first === 0 ? second : sum(pointer, 3)
    },
    7: {
      store: 3,
      operation: (first, second) => first < second ? 1 : 0,
      inc: 4
    },
    8: {
      store: 3,
      operation: (first, second) => first === second ? 1 : 0,
      inc: 4
    },
    99: {
      operation: () => null
    }
  };
  const intcode = [...originalIntcode];
  let pointer = 0;
  let result;

  do {
    const {
      opcode,
      parameters,
      store,
      nextPointer
    } = await parseInstruction({ intcode, pointer, spec });
    result = await performOperation({ opcode, parameters, spec });
    if (Number.isInteger(result) && Number.isInteger(store)) intcode[store] = result;
    pointer = nextPointer;
  } while (result !== null);

  return intcode;
};
