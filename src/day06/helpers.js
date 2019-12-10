import { pipePromise, pipe , getDataFromInput } from '../utils.js';

// :: String -> Object
const normalizeData = pipe(
  s => s.split(/\n/),
  a => a.map(s => s.split(')')),
  a => a.reduce((map, [orbitee, orbiter]) => {
    map[orbiter] = orbitee;
    return map;
  }, {})
);

export const getMapData = pipePromise(
  getDataFromInput,
  normalizeData
);

// :: Object -> [String]
export const getOrbitees = ({map, object}) => {
  const orbitee = map[object];
  return [
    ...(orbitee ? getOrbitees({ map, object: orbitee }) : []),
    ...(orbitee ? [orbitee] : [])
  ];
};
